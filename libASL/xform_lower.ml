(****************************************************************
 * ASL lowering transform
 *
 * Transforms
 * - Slice_HiLo to Slice_LoWd
 * - Slice_Single to Slice_LoWd
 * - Slice_Element to Slice_LoWd
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils

let assign_var = new Asl_utils.nameSupply "__l"

(* Construct nested let-expressions from a list of bindings
 *
 *     mk_lets [(x, tx, ex); (y, ty, ey)] e
 *   =
 *     let x:tx = ex in (let y:ty = ey in e)
 *)
let rec mk_let_exprs (bindings : (Ident.t * AST.ty * AST.expr) list) (e : AST.expr) : AST.expr =
  ( match bindings with
  | [] -> e
  | ((v, ty, e') :: bs) -> AST.Expr_Let(v, ty, e', mk_let_exprs bs e)
  )

class lower_class = object (self)
    inherit Asl_visitor.nopAslVisitor

    (* The main complications in this transformation are
     * 1) Not duplicating the expressions inside slices
     *
     *    We handle this by assigning complex expressions to
     *    new variables (from the assign_var supply).
     *    These assignmens are collected in "pre" and
     *    generate either let-expressions if we are inside
     *    an expression or let-statements if we are inside
     *    a statement.
     *
     * 2) Handling L-expressions of the form l[...][...]
     *
     *    We handle this by lifting "l[...]" out like this
     *
     *      var tmp : ty = l[...];
     *      tmp[...] = ...;
     *      l[...] = tmp;
     *
     *    The variable, type and l[...] are collected in "lifted"
     *)
    val mutable pre : (Ident.t * AST.ty * AST.expr) list = []
    val mutable lifted : (Ident.t * AST.ty * AST.lexpr) list = []

    method mk_intexpr_safe_to_replicate (e : AST.expr) : AST.expr =
      if is_safe_to_replicate e then
        e
      else
        let v = assign_var#fresh in
        pre <- (v, type_integer, e) :: pre;
        Expr_Var v

    (* lower a slice when used as part LExpr_Slices *)
    method! vslice (x : AST.slice) : AST.slice Visitor.visitAction =
      ( match x with
      | Slice_Element (lo, wd) ->
          let wd' = self#mk_intexpr_safe_to_replicate wd in
          let lo' = Xform_simplify_expr.simplify (mk_mul_int lo wd') in
          ChangeDoChildrenPost (AST.Slice_LoWd (lo', wd'), Fun.id)
      | Slice_HiLo (hi, lo) ->
          let lo' = self#mk_intexpr_safe_to_replicate lo in
          let wd = Xform_simplify_expr.mk_add_int (mk_sub_int hi lo') one in
          ChangeDoChildrenPost (AST.Slice_LoWd (lo', wd), Fun.id)
      | Slice_Single i ->
          ChangeDoChildrenPost (AST.Slice_LoWd (i, one), Fun.id)
      | Slice_LoWd _ ->
          DoChildren
      )

    method! vexpr (x : AST.expr) : AST.expr Visitor.visitAction =
      ( match x with
      | Expr_Slices (ty, e, slices) ->
          let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
          let old_pre = pre in (* save previous value of pre *)
          pre <- [];
          let slices' = Visitor.mapNoCopy (Asl_visitor.visit_slice (self :> Asl_visitor.aslVisitor)) slices in
          let r = mk_let_exprs (List.rev pre) (Expr_Slices (ty, e', slices')) in
          pre <- old_pre;
          ChangeDoChildrenPost (r, Fun.id)
      | _ -> DoChildren
      )

    method! vlexpr x =
      let rec is_complex (x : AST.lexpr) : bool =
          ( match x with
          | LExpr_Var _ -> false
          | LExpr_Field (l, _) -> is_complex l
          | _ -> true
          )
      in
      ( match x with
      | LExpr_Slices (ty, l, slices) when is_complex l ->
          (* l[slice] = r; --> var t = l; t[slice] = r; l = t; *)
          let l' = Asl_visitor.visit_lexpr (self :> Asl_visitor.aslVisitor) l in
          let slices' = Visitor.mapNoCopy (Asl_visitor.visit_slice (self :> Asl_visitor.aslVisitor)) slices in

          let tmp = assign_var#fresh in
          lifted <- (tmp, ty, l') :: lifted;
          ChangeTo (LExpr_Slices (ty, LExpr_Var tmp, slices'))
      | _ ->
          DoChildren
      )

    method! vstmt s =
      ( match s with
      | Stmt_Assign (l, r, loc) ->
          assert (Utils.is_empty pre);
          assert (Utils.is_empty lifted);
          let l' = Asl_visitor.visit_lexpr (self :> Asl_visitor.aslVisitor) l in

          (* Lets from any R-expressions that were lifted out *)
          let pre_lets = List.map (fun (v, ty, e) ->
                                      let decl_item = AST.DeclItem_Var (v, Some ty) in
                                      AST.Stmt_ConstDecl (decl_item, e, loc)) pre
          in

          (* Vars from any L-expressions that were lifted out *)
          let pre_vars = List.map (fun (v, ty, l) ->
              let e = match Asl_utils.lexpr_to_expr l with
                      | Some e -> e
                      | None -> raise (Error.Unimplemented (loc, "lower_class", fun fmt -> Asl_fmt.lexpr fmt l))
              in
              let decl_item = AST.DeclItem_Var (v, Some ty) in
              AST.Stmt_VarDecl (decl_item, e, loc)
            )
            (List.rev lifted)
          in
          let post_assigns = List.map (fun (v, ty, l) -> AST.Stmt_Assign (l, Expr_Var v, loc)) lifted in

          pre <- [];
          lifted <- [];
          let r' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) r in
          ChangeTo (pre_lets @ pre_vars @ [AST.Stmt_Assign (l', r', loc)] @ post_assigns)
      | _ -> DoChildren
      )

  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new lower_class in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new lower_class in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new lower_class in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) ss

(****************************************************************
 * Command: :xform_lower
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_lower" [] [] [] "Normalize bitslice operations" cmd

(****************************************************************
 * End
 ****************************************************************)
