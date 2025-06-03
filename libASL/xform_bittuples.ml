(****************************************************************
 * ASL bittuple transforms
 *
 * This simplifies assignments and expressions involving multiple slices
 *
 * 1) 'bittuple' L-expression assignments such as
 *
 *        var x :: bits(8);
 *        var y :: bits(8);
 *        var z :: bits(16);
 *        ...
 *        [x,y,z] = e;
 *    ==>
 *        let tmp = e;
 *        x = tmp[24 +: 8];
 *        y = tmp[16 +: 8];
 *        z = tmp[0 +: 16];
 *
 *    Note that, in the general case, x, y and z can be any L-expressions.
 *
 * 2) multiple slice L-expression assignments such as
 *
 *        x[0 +: 8, 8 +: 8, 16 +: 8, 24 +: 8] = e; // byte-reverse
 *    ==>
 *        [x[0 +: 8], x[8 +: 8], x[16 +: 8], x[24 +: 8]] = e; // byte-reverse
 *    ==>
 *        let tmp = e;
 *        x[0 +: 8] = tmp[24 +: 8];
 *        x[8 +: 8] = tmp[16 +: 8];
 *        x[16 +: 8] = tmp[8 +: 8];
 *        x[24 +: 8] = tmp[0 +: 8];
 *
 * 3) multiple slice R-expressions such as
 *
 *        ... = e[0 +: 8, 8 +: 8];
 *    ==>
 *        ... = __let tmp = e __in [tmp[0 +: 8], tmp[8 +: 8]];
 *
 * 4) 'bittuple' variable initializers such as
 *
 *        let [hi : bits(32), lo : bits(32)] = e;
 *    ==>
 *        let tmp = e;
 *        let hi = tmp[32 +: 32];
 *        let lo = tmp[0  +: 32];
 *
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_ast

let assign_var = new Asl_utils.nameSupply "__a"

(* Transform '[l1, ... ln] = r;' where, for each 'i', 'li :: bits(wi) *)
let xform
    (loc : Loc.t)
    (ws : AST.expr list)
    (ls : AST.lexpr list)
    (r : AST.expr)
    : AST.stmt list =
    let tmp_ident = assign_var#fresh in
    let total_width = Xform_simplify_expr.mk_add_ints ws in
    let ty = Asl_utils.type_bits total_width in

    let (ss, _) = List.fold_right2 (fun l w (ss, idx) ->
      let slice = Slice_LoWd (idx, w) in
      let r' = Expr_Slices (ty, Expr_Var tmp_ident, [slice]) in
      (Stmt_Assign (l, r', loc) :: ss, Asl_utils.mk_add_int idx w)
    ) ls ws ([], Asl_utils.zero) in

    let tmp_var = DeclItem_Var (tmp_ident, Some ty) in
    let tmp_const_decl = Stmt_VarDecl (true, tmp_var, r, loc) in
    tmp_const_decl :: ss

(* Transform 'let/var [v1, ... vn] = r;' *)
let xform_dbi
    (loc : Loc.t)
    (is_constant : bool)
    (dbs : (Ident.t option * AST.ty) list)
    (r : AST.expr)
    : AST.stmt list =
    let tmp_ident = assign_var#fresh in
    let widths = List.map (fun (_, ty) -> Option.get (Asl_utils.width_of_type ty)) dbs in
    let total_width = Xform_simplify_expr.mk_add_ints widths in
    let tmp_ty = Asl_utils.type_bits total_width in
    let tmp_var = DeclItem_Var (tmp_ident, Some tmp_ty) in
    let tmp_const_decl = Stmt_VarDecl (true, tmp_var, r, loc) in

    let (ss, _) = List.fold_right (fun (ov, ty) (ss, idx) ->
      let w = Option.get (Asl_utils.width_of_type ty) in
      let idx' = Xform_simplify_expr.mk_add_int idx w in
      ( match ov with
      | Some v ->
          let slice = Slice_LoWd (idx, w) in
          let r' = Expr_Slices (tmp_ty, Expr_Var tmp_ident, [slice]) in
          let di = DeclItem_Var (v, Some ty) in
          let s = Stmt_VarDecl (is_constant, di, r', loc) in
          (s :: ss, idx')
      | None ->
          (ss, idx')
      )
    ) dbs ([], Asl_utils.zero) in

    tmp_const_decl :: ss

let slice_width (x : AST.slice) : AST.expr =
  ( match x with
  | Slice_Single e -> Asl_utils.one
  | Slice_HiLo (hi, lo) -> Xform_simplify_expr.mk_add_int (Asl_utils.mk_sub_int hi lo) Asl_utils.one
  | Slice_LoWd (lo, wd) -> wd
  | Slice_HiWd (hi, wd) -> wd
  | Slice_Element (lo, wd) -> wd
  )

class replace_bittuples (ds : AST.declaration list option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_Slices (ty, e, ss) when List.length ss > 1 -> (* todo: only if e is atomic *)
        let ws = List.map slice_width ss in
        let x' = Asl_utils.mk_expr_safe_to_replicate assign_var e ty (fun e' ->
            let es = List.map (fun s -> Expr_Slices (ty, e', [s])) ss in
            Expr_Concat (ws, es))
        in
        Visitor.ChangeTo x'
      | _ -> DoChildren
      )

    method! vstmt s =
      match s with
      | Stmt_VarDecl (is_constant, DeclItem_BitTuple dbs, r, loc) ->
          Visitor.ChangeTo (xform_dbi loc is_constant dbs r)
      | Stmt_Assign (LExpr_BitTuple (ws, ls), r, loc) ->
          Visitor.ChangeTo (xform loc ws ls r)
      | Stmt_Assign (LExpr_Slices (ty, l, ss), r, loc) when List.length ss > 1 ->
          let (ws, ls) =
            List.map (fun s -> (slice_width s, LExpr_Slices (ty, l, [s]))) ss
            |> List.split
          in
          Visitor.ChangeTo (xform loc ws ls r)
      | _ -> DoChildren
  end

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let replacer = new replace_bittuples None in
  Asl_visitor.visit_stmts (replacer :> Asl_visitor.aslVisitor) ss

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replace_bittuples (Some ds) in
  List.map (Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_bittuples
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_bittuples" [] [] [] "Transform bittuple" cmd

(****************************************************************
 * End
 ****************************************************************)
