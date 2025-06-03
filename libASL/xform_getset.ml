(****************************************************************
 * ASL getters and setters elimination transform
 *
 * It also:
 * - replaces assignment to LExpr_Write with procedure call
 *
 * - replaces LExpr_ReadWrite by LExpr_Var and wraps the
 *   assignment statement containing the l-expr with two
 *   statements:
 *     (1) function call which reads the initial value into
 *         the variable,
 *     (2) procedure call which writes the modified value back.
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Identset

let getFunReturnType (d : AST.declaration) : AST.ty =
  ( match d with
  | Decl_FunDefn (f, fty, body, loc) -> fty.rty
  | _ -> raise (Utils.InternalError (Loc.Unknown, "function definition expected", (fun fmt -> Asl_fmt.declaration fmt d), __LOC__))
  )

let rmwVariables = new Asl_utils.nameSupply "__rmw"

class replaceClass (ds : AST.declaration list) =
  object (self)
    inherit Asl_visitor.nopAslVisitor
    val mutable le_vars : (AST.lexpr * Ident.t) list = []

    val decl_lookup_table =
      ds
      |> List.to_seq
      |> Seq.filter_map monomorphizable_decl_to_ident_and_decl
      |> IdentTable.of_seq

    method! vlexpr e =
      match e with
      | LExpr_ReadWrite (f, g, tes, es, throws) ->
          let v = rmwVariables#fresh in
          le_vars <- (e, v) :: le_vars;
          ChangeTo (AST.LExpr_Var v)
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (AST.LExpr_Write (f, tes, es, throws), r, loc) ->
          let s = AST.Stmt_TCall (f, tes, es @ [ r ], throws, loc) in
          Visitor.ChangeTo [ s ]
      | Stmt_Assign (_, _, loc) ->
          let post_action (ss : AST.stmt list) : AST.stmt list =
            let wrap_stmts (ss : AST.stmt list) = function
              | AST.LExpr_ReadWrite (f, g, tes, es, throws), v ->
                  let e = AST.Expr_TApply (f, tes, es, throws) in
                  let fd = Option.get (IdentTable.find_opt decl_lookup_table f) in
                  let rty = getFunReturnType fd in
                  let r = AST.Stmt_VarDecl (AST.DeclItem_Var (v, Some rty), e, loc) in
                  let w = AST.Stmt_TCall (g, tes, es @ [ Expr_Var v ], throws, loc) in
                  [ r ] @ ss @ [ w ]
              | _ -> ss
            in
            let ss' = List.fold_left wrap_stmts ss le_vars in
            le_vars <- [];
            ss'
          in
          ChangeDoChildrenPost ([ s ], post_action)
      | _ -> DoChildren
  end

let replace (cl : replaceClass) (ss : AST.stmt list) : AST.stmt list =
  Asl_visitor.visit_stmts (cl :> Asl_visitor.aslVisitor) ss

let xform_funtype (fty : AST.function_type) : AST.function_type =
  let args' =
    ( match fty.setter_arg with
    | Some (nm, ty) -> fty.args @ [(nm, ty, None)]
    | None -> fty.args
    )
  in
  { fty with args = args'; setter_arg=None; use_array_syntax=false; is_getter_setter=false }

let xform_decl (replacer : replaceClass) (d : AST.declaration) :
    AST.declaration list =
  match d with
  | Decl_FunType (f, fty, loc) ->
      let fty' = xform_funtype fty in
      let d' = AST.Decl_FunType (f, fty', loc) in
      [ d' ]
  | Decl_FunDefn (f, fty, body, loc) ->
      let fty' = xform_funtype fty in
      let body' = replace replacer body in
      let d' = AST.Decl_FunDefn (f, fty', body', loc) in
      [ d' ]
  | _ -> [ d ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceClass ds in
  List.flatten (List.map (xform_decl replacer) ds)

(****************************************************************
 * Command: :xform_getset
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_getset" [] [] [] "Introduce calls to getter/setter functions" cmd

(****************************************************************
 * End
 ****************************************************************)
