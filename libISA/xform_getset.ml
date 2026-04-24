(****************************************************************
 * Assignment-form elimination transform
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
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
module FMT = Isa_fmt
open Isa_utils
open Identset

let getFunReturnType (ds : AST.declaration list) : AST.ty =
  let rty = List.find_map
    (function
     | AST.Decl_FunDefn (f, fty, body, loc) -> Some fty.rty
     | _ -> None
    ) ds
  in
  match rty with
  | Some rty -> rty
  | None -> raise (Utils.InternalError
      (Loc.Unknown, "function definition expected", (fun fmt -> FMT.declarations fmt ds), __LOC__))

let rmwVariables = new Isa_utils.nameSupply "__rmw"

class replaceClass (ds : AST.declaration list) =
  object (self)
    inherit Isa_visitor.nopIsaVisitor
    val mutable le_vars : (AST.lexpr * Ident.t) list = []

    val decl_lookup_table =
      let table : AST.declaration list IdentTable.t = IdentTable.create 16 in
      ds
      |> List.filter_map monomorphizable_fun_decl_to_ident_and_decl
      |> List.iter (fun (f, d) ->
           let updated =
             match IdentTable.find_opt table f with
             | Some ds -> d :: ds
             | None -> [ d ]
           in
           IdentTable.replace table f (List.rev updated)
         );
      table

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
                  let fds = Option.get (IdentTable.find_opt decl_lookup_table f) in
                  let rty = getFunReturnType fds in
                  let r = AST.Stmt_VarDecl (false, AST.DeclItem_Var (v, Some rty), e, loc) in
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
  Isa_visitor.visit_stmts (cl :> Isa_visitor.isaVisitor) ss

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
  List.concat (List.map (xform_decl replacer) ds)

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
