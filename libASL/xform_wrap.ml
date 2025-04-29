(****************************************************************
 * ASL global variable wrapping transform
 *
 * It supplements global variables with functions for reading and
 * writing the variables, and replaces use of the variables by
 * calls to the functions.
 *
 * For variable x the names of created functions are x_read and
 * x_write.
 *
 * Because the variables typically represent processor or memory
 * state, a simulator can redefine the functions to provide access
 * to its own simulation state instead.
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Identset

let mk_read_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"read"

let mk_write_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"write"

class replaceClass (ds : AST.declaration list) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    (* Set of identifiers of variables to be wrapped *)
    val decl_var_idents : IdentSet.t =
      ds
      |> List.filter_map (function
           | AST.Decl_Var (v, ty, loc) -> Some v
           | _ -> None)
      |> IdentSet.of_list

    method! vexpr e =
      match e with
      | Expr_Array (Expr_Var v, i) when IdentSet.mem v decl_var_idents ->
          let f = mk_read_fident v in
          let e' = AST.Expr_TApply (f, [], [ i ], NoThrow) in
          ChangeDoChildrenPost (e', Fun.id)
      | Expr_Var v when IdentSet.mem v decl_var_idents ->
          let f = mk_read_fident v in
          ChangeTo (AST.Expr_TApply (f, [], [], NoThrow))
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (LExpr_Array (LExpr_Var v, i), e, loc) when IdentSet.mem v decl_var_idents ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ i ; e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | Stmt_Assign (LExpr_Var v, e, loc) when IdentSet.mem v decl_var_idents ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | _ -> DoChildren

  end

let xform_decl (replacer : replaceClass) (d : AST.declaration) :
    AST.declaration list =
  let d' = Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor) d in
  match d' with
  | Decl_Var (v, Type_Array (ixty, ty), loc) ->
      let i = Ident.mk_ident "i" in
      let rd_f = mk_read_fident v in
      let rd_fty : AST.function_type = {
        parameters=[];
        args=[(i, ixtype_basetype ixty, None)];
        setter_arg=None;
        rty=Some ty;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow
      } in
      let rd_type = AST.Decl_FunType (rd_f, rd_fty, loc) in
      let rd_body = [ AST.Stmt_FunReturn (Expr_Array (Expr_Var v, Expr_Var i), loc) ] in
      let rd_defn = AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc) in

      let wr_f = mk_write_fident v in
      let vl = Ident.mk_ident "v" in
      let wr_fty : AST.function_type = {
        parameters=[];
        args=[(i, ixtype_basetype ixty, None); (vl, ty, None)];
        rty=None;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow
      } in
      let wr_type = AST.Decl_FunType (wr_f, wr_fty, loc) in
      let wr_body = [ AST.Stmt_Assign (LExpr_Array (LExpr_Var v, Expr_Var i), Expr_Var vl, loc) ] in
      let wr_defn = AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc) in

      [ d' ; rd_type ; wr_type ; rd_defn ; wr_defn ]
  | Decl_Var (v, ty, loc) ->
      let rd_f = mk_read_fident v in
      let rd_fty : AST.function_type = {
        parameters=[];
        args=[];
        rty=Some ty;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow
      } in
      let rd_type = AST.Decl_FunType (rd_f, rd_fty, loc) in
      let rd_body = [ AST.Stmt_FunReturn (Expr_Var v, loc) ] in
      let rd_defn = AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc) in

      let wr_f = mk_write_fident v in
      let vl = Ident.mk_ident "v" in
      let wr_fty : AST.function_type = {
        parameters=[];
        args=[(vl, ty, None)];
        rty=None;
        setter_arg=None;
        use_array_syntax=false;
        is_getter_setter=false;
        throws=NoThrow
      } in
      let wr_type = AST.Decl_FunType (wr_f, wr_fty, loc) in
      let wr_body = [ AST.Stmt_Assign (LExpr_Var v, Expr_Var vl, loc) ] in
      let wr_defn = AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc) in

      [ d' ; rd_type ; wr_type ; rd_defn ; wr_defn ]
  | _ -> [ d' ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceClass ds in
  ds
  |> List.map (xform_decl replacer)
  |> List.flatten
  (* Re-hoist function prototypes because the transformation adds new ones *)
  |> hoist_prototypes

(****************************************************************
 * Command: :xform_wrap
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_wrap" [] [] [] "Wrap global variables into functions" cmd

(****************************************************************
 * End
 ****************************************************************)
