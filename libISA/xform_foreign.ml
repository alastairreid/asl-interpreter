(****************************************************************
 * ISA foreign import and export transform
 *
 * Processes foreign variable declarations in the ISA
 * specification.
 *
 * For foreign import var declaration, replaces all uses of
 * the variable in expressions and assignments with calls to
 * generated functions to read/write the variable from/to C.
 * These functions are declared as imported using foreign import
 * function. Declaration of the variable itself is removed as no
 * longer needed.

 * For foreign export var declaration, generates a pair of
 * functions to read/write the variable from/to ISA and declares
 * them as exported using foreign export function.
 *
 * E.g. for variable x the names of generated functions are
 * x_Read.0 and x_Write.0 which are imported/exported from/to C
 * with x_Read and x_Write names.
 *
 * Copyright (C) 2025-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Identset
open Isa_utils

let mk_rd_fty (ty : AST.ty) : AST.function_type =
  {
    parameters = [];
    args = [];
    setter_arg = None;
    rty = ty;
    use_array_syntax = false;
    is_getter_setter = false;
    throws = NoThrow;
    is_builtin = false;
  }

let mk_array_rd_fty (i : Ident.t) (ity : AST.ixtype) (ty : AST.ty) :
    AST.function_type =
  {
    parameters = [];
    args = [ (i, ixtype_basetype ity, None) ];
    setter_arg = None;
    rty = ty;
    use_array_syntax = false;
    is_getter_setter = false;
    throws = NoThrow;
    is_builtin = false;
  }

let mk_wr_fty (vl : Ident.t) (ty : AST.ty) : AST.function_type =
  {
    parameters = [];
    args = [ (vl, ty, None) ];
    setter_arg = None;
    rty = type_unit;
    use_array_syntax = false;
    is_getter_setter = false;
    throws = NoThrow;
    is_builtin = false;
  }

let mk_array_wr_fty (i : Ident.t) (ity : AST.ixtype) (vl : Ident.t)
    (ty : AST.ty) : AST.function_type =
  {
    parameters = [];
    args = [ (i, ixtype_basetype ity, None); (vl, ty, None) ];
    setter_arg = None;
    rty = type_unit;
    use_array_syntax = false;
    is_getter_setter = false;
    throws = NoThrow;
    is_builtin = false;
  }

let mk_read_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"Read"

let mk_write_fident (i : Ident.t) : Ident.t =
  Ident.add_suffix (Ident.mk_fident_with_tag i ~tag:0) ~suffix:"Write"

class replacerClass (ds : AST.declaration list) =
  object (self)
    inherit Isa_visitor.nopIsaVisitor

    (* Set of variable identifiers to be imported *)
    val vars_imported : IdentSet.t =
      ds
      |> List.filter_map (function
           | AST.Decl_VarFFI (nm, false, v, loc) -> Some v
           | _ -> None)
      |> IdentSet.of_list

    (* Set of variable identifiers to be exported *)
    val vars_exported : IdentSet.t =
      ds
      |> List.filter_map (function
           | AST.Decl_VarFFI (nm, true, v, loc) -> Some v
           | _ -> None)
      |> IdentSet.of_list

    method is_imported (v : Ident.t) : bool =
      IdentSet.mem v vars_imported

    method is_exported (v : Ident.t) : bool =
      IdentSet.mem v vars_exported

    method! vexpr e =
      match e with
      | Expr_Array (Expr_Var v, i) when self#is_imported v ->
          let f = mk_read_fident v in
          let e' = AST.Expr_TApply (f, [], [ i ], NoThrow) in
          ChangeDoChildrenPost (e', Fun.id)
      | Expr_Var v when self#is_imported v ->
          let f = mk_read_fident v in
          ChangeTo (AST.Expr_TApply (f, [], [], NoThrow))
      | _ -> DoChildren

    method! vstmt s =
      match s with
      | Stmt_Assign (LExpr_Array (LExpr_Var v, i), e, loc) when self#is_imported v ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ i; e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | Stmt_Assign (LExpr_Var v, e, loc) when self#is_imported v ->
          let f = mk_write_fident v in
          let s' = AST.Stmt_TCall (f, [], [ e ], NoThrow, loc) in
          ChangeDoChildrenPost ([ s' ], Fun.id)
      | _ -> DoChildren
  end

let xform_decl (replacer : replacerClass) (d : AST.declaration) :
    AST.declaration list =
  let d' = Isa_visitor.visit_decl (replacer :> Isa_visitor.isaVisitor) d in
  match d' with
  | Decl_Var (v, Type_Array (ixty, ty), loc) when replacer#is_imported v ->
      let i = Ident.mk_ident "i" in
      let rd_f = mk_read_fident v in
      let rd_fty = mk_array_rd_fty i ixty ty in
      let rd_type =
        AST.Decl_FunType (rd_f, rd_fty, loc)
      in
      let vl = Ident.mk_ident "value" in
      let wr_f = mk_write_fident v in
      let wr_fty = mk_array_wr_fty i ixty vl ty in
      let wr_type =
        AST.Decl_FunType (wr_f, wr_fty, loc)
      in
      [ rd_type; wr_type ]
  | Decl_Var (v, ty, loc) when replacer#is_imported v ->
      let rd_f = mk_read_fident v in
      let rd_fty = mk_rd_fty ty in
      let rd_type =
        AST.Decl_FunType (rd_f, rd_fty, loc)
      in
      let vl = Ident.mk_ident "value" in
      let wr_f = mk_write_fident v in
      let wr_fty = mk_wr_fty vl ty in
      let wr_type =
        AST.Decl_FunType (wr_f, wr_fty, loc)
      in
      [ rd_type; wr_type ]
  | Decl_Var (v, Type_Array (ixty, ty), loc) when replacer#is_exported v ->
      let i = Ident.mk_ident "i" in
      let rd_f = mk_read_fident v in
      let rd_fty = mk_array_rd_fty i ixty ty in
      let rd_type =
        AST.Decl_FunType (rd_f, rd_fty, loc)
      in
      let rd_body =
        [ AST.Stmt_Return (Expr_Array (Expr_Var v, Expr_Var i), loc) ]
      in
      let rd_defn =
        AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc)
      in
      let vl = Ident.mk_ident "value" in
      let wr_f = mk_write_fident v in
      let wr_fty = mk_array_wr_fty i ixty vl ty in
      let wr_type =
        AST.Decl_FunType (wr_f, wr_fty, loc)
      in
      let wr_body =
        [ AST.Stmt_Assign (LExpr_Array (LExpr_Var v, Expr_Var i), Expr_Var vl, loc) ]
      in
      let wr_defn =
        AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc)
      in
      [ d'; rd_type; wr_type; rd_defn; wr_defn ]
  | Decl_Var (v, ty, loc) when replacer#is_exported v ->
      let rd_f = mk_read_fident v in
      let rd_fty = mk_rd_fty ty in
      let rd_type =
        AST.Decl_FunType (rd_f, rd_fty, loc)
      in
      let rd_body =
        [ AST.Stmt_Return (Expr_Var v, loc) ]
      in
      let rd_defn =
        AST.Decl_FunDefn (rd_f, rd_fty, rd_body, loc)
      in
      let vl = Ident.mk_ident "value" in
      let wr_f = mk_write_fident v in
      let wr_fty = mk_wr_fty vl ty in
      let wr_type =
        AST.Decl_FunType (wr_f, wr_fty, loc)
      in
      let wr_body =
        [ AST.Stmt_Assign (LExpr_Var v, Expr_Var vl, loc) ]
      in
      let wr_defn =
        AST.Decl_FunDefn (wr_f, wr_fty, wr_body, loc)
      in
      [ d'; rd_type; wr_type; rd_defn; wr_defn ]
  | AST.Decl_VarFFI (nm, is_export, v, loc) ->
      let rd_f = Ident.add_suffix (Ident.mk_fident_with_tag v ~tag:0) ~suffix:"Read" in
      let rd = AST.Decl_FunFFI (nm ^ "_Read", is_export, rd_f, [], loc) in
      let wr_f = Ident.add_suffix (Ident.mk_fident_with_tag v ~tag:0) ~suffix:"Write" in
      let wr = AST.Decl_FunFFI (nm ^ "_Write", is_export, wr_f, [], loc) in
      [ rd ; wr ]
  | _ -> [ d' ]

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replacerClass ds in
  ds
  |> List.map (xform_decl replacer)
  |> List.concat
  (* Re-hoist function prototypes because the transformation adds new ones *)
  |> hoist_prototypes

(****************************************************************
 * Command: :xform_foreign
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_foreign" [] [] []
    "Transform foreign import/export" cmd

(****************************************************************
 * End
 ****************************************************************)
