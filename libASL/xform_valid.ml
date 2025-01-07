(****************************************************************
 * ASL transform to track valid bits
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Builtin_idents

let mk_call (args : AST.expr list) (loc : Loc.t) : AST.stmt =
  AST.Stmt_TCall (asl_fuzz, [], args, NoThrow, loc)

class tracker (vars : Ident.t list) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt s =
      match s with
      | Stmt_Assign
          ( LExpr_Slices (ty, LExpr_Var v, [ Slice_LoWd (lo, wd) ]),
            Expr_Unknown _,
            loc )
        when is_safe_to_replicate lo && is_safe_to_replicate wd
             && List.mem v vars ->
          let call = mk_call [ mk_litstr (Ident.name v); lo; wd ] loc in
          ChangeTo (s :: [ call ])
      | Stmt_Assign
          ( LExpr_Var v,
            Expr_Unknown (Type_Bits (wd, _)),
            loc)
        when is_safe_to_replicate wd && List.mem v vars ->
          let call =
            mk_call [ mk_litstr (Ident.name v); mk_litint 0; wd ] loc
          in
          ChangeTo (s :: [ call ])
      | _ -> DoChildren
  end

let xform_stmts (vars : Ident.t list) (ss : AST.stmt list) : AST.stmt list =
  let xform = new tracker vars in
  Asl_visitor.visit_stmts (xform :> Asl_visitor.aslVisitor) ss

let xform_decls (vars : Ident.t list) (ds : AST.declaration list) :
    AST.declaration list =
  let xform = new tracker vars in
  List.map (Asl_visitor.visit_decl (xform :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_valid
 ****************************************************************)

let _ =
  let group = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let targets = Ident.mk_idents (Configuration.get_strings !group) in
    Commands.declarations := xform_decls targets !Commands.declarations;
    true
  in
  let args = [
    (group, "config group");
  ]
  in
  Commands.registerCommand "xform_valid" [] args [] "Instrument UNKNOWN assignments" cmd

(****************************************************************
 * End
 ****************************************************************)
