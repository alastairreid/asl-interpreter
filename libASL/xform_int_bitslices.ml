(****************************************************************
 * ASL integer bitslicing transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils

class int_bitslice_class =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      match x with
      | Expr_Slices (Type_Integer _, expr, [ Slice_LoWd (lo, wd) ]) ->
          let n = Xform_simplify_expr.mk_add_int lo wd in
          Visitor.ChangeTo
            (Expr_Slices
               (Type_Bits (n, []), mk_cvt_int_bits n expr, [ Slice_LoWd (lo, wd) ]))
      | _ -> DoChildren
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new int_bitslice_class in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new int_bitslice_class in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new int_bitslice_class in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) ss

(****************************************************************
 * Command: :xform_int_bitslice
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_int_bitslices" [] [] [] "Simplify x[lo +: wd] when x : integer" cmd

(****************************************************************
 * End
 ****************************************************************)
