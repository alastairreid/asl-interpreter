(****************************************************************
 * ISA integer bitslicing transform
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Isa_utils

class int_bitslice_class =
  object
    inherit Isa_visitor.nopIsaVisitor

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
  List.map (Isa_visitor.visit_decl (simplify :> Isa_visitor.isaVisitor)) ds

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new int_bitslice_class in
  Isa_visitor.visit_expr (simplify :> Isa_visitor.isaVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new int_bitslice_class in
  Isa_visitor.visit_stmts (simplify :> Isa_visitor.isaVisitor) ss

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
