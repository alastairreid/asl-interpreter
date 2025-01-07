(****************************************************************
 * ASL desugaring transformations
 *
 * This performs some simple desugaring transformations:
 *
 * 1) add_bits_int{N}(x, y) -> add_bits(x, mk_cvt_int_bits{N}(y, N))
 * 2) sub_bits_int{N}(x, y) -> sub_bits(x, mk_cvt_int_bits{N}(y, N))
 * 3) mul_bits_int{N}(x, y) -> mul_bits(x, mk_cvt_int_bits{N}(y, N))
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_ast
open Builtin_idents

class desugar (ds : AST.declaration list option) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i add_bits_int ->
        Visitor.ChangeTo (Asl_utils.mk_add_bits n x (Asl_utils.mk_cvt_int_bits n y))
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i sub_bits_int ->
        Visitor.ChangeTo (Asl_utils.mk_sub_bits n x (Asl_utils.mk_cvt_int_bits n y))
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i sub_bits_int ->
        Visitor.ChangeTo (Asl_utils.mk_mul_bits n x (Asl_utils.mk_cvt_int_bits n y))
      | _ -> DoChildren
      )
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let xform = new desugar (Some ds) in
  List.map (Asl_visitor.visit_decl (xform :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_desugar
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_desugar" [] [] [] "Remove syntactic sugar" cmd

(****************************************************************
 * End
 ****************************************************************)
