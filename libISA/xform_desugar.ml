(****************************************************************
 * ISA desugaring transformations
 *
 * This performs some simple desugaring transformations:
 *
 * 1) add_bits_int{N}(x, y) -> add_bits(x, mk_cvt_int_bits{N}(y, N))
 * 2) sub_bits_int{N}(x, y) -> sub_bits(x, mk_cvt_int_bits{N}(y, N))
 * 3) mul_bits_int{N}(x, y) -> mul_bits(x, mk_cvt_int_bits{N}(y, N))
 * 4) Std::Bits::Append(x, y) -> Expr_Concat(x, y)
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Isa_ast
open Builtin_idents

class desugar (ds : AST.declaration list option) =
  object (self)
    inherit Isa_visitor.nopIsaVisitor

    method! vexpr x =
      ( match x with
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i add_bits_int ->
        Visitor.ChangeTo (Isa_utils.mk_add_bits n x (Isa_utils.mk_cvt_int_bits n y))
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i sub_bits_int ->
        Visitor.ChangeTo (Isa_utils.mk_sub_bits n x (Isa_utils.mk_cvt_int_bits n y))
      | Expr_TApply (i, [n], [x; y], _) when Ident.equal i mul_bits_int ->
        Visitor.ChangeTo (Isa_utils.mk_mul_bits n x (Isa_utils.mk_cvt_int_bits n y))
      | Expr_TApply (i, szs, args, _) when Ident.equal i append_bits ->
        let f (acc1, acc2) sz arg =
          let e1 = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) sz in
          let e2 = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) arg in
          match e2 with
          | Expr_Concat (ws, es) -> (List.rev_append ws acc1, List.rev_append es acc2)
          | _ -> (e1 :: acc1, e2 :: acc2)
        in
        let es1, es2 = List.fold_left2 f ([], []) szs args in
        let szs', args' = (List.rev es1, List.rev es2) in
        Visitor.ChangeTo (Isa_utils.mk_concat szs' args')
      | _ -> DoChildren
      )
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let xform = new desugar (Some ds) in
  List.map (Isa_visitor.visit_decl (xform :> Isa_visitor.isaVisitor)) ds

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
