(****************************************************************
 * ASL lowering transform
 *
 * Transforms
 * - Slice_HiLo to Slice_LoWd
 * - Slice_Single to Slice_LoWd
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_decls : AST.declaration list -> AST.declaration list
val xform_expr : AST.expr -> AST.expr
val xform_stmts : AST.stmt list -> AST.stmt list

(****************************************************************
 * End
 ****************************************************************)
