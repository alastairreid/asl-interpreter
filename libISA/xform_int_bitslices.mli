(****************************************************************
 * ISA integer bitslicing transform
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast

val xform_expr : AST.expr -> AST.expr
val xform_stmts : AST.stmt list -> AST.stmt list
val xform_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
