(****************************************************************
 * ASL complex case elimination transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_expr : AST.expr -> AST.expr
val xform_stmts : AST.stmt list -> AST.stmt list
val xform_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
