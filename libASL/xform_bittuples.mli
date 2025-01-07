(****************************************************************
 * Transform bittuples to bitslice assignments.
 * Example:
 * var v1 :: bits(2);
 * var v2 :: bits(4);
 * var v3 :: bits(3);
 *
 * This lowering pass replaces:
 * [v1, v2, v3] = expr;
 * With:
 * let __a0 = expr;
 * v1 = __a0[7 +: 2];
 * v2 = __a0[3 +: 4];
 * v3 = __a0[0 +: 3];
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_stmts : AST.stmt list -> AST.stmt list

val xform_decls : AST.declaration list -> AST.declaration list
