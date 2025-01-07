(****************************************************************
 * ASL desugaring transformations
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val xform_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
