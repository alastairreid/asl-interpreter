(****************************************************************
 * Global checks
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val check_defn_markers : bool ref
val check_call_markers : bool ref

val check_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
