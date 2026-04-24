(****************************************************************
 * Global checks
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast

val check_defn_markers : bool ref
val check_call_markers : bool ref

val check_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
