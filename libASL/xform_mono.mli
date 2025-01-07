(****************************************************************
 * ASL function monomorphization transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

val enable_auto_case_split : bool ref

val monomorphize : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)


