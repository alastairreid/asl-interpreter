(****************************************************************
 * ISA tuple elimination transform
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast

(* Used by FFI support in the code generator to support
 * import/export of functions that return tuples of results
 *)
val isReturnTypeName : Ident.t -> bool

val xform_stmts : AST.stmt list -> AST.stmt list

val xform_decls : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
