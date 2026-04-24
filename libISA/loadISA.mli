(****************************************************************
 * Functions for processing ISA files
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
module TC = Tcheck

val read_file : string list -> string -> bool -> bool -> Isa_ast.declaration list
(** Parse and typecheck ISA file *)

val parse_spec : string list -> string -> bool -> Isa_ast.declaration list

val parse_file : string list -> string -> bool -> Isa_ast.declaration list
(** Parse ISA file, but do not typecheck *)

val read_files : string list -> string list -> bool -> Isa_ast.declaration list
(** Parse and typecheck ISA files. All files are first parsed then typechecked
    together.*)

val read_config : TC.Env.t -> Loc.t -> string -> Ident.t * AST.expr * AST.ty
val read_expr : TC.Env.t -> Loc.t -> string -> AST.expr
val read_stmt : TC.Env.t -> string -> AST.stmt list
val read_stmts : TC.Env.t -> string -> AST.stmt list
val read_declarations_unsorted : TC.GlobalEnv.t -> string -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
