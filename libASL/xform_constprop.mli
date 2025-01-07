(****************************************************************
 * ASL constant propagation transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

module Env : sig
  type t
  val pp : Format.formatter -> t -> unit
end

val unroll_loops : bool ref

val mkEnv : Eval.GlobalEnv.t -> (Ident.t * Value.value) list -> Env.t

val xform_ty : Env.t -> AST.ty -> AST.ty
val xform_expr : Env.t -> AST.expr -> AST.expr
val xform_stmts : Env.t -> AST.stmt list -> AST.stmt list

val xform_decls : Eval.GlobalEnv.t -> AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
