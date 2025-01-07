(****************************************************************
 * ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

(* global symbol table *)
module GlobalEnv : sig
  type t

  val get_global_constant : t -> Ident.t -> Value.value option

  val get_record : t -> Ident.t -> (Ident.t list * (Ident.t * AST.ty) list) option
  val get_typedef : t -> Ident.t -> (Ident.t list * AST.ty) option

  val get_function :
    t ->
    Ident.t ->
    (Ident.t list * Ident.t list * Loc.t * AST.stmt list) option

  val set_config : t -> Ident.t -> Value.value -> unit
  val pp : Format.formatter -> t -> unit
end

(* global + local symbol table *)
module Env : sig
  type t

  val mkEnv : GlobalEnv.t -> Value.value ScopeStack.t -> t
  val newEnv : GlobalEnv.t -> t
  val globals : t -> GlobalEnv.t
  val pp : Format.formatter -> t -> unit
end

val eval_expr : Loc.t -> Env.t -> AST.expr -> Value.value
val eval_pattern : Loc.t -> Env.t -> Value.value -> AST.pattern -> bool
val eval_stmt : Env.t -> AST.stmt -> unit

val eval_proccall :
  Loc.t -> Env.t -> Ident.t -> Value.value list -> Value.value list -> unit
(** Evaluate call to procedure *)

val eval_funcall :
  Loc.t ->
  Env.t ->
  Ident.t ->
  Value.value list ->
  Value.value list ->
  Value.value
(** Evaluate call to function *)

val build_evaluation_environment : AST.declaration list -> Env.t
val build_constant_environment : AST.declaration list -> GlobalEnv.t

(****************************************************************
 * End
 ****************************************************************)
