(****************************************************************
 * ASL to C backend
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** select which runtime library is used *)
val set_runtime : string -> unit

(** Supply of goto labels for exception implementation *)
val catch_labels : Asl_utils.nameSupply

val declarations : Format.formatter -> Asl_ast.declaration list -> unit

val get_rt_header : unit -> string list

(****************************************************************
 * End
 ****************************************************************)
