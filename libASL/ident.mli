(****************************************************************
 * Identifier support
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type t

val hash : t -> int

val compare : t -> t -> int

val matches : t -> name:string -> bool

val equal : t -> t -> bool

val root_equal : t -> root:t -> bool

val in_list : t -> t list -> bool

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val name_with_tag : t -> string

val name : t -> string

val is_function : t -> bool

val mk_ident : string -> t
(** Make an identifier with name. If it already exists, the existing identifier
  * is returned *)

val mk_idents : string list -> t list

val mk_fident : string -> t
(** Make a function identifier with a name. If it already exists, the existing
  * identifier is returned *)

val mk_fidents : string list -> t list

val mk_fident_with_tag : t -> tag:int -> t
(** Make a new function identifier based on a previous identfier *)

val add_suffix : t -> suffix:string -> t

val add_prefix : t -> prefix:string -> t

(****************************************************************
 * End
 ****************************************************************)
