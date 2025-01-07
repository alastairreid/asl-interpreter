(****************************************************************
 * Locations in source code
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type pos = Lexing.position

(** Location tracking *)
type t =
    | Unknown
    | Int of string * t option
    | Generated of t
    | Range of pos * pos

val to_string : t -> string

val pp : Format.formatter -> t -> unit

(****************************************************************
 * End
 ****************************************************************)
