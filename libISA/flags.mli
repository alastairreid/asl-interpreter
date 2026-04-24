(****************************************************************
 * Control flag registry
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module FlagMap : Map.S with type key = string

val flags : (bool ref * string) FlagMap.t ref

val registerFlag : string -> bool ref -> string -> unit

(****************************************************************
 * End
 ****************************************************************)
