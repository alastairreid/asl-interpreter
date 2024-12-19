(****************************************************************
 * ELF loader
 *
 * Currently only handles little-endian, 64-bit
 *
 * Copyright Alastair Reid (c) 2019-2020
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type uint64 = Int64.t

(** load ELF file, returning entry address *)
val load_file : string -> (uint64 -> char -> unit) -> uint64

(****************************************************************
 * End
 ****************************************************************)
