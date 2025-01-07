(****************************************************************
 * Runtime library support interface
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support *)

module AST = Asl_ast
module PP = Format

(* Representation of an expression used by runtime libraries.
 *
 * The runtime does not directly examine expressions but it needs
 * to be able to print subexpressions.
 *
 * This type should be treated as an abstract type because we will
 * likely want to add other information later.
 *)
type rt_expr = PP.formatter -> unit

let pp_expr (fmt : PP.formatter) (x : rt_expr) : unit = x fmt

let mk_rt_expr (pp : PP.formatter -> AST.expr -> unit) (x : AST.expr) : rt_expr =
    (fun fmt -> pp fmt x)

module type RuntimeLib = sig
  (* file header needed by this runtime variant *)
  val file_header : string list

  (* types *)
  val ty_int : PP.formatter -> unit
  val ty_sintN : PP.formatter -> int -> unit
  val ty_ram : PP.formatter -> unit
  val ty_bits : PP.formatter -> int -> unit

  (* literal constants *)
  val int_literal : PP.formatter -> Z.t -> unit
  val sintN_literal : PP.formatter -> Primops.sintN -> unit
  val bits_literal : PP.formatter -> Primops.bitvector -> unit

  (* integer functions *)
  val add_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val mul_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val sub_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val neg_int : PP.formatter -> rt_expr -> unit
  val zdiv_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val zrem_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val shr_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val shl_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val eq_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val ne_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val ge_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val gt_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val le_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val lt_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val exact_div_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val fdiv_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val frem_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val is_pow2_int : PP.formatter -> rt_expr -> unit
  val pow2_int : PP.formatter -> rt_expr -> unit
  val align_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val mod_pow2_int : PP.formatter -> rt_expr -> rt_expr -> unit
  val print_int_dec : PP.formatter -> rt_expr -> unit
  val print_int_hex : PP.formatter -> rt_expr -> unit

  (* signed sized integer functions *)
  val eq_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val ne_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val ge_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val gt_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val le_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val lt_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val add_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val neg_sintN : PP.formatter -> int -> rt_expr -> unit
  val sub_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val shl_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val shr_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val mul_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val exact_div_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val zdiv_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val zrem_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val fdiv_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val frem_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val is_pow2_sintN : PP.formatter -> int -> rt_expr -> unit
  val pow2_sintN : PP.formatter -> int -> rt_expr -> unit
  val align_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val mod_pow2_sintN : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val cvt_sintN_bits : PP.formatter -> int -> int -> rt_expr -> unit
  val cvt_bits_ssintN : PP.formatter -> int -> rt_expr -> unit
  val cvt_bits_usintN : PP.formatter -> int -> rt_expr -> unit
  val cvt_sintN_int : PP.formatter -> int -> rt_expr -> unit
  val cvt_int_sintN : PP.formatter -> int -> rt_expr -> unit
  val resize_sintN : PP.formatter -> int -> int -> rt_expr -> unit
  val print_sintN_dec : PP.formatter -> int -> rt_expr -> unit
  val print_sintN_hex : PP.formatter -> int -> rt_expr -> unit

  (* bitvector functions *)
  val get_slice : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val set_slice : PP.formatter -> int -> int -> rt_expr -> rt_expr -> rt_expr -> unit
  val eq_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val ne_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val add_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val sub_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val mul_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val and_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val or_bits  : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val eor_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val not_bits : PP.formatter -> int -> rt_expr -> unit
  val lsl_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val lsr_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val asr_bits : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val cvt_bits_sint : PP.formatter -> int -> rt_expr -> unit
  val cvt_bits_uint : PP.formatter -> int -> rt_expr -> unit
  val cvt_int_bits : PP.formatter -> int -> rt_expr -> unit
  val zeros_bits : PP.formatter -> int -> unit
  val ones_bits : PP.formatter -> int -> unit
  val mk_mask : PP.formatter -> int -> rt_expr -> unit
  val zero_extend_bits : PP.formatter -> int -> int -> rt_expr -> unit
  val sign_extend_bits : PP.formatter -> int -> int -> rt_expr -> unit
  val in_bits : PP.formatter -> int -> rt_expr -> Primops.mask -> unit
  val append_bits : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val replicate_bits : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val print_bits_hex : PP.formatter -> int -> rt_expr -> unit

  (* RAM functions *)
  val ram_init : PP.formatter -> int -> rt_expr -> rt_expr -> unit
  val ram_read : PP.formatter -> int -> int -> rt_expr -> rt_expr -> unit
  val ram_write : PP.formatter -> int -> int -> rt_expr -> rt_expr -> rt_expr -> unit

  (* Print functions *)
  val print_char : PP.formatter -> rt_expr -> unit
  val print_str : PP.formatter -> rt_expr -> unit

  (* Foreign Function Interface (FFI) *)
  val ffi_integer_to_c_int : PP.formatter -> rt_expr -> unit
  val ffi_integer_to_c_sint64 : PP.formatter -> rt_expr -> unit
  val ffi_bits_to_c_uint64 : PP.formatter -> int -> rt_expr -> unit
end

(****************************************************************
 * End
 ****************************************************************)
