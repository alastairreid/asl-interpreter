(****************************************************************
 * Identifiers of builtin function, types, etc.
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

val not_bool : Ident.t
val neg_int : Ident.t
val not_bits : Ident.t
val zeros_bits : Ident.t
val ones_bits : Ident.t
val is_zero : Ident.t
val is_ones : Ident.t
val lazy_and_bool : Ident.t
val strict_and_bool : Ident.t
val lazy_or_bool : Ident.t
val strict_or_bool : Ident.t
val eq_enum : Ident.t
val eq_int : Ident.t
val le_int : Ident.t
val add_int : Ident.t
val sub_int : Ident.t
val mul_int : Ident.t
val eq_bits : Ident.t
val add_bits : Ident.t
val sub_bits : Ident.t
val mul_bits : Ident.t
val in_mask : Ident.t
val and_bits : Ident.t
val or_bits : Ident.t
val lsr_bits : Ident.t
val lsl_bits : Ident.t
val length : Ident.t
val max : Ident.t
val min : Ident.t
val eq_bool : Ident.t
val ne_bool : Ident.t
val ne_enum : Ident.t
val align_int : Ident.t
val exact_div_int : Ident.t
val cdiv_int : Ident.t
val crem_int : Ident.t
val fdiv_int : Ident.t
val frem_int : Ident.t
val ge_int : Ident.t
val gt_int : Ident.t
val is_pow2_int : Ident.t
val lt_int : Ident.t
val mod_pow2_int : Ident.t
val ne_int : Ident.t
val pdiv_int : Ident.t
val prem_int : Ident.t
val pow2_int : Ident.t
val pow_int_int : Ident.t
val shl_int : Ident.t
val shr_int : Ident.t
val zdiv_int : Ident.t
val zrem_int : Ident.t
val cvt_bits_sint : Ident.t
val cvt_bits_uint : Ident.t
val cvt_int_bits : Ident.t
val xor_bits : Ident.t
val frem_bits_int : Ident.t
val notin_mask : Ident.t
val asr_bits : Ident.t
val pow2_bits : Ident.t
val ne_bits : Ident.t
val replicate_bits : Ident.t
val print_int_hex : Ident.t
val print_int_dec : Ident.t
val print_char : Ident.t
val print_str : Ident.t
val print_bits_hex : Ident.t
val ram_init : Ident.t
val ram_read : Ident.t
val ram_write : Ident.t
val add_bits_int : Ident.t
val sub_bits_int : Ident.t
val mul_bits_int : Ident.t
val zero_extend_bits : Ident.t
val sign_extend_bits : Ident.t
val append_bits : Ident.t
val mk_mask : Ident.t
val mask_int : Ident.t

(* FFI support *)
val cint_type         : Ident.t
val uints_type        : Ident.t
val uint_type         : Ident.t
val sint_type         : Ident.t
val int_type          : Ident.t

val cint_to_integer   : Ident.t
val cint_from_integer : Ident.t
val uints_to_bits     : Ident.t
val uints_from_bits   : Ident.t
val uint_to_bits      : Ident.t
val uint_from_bits    : Ident.t
val uint_to_integer   : Ident.t
val uint_from_integer : Ident.t
val sint_to_integer   : Ident.t
val sint_from_integer : Ident.t
val int_to_integer    : Ident.t
val int_from_integer  : Ident.t

val asl_reset : Ident.t
val asl_step : Ident.t
val asl_get_pc : Ident.t
val asl_set_pc : Ident.t
val asl_elf_write_memory8 : Ident.t
val asl_fake_return_value : Ident.t
val asl_error_unmatched_case : Ident.t
val asl_end_execution : Ident.t
val asl_fuzz : Ident.t
val trace_next : Ident.t
val trace_physical_memory : Ident.t
val trace_virtual_memory : Ident.t
val trace_page_table_walk : Ident.t
val trace_error : Ident.t
val trace_event : Ident.t
val boolean_ident : Ident.t
val integer_ident : Ident.t
val string_ident : Ident.t
val bits_ident : Ident.t
val slice_ident : Ident.t
val wildcard_ident : Ident.t
val dash_ident : Ident.t
val ram : Ident.t
val true_ident : Ident.t
val false_ident : Ident.t

val sintN            : Ident.t
val eq_sintN         : Ident.t
val ne_sintN         : Ident.t
val gt_sintN         : Ident.t
val ge_sintN         : Ident.t
val le_sintN         : Ident.t
val lt_sintN         : Ident.t
val add_sintN        : Ident.t
val neg_sintN        : Ident.t
val sub_sintN        : Ident.t
val shl_sintN        : Ident.t
val shr_sintN        : Ident.t
val mul_sintN        : Ident.t
val exact_div_sintN  : Ident.t
val zdiv_sintN       : Ident.t
val zrem_sintN       : Ident.t
val cdiv_sintN       : Ident.t
val crem_sintN       : Ident.t
val fdiv_sintN       : Ident.t
val frem_sintN       : Ident.t
val is_pow2_sintN    : Ident.t
val pow2_sintN       : Ident.t
val align_sintN      : Ident.t
val mod_pow2_sintN   : Ident.t
val cvt_sintN_bits   : Ident.t
val cvt_bits_ssintN  : Ident.t
val cvt_bits_usintN  : Ident.t
val cvt_sintN_int    : Ident.t
val cvt_int_sintN    : Ident.t
val resize_sintN     : Ident.t
val print_sintN_dec  : Ident.t
val print_sintN_hex  : Ident.t
val print            : Ident.t
val print_boolean    : Ident.t
val info             : Ident.t

(* deprecated *)
val old_bits_ident : Ident.t
val old_integer_ident : Ident.t

(****************************************************************
 * End
 ****************************************************************)
