(****************************************************************
 * Fallback runtime library support
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support (fallback) *)

module PP = Format
module V = Value
module RT = Runtime
module FMTUtils = Format_utils

module Runtime : RT.RuntimeLib = struct

  (* All definitions in the runtime library use the "ASL_" prefix *)
  let asl_keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt ("ASL_" ^ s)

  let int_width = 128

  let min_int (num_bits : int) : Z.t = Z.shift_left Z.minus_one (num_bits - 1)
  let max_int (num_bits : int) : Z.t = Z.lognot (min_int num_bits)

  (* Return the number of bits necessary to represent an integer in binary,
     including the sign bit *)
  let bit_length (x : Z.t) : int =
    let x' = if Z.sign x = -1 then Z.succ x else x in
    (* +1 for sign bit, not taken into account by Z.numbits *)
    Z.numbits x' + 1

  (* file header needed by this runtime variant *)
  let file_header : string list = [
      "#include <assert.h>";
      "#include <stdbool.h>";
      "#include <stdint.h>";
      "#ifndef ASL_FALLBACK";
      "#define ASL_FALLBACK";
      "#endif";
      "#include \"asl/runtime.h\"";
  ]

  (* C types defined elsewhere *)
  let ty_int (fmt : PP.formatter) : unit = asl_keyword fmt "int_t"
  let ty_sintN (fmt : PP.formatter) (width : int) : unit = asl_keyword fmt "int_t"
  let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

  let c_int_width_64up (width : int) : int =
    if width > 64 then Utils.round_up_to_pow2 width else 64

  let ty_bits (fmt : PP.formatter) (width : int) : unit =
    asl_keyword fmt ("bits" ^ string_of_int (c_int_width_64up width) ^ "_t")

  (* Generate ty_int<n>_MIN macro constant *)
  let minint_constant (fmt : PP.formatter) (n : int) : unit =
    PP.pp_print_string fmt ("ASL_INT" ^ string_of_int n ^ "_MIN")

  (* Generate ty_int<n>_MAX macro constant *)
  let maxint_constant (fmt : PP.formatter) (n : int) : unit =
    PP.pp_print_string fmt ("ASL_INT" ^ string_of_int n ^ "_MAX")

  (* Try generating min/max macro constants *)
  let int_constant (fmt : PP.formatter) (n : int) (x : Z.t)
      (f : PP.formatter -> Z.t -> unit) : unit =
    if Z.equal x (min_int n) then
      minint_constant fmt n
    else if Z.equal x (max_int n) then
      maxint_constant fmt n
    else
      f fmt x

  let int_literal (fmt : PP.formatter) (x : Z.t) : unit =
    let int_literal_fit_int64 (fmt : PP.formatter) (x : Z.t) : unit =
      int_constant fmt 64 x (fun fmt x -> PP.pp_print_string fmt (Z.format "%d" x ^ "LL"))
    in

    (* Integer literal which does not fit 64-bit integer.
     * Generates a function invocation of the form ty_int_N(.., a1, a0)
     * where a0, a1, ... are 64-bit slices of the literal with a0 as the least
     * significant slice. The N of the name ty_int_N is the resulting integer
     * width rounded to the power of 2. e.g. 128, 256, ...
     *)
    let int_literal_not_fit_int64 (fmt : PP.formatter) (x : Z.t) : unit =
      let num_bits = Utils.round_up_to_pow2 (bit_length x) in
      int_constant fmt num_bits x (fun fmt x ->
          let hex_string =
            Z.format
              ("%0" ^ string_of_int (num_bits / 4) ^ "x")
              (Z.extract x 0 num_bits)
          in
          let num_limbs = num_bits / 64 in
          let limbs =
            List.init num_limbs (fun i ->
                let pos = i * 16 in
                "0x" ^ String.sub hex_string pos 16 ^ "ULL")
          in
          asl_keyword fmt ("int_" ^ string_of_int num_bits);
          FMTUtils.parens fmt (fun _ -> Utils.commasep PP.pp_print_string fmt limbs)
      )
    in

    if Z.fits_int64 x then
      int_literal_fit_int64 fmt x
    else
      int_literal_not_fit_int64 fmt x

  let bits_literal (fmt : PP.formatter) (x : Primops.bitvector) : unit =
    let bit_to_hex (b : Z.t) : string =
      Z.format "%#x" b ^ "ULL"
    in
    if x.n <= 64 then begin
        PP.pp_print_string fmt (bit_to_hex x.v)
    end else begin
      let num_bits = Utils.round_up_to_pow2 x.n in
      let num_limbs = (num_bits / 64) in
      let limbs = List.init
          num_limbs
          (fun i -> bit_to_hex (Z.extract x.v (i * 64) 64))
      in
      asl_keyword fmt "bits";
      FMTUtils.parens fmt (fun _ ->
          Utils.commasep PP.pp_print_string fmt (string_of_int num_bits :: List.rev limbs))
    end

  let unop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%s %a)"
      op
      RT.pp_expr x

  let binop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a %s %a)"
      RT.pp_expr x
      op
      RT.pp_expr y

  let apply1 (fmt : PP.formatter) (f : string) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%a)"
      asl_keyword f
      RT.pp_expr x

  let apply2 (fmt : PP.formatter) (f : string) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%a, %a)"
      asl_keyword f
      RT.pp_expr x
      RT.pp_expr y

  (* Calculate mask with x ones *)
  let mask_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    apply1 fmt "mask_int" x

  let apply_bits_1_0 (fmt : PP.formatter) (f : string) (n : int) : unit =
    PP.fprintf fmt "%a(%d, %d)"
      asl_keyword f
      (c_int_width_64up n)
      n

  let apply_bits_1_1 (fmt : PP.formatter) (f : string) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a)"
      asl_keyword f
      (c_int_width_64up n)
      n
      RT.pp_expr x

  let apply_bits_1_2 (fmt : PP.formatter) (f : string) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a)"
      asl_keyword f
      (c_int_width_64up n)
      n
      RT.pp_expr x
      RT.pp_expr y

  let add_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "+" x y
  let mul_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "*" x y
  let sub_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "-" x y
  let neg_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = unop fmt "-" x
  let zdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let zrem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "%" x y
  let shr_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">>" x y
  let shl_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<<" x y

  let eq_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "==" x y
  let ne_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "!=" x y
  let ge_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">=" x y
  let gt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">" x y
  let le_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<=" x y
  let lt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<" x y

  let exact_div_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply2 fmt "exact_div_int" x y
  let cdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply2 fmt "cdiv_int" x y
  let crem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply2 fmt "crem_int" x y
  let fdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply2 fmt "fdiv_int" x y
  let frem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply2 fmt "frem_int" x y
  let is_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = apply1 fmt "is_pow2_int" x

  let sintN_literal (fmt : PP.formatter) (x : Primops.sintN) : unit = int_literal fmt x.v

  let print_sintN_decimal (fmt : PP.formatter) (n : int) ~(add_size : bool) (x : RT.rt_expr) : unit =
    let add = if add_size then "true" else "false" in
    PP.fprintf fmt "ASL_print_int_dec(%d, %s, %a)" n add RT.pp_expr x
  let print_sintN_hexadecimal (fmt : PP.formatter) (n : int) ~(add_size : bool) (x : RT.rt_expr) : unit =
    let add = if add_size then "true" else "false" in
    PP.fprintf fmt "ASL_print_int_hex(%d, %s, %a)" n add RT.pp_expr x

  let print_int_dec (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    print_sintN_decimal fmt int_width ~add_size:false x

  let print_int_hex (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    print_sintN_hexadecimal fmt int_width ~add_size:false x

  let pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(((ASL_bits64_t)1) << %a)"
      RT.pp_expr x

  let align_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a / %a) * %a)"
      RT.pp_expr x
      RT.pp_expr y
      RT.pp_expr y

  let mod_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a & %a)"
      RT.pp_expr x
      mask_int y

  let cvt_bits_sint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = apply_bits_1_1 fmt "cvt_bits_sint" n x
  let cvt_bits_uint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = apply_bits_1_1 fmt "cvt_bits_uint" n x
  let cvt_int_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = apply_bits_1_1 fmt "cvt_int_bits" n x

  let get_slice_int (fmt : PP.formatter) (w : int) (x : RT.rt_expr) (i : RT.rt_expr) : unit =
    let slice fmt =
      PP.fprintf fmt "(%a >> %a)"
        RT.pp_expr x
        RT.pp_expr i
    in
    cvt_int_bits fmt w slice

  let set_slice_int (fmt : PP.formatter) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) (r : RT.rt_expr) : unit =
    let mask = Z.sub (Z.shift_left Z.one w) Z.one in
    PP.fprintf fmt "%a = ({ int __index = %a; %a __mask = %a << __index; (%a & ~__mask) | (((%a)%a) << __index); });"
      RT.pp_expr l
      RT.pp_expr i
      ty_sintN int_width
      int_literal mask
      RT.pp_expr l
      ty_sintN int_width
      RT.pp_expr r

  (* signed sized integer functions *)
  let eq_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = eq_int fmt x y
  let ne_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = ne_int fmt x y
  let ge_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = ge_int fmt x y
  let gt_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = gt_int fmt x y
  let le_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = le_int fmt x y
  let lt_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = lt_int fmt x y
  let add_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = add_int fmt x y
  let neg_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = neg_int fmt x
  let sub_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = sub_int fmt x y
  let mul_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = mul_int fmt x y
  let shr_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = shr_int fmt x y
  let shl_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = shl_int fmt x y
  let exact_div_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = exact_div_int fmt x y
  let zdiv_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = zdiv_int fmt x y
  let zrem_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = zrem_int fmt x y
  let cdiv_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = cdiv_int fmt x y
  let crem_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = crem_int fmt x y
  let fdiv_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = fdiv_int fmt x y
  let frem_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = frem_int fmt x y
  let is_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = is_pow2_int fmt x
  let pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = pow2_int fmt x
  let align_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = align_int fmt x y
  let mod_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = mod_pow2_int fmt x y
  let cvt_sintN_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit = cvt_int_bits fmt n x
  let cvt_bits_ssintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_sint fmt n x
  let cvt_bits_usintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_uint fmt n x
  let cvt_sintN_int (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = RT.pp_expr fmt x
  let cvt_int_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = RT.pp_expr fmt x
  let resize_sintN (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit = RT.pp_expr fmt x
  let print_sintN_dec (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = print_sintN_decimal fmt n ~add_size:true x
  let print_sintN_hex (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = print_sintN_hexadecimal fmt n ~add_size:true x

  let get_slice (fmt : PP.formatter) (n : int) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %a, %a, %d)"
      asl_keyword "slice_lowd"
      (c_int_width_64up n)
      (c_int_width_64up w)
      RT.pp_expr l
      RT.pp_expr i
      w

  let set_slice (fmt : PP.formatter) (n : int) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) (r : RT.rt_expr) : unit =
    PP.fprintf fmt "%a(%d, %d, %d, &%a, %a, %d, %a);"
      asl_keyword "set_slice"
      (c_int_width_64up n)
      (c_int_width_64up w)
      n
      RT.pp_expr l
      RT.pp_expr i
      w
      RT.pp_expr r

  let eq_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "eq_bits" n x y
  let ne_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "ne_bits" n x y
  let add_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "add_bits" n x y
  let sub_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "sub_bits" n x y
  let mul_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "mul_bits" n x y
  let and_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "and_bits" n x y
  let or_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "or_bits" n x y
  let xor_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "xor_bits" n x y
  let not_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = apply_bits_1_1 fmt "not_bits" n x
  let lsl_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "lsl_bits" n x y
  let lsr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "lsr_bits" n x y
  let asr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = apply_bits_1_2 fmt "asr_bits" n x y
  let zeros_bits (fmt : PP.formatter) (n : int) : unit = apply_bits_1_0 fmt "zeros_bits" n
  let ones_bits (fmt : PP.formatter) (n : int) : unit = apply_bits_1_0 fmt "ones_bits" n

  let mk_mask (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %a)"
        asl_keyword "mk_mask"
        (c_int_width_64up n)
        RT.pp_expr x

  let zero_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %d, %a, %d)"
        asl_keyword "zero_extend_bits"
        (c_int_width_64up m)
        (c_int_width_64up n)
        m
        RT.pp_expr x
        n

  let sign_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %d, %a, %d)"
        asl_keyword "sign_extend_bits"
        (c_int_width_64up m)
        (c_int_width_64up n)
        m
        RT.pp_expr x
        n

  let print_bits_hex (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = apply_bits_1_1 fmt "print_bits_hex" n x

  let in_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (m : Primops.mask) : unit =
      PP.fprintf fmt "((%a & %s) == %s)"
        RT.pp_expr x
        (Z.format "%#x" m.m)
        (Z.format "%#x" m.v)

  let append_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) (y : RT.rt_expr) : unit =
      let result_width = m + n in
      let x' fmt = zero_extend_bits fmt m result_width x in
      let y' fmt = zero_extend_bits fmt n result_width y in
      PP.fprintf fmt "%a(%d, %d, %d, %a, %a)"
        asl_keyword "append_bits"
        (c_int_width_64up result_width)
        m
        n
        RT.pp_expr x'
        RT.pp_expr y'

  let replicate_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
      let result_width = m * n in
      let x' fmt = zero_extend_bits fmt m result_width x in
      PP.fprintf fmt "%a(%d, %d, %a, %d)"
        asl_keyword "replicate_bits"
        (c_int_width_64up result_width)
        m
        RT.pp_expr x'
        n

  let ram_init (fmt : PP.formatter) (a : int) (ram : RT.rt_expr) (v : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %a, %a)"
        asl_keyword "ram_init"
        a
        RT.pp_expr ram
        RT.pp_expr v

  let ram_read (fmt : PP.formatter) (a : int) (n : int) (ram : RT.rt_expr) (addr : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %a, %a)"
        asl_keyword "ram_read"
        a
        n
        RT.pp_expr ram
        RT.pp_expr addr

  let ram_write (fmt : PP.formatter) (a : int) (n : int) (ram : RT.rt_expr) (addr : RT.rt_expr) (v : RT.rt_expr) : unit =
      PP.fprintf fmt "%a(%d, %d, %a, %a, %a)"
        asl_keyword "ram_write"
        a
        n
        RT.pp_expr ram
        RT.pp_expr addr
        RT.pp_expr v

  let print_char (fmt : PP.formatter) (x : RT.rt_expr) : unit = apply1 fmt "print_char" x
  let print_str (fmt : PP.formatter) (x : RT.rt_expr) : unit = apply1 fmt "print_str" x

  let info_format_int (add : string -> (PP.formatter -> unit) -> unit)
      (arg : RT.rt_expr) : unit =
    if int_width <= 64 then
      add "%lld"
        (fun fmt -> PP.fprintf fmt "(long long)%a" RT.pp_expr arg)
    else
      add "%s" (fun fmt ->
        (* Call helper function to format the value as hex only if it does
           not fit in 64-bit integer. Cannot do this check at compile time. *)
        PP.fprintf fmt "ASL_format_int_as_str((char[ASL_INT_BUFSZ]){0}, %a)"
          RT.pp_expr arg)

  let info_format_bits (add : string -> (PP.formatter -> unit) -> unit)
      (w : int) (arg : RT.rt_expr) : unit =
    let hex_chars = (w + 3) / 4 in
    add "%d'x" (fun fmt -> PP.fprintf fmt "%d" w);
    if w <= 64 then
      add (Printf.sprintf "%%0%dllx" hex_chars)
        (fun fmt -> PP.fprintf fmt "(long long)%a" RT.pp_expr arg)
    else begin
      (* Note: Leading zeros can be suppressed via a helper function, but this
         may negatively impact readability. *)
      let full_limbs = hex_chars / 16 in
      let top_hex_chars = hex_chars - full_limbs * 16 in
      if top_hex_chars > 0 then
        add (Printf.sprintf "%%0%dllx" top_hex_chars)
          (fun fmt -> PP.fprintf fmt "(long long)%a.u64[%d]" RT.pp_expr arg full_limbs);
      for i = full_limbs - 1 downto 0 do
        add "%016llx"
          (fun fmt -> PP.fprintf fmt "(long long)%a.u64[%d]" RT.pp_expr arg i)
      done
    end

  let info (fmt : PP.formatter) (n_states : int) (level : RT.rt_expr)
      (asl_fmt : string) (pp_states : (PP.formatter -> unit) list)
      (tagged_args : (int * RT.rt_expr) list) : unit =
    (* Tags for args: >=0 = Bits(n), -1 = Integer, -2 = Boolean, -3 = String *)
    let c_fmt_buf = Buffer.create (String.length asl_fmt) in
    let pp_args = ref [] in
    let add fmt_str pp =
      Buffer.add_string c_fmt_buf fmt_str;
      pp_args := pp :: !pp_args
    in
    let remaining = ref tagged_args in
    let add_literal s =
      String.iter
        (fun c ->
          if c = '%' then
            (* Escape '%' as '%%' *)
            Buffer.add_string c_fmt_buf "%%"
          else
            Buffer.add_char c_fmt_buf c)
        s
    in
    List.iter (function
      | Utils.Fmt_lit s ->
          add_literal s
      | Utils.Fmt_var _ ->
          let (tag, arg) = List.hd !remaining in
          remaining := List.tl !remaining;
          if tag = -1 then
            info_format_int add arg
          else if tag = -2 then
            add "%s"
              (fun fmt -> PP.fprintf fmt "(%a ? \"true\" : \"false\")" RT.pp_expr arg)
          else if tag = -3 then
            add "%s"
              (fun fmt -> RT.pp_expr fmt arg)
          else
            info_format_bits add tag arg
    ) (Utils.parse_fmt_string asl_fmt);
    assert (!remaining = []);
    let pp_n_states fmt = PP.pp_print_int fmt n_states in
    let pp_level fmt = RT.pp_expr fmt level in
    let pp_fmt fmt =
      PP.fprintf fmt "\"%s\"" (String.escaped (Buffer.contents c_fmt_buf))
    in
    let pp_states_cast =
      (* Cast pointers to (void *\) to avoid undefined behavior when reading
         them using va_args(args, void *\) *)
      List.map (fun p fmt -> PP.fprintf fmt "(void *)%t" p) pp_states
    in
    let printers =
      [ pp_n_states; pp_level; pp_fmt ] @ pp_states_cast @ (List.rev !pp_args)
    in
    PP.fprintf fmt "%a(%a)" asl_keyword "info"
      (Utils.commasep (fun fmt p -> p fmt))
      printers

  let end_execution (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "ASL_end_execution(%a)" RT.pp_expr x

  (* Foreign Function Interface (FFI) *)

  let ffi_c2asl_cint (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%t)%a)" ty_int RT.pp_expr x

  let ffi_asl2c_cint (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((int)%a)" RT.pp_expr x

  let ffi_c2asl_integer_small (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%t)%a)" ty_int RT.pp_expr x

  let ffi_asl2c_integer_small (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((int64_t)%a)" RT.pp_expr x

  let ffi_c2asl_sintN_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%t)%a)" ty_int RT.pp_expr x

  let ffi_asl2c_sintN_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((int64_t)%a)" RT.pp_expr x

  let ffi_c2asl_bits_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    assert (List.mem n [8; 16; 32; 64]);
    PP.fprintf fmt "((%a)%a)"
      ty_bits n
      RT.pp_expr x

  let ffi_asl2c_bits_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    assert (List.mem n [8; 16; 32; 64]);
    PP.fprintf fmt "((uint%d_t)%a)"
      n
      RT.pp_expr x

  let ffi_c2asl_bits_large (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "%a %a = *((%a*)%a);"
      ty_bits n
      RT.pp_expr x
      ty_bits n
      RT.pp_expr y

  let ffi_asl2c_bits_large (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "*((%a*)%a) = %a;"
      ty_bits n
      RT.pp_expr x
      RT.pp_expr y

end

(****************************************************************
 * End
 ****************************************************************)
