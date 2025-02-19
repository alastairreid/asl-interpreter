(****************************************************************
 * Algorithmic C runtime library support
 * This uses the C++ "Algorithmic C" library "ac_int.h"
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support (C++) *)

module PP = Format
module V = Value
module RT = Runtime
open Utils

module Runtime : RT.RuntimeLib = struct

  (* All definitions in the runtime library use the "ASL_" prefix *)
  let asl_keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt ("ASL_" ^ s)

  let int_width = 128

  let zero_sintN (fmt : PP.formatter) (n : int) : unit =
    PP.fprintf fmt "ac_int<128,true>(value<AC_VAL_0,%d,true>(0))" n

  let min_sintN (fmt : PP.formatter) (n : int) : unit =
    PP.fprintf fmt "ac_int<128,true>(value<AC_VAL_MIN,%d,true>(0))" n

  let max_sintN (fmt : PP.formatter) (n : int) : unit =
    PP.fprintf fmt "ac_int<128,true>(value<AC_VAL_MAX,%d,true>(0))" n

  (* signed and unsigned ints *)
  let ty_sint (fmt : PP.formatter) (width : int) : unit =
    PP.fprintf fmt "ac_int<%d,true>" width

  let ty_uint (fmt : PP.formatter) (width : int) : unit =
    PP.fprintf fmt "ac_int<%d,false>" width

  (* file header needed by this runtime variant *)
  let file_header : string list = [
      "#include <cstdint>";
      "#include \"ac_int.h\"";
      "#ifndef ASL_AC";
      "#define ASL_AC";
      "#endif";
      "#include \"asl/runtime.hpp\"";
  ]

  let ty_int (fmt : PP.formatter) : unit = ty_sint fmt int_width
  let ty_sintN (fmt : PP.formatter) (width : int) : unit = ty_sint fmt width
  let ty_bits (fmt : PP.formatter) (width : int) : unit = ty_uint fmt width
  let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

  (** split a bigint into a number of chunks in little-endian order *)
  let split_int (x : Z.t) (num_chunks : int) (chunk_size : int) : Z.t list =
    List.init
      num_chunks
      (fun i -> Z.extract x (i * chunk_size) chunk_size)

  let constant_u32 (fmt : PP.formatter) (x : Z.t) : unit =
    PP.fprintf fmt "static_cast<int>(%sUL)" (Z.format "%#x" x)

  let pos_int_literal (n : int) (fmt : PP.formatter) (x : Z.t) : unit =
    let num_limbs = (n + 31) / 32 in
    let limbs = split_int x num_limbs 32 in
    PP.fprintf fmt "ac::bit_fill<%a>((int [%d]){"
      ty_sint n
      num_limbs;
    PP.pp_print_list
      ~pp_sep:(fun fmt _ -> PP.pp_print_string fmt ", ")
      constant_u32
      fmt
      limbs;
    PP.fprintf fmt "}, false)"

  let intN_literal (n : int) (fmt : PP.formatter) (x : Z.t) : unit =
    if Z.geq x Z.zero then
      pos_int_literal n fmt x
    else
      PP.fprintf fmt "(-%a)"
        (pos_int_literal n) (Z.neg x)

  let int_literal (fmt : PP.formatter) (x : Z.t) : unit = intN_literal int_width fmt x
  let sintN_literal (fmt : PP.formatter) (x : Primops.sintN) : unit = intN_literal x.n fmt x.v

  let bits_literal (fmt : PP.formatter) (x : Primops.bitvector) : unit =
    if x.n <= 32 then begin
      Format.fprintf fmt "ac_int<%d,false>(%a)"
        x.n
        constant_u32 x.v
    end else begin
      let num_limbs = (x.n + 31) / 32 in
      let limbs = split_int x.v num_limbs 32 in
      PP.fprintf fmt "ac::bit_fill<%a>((int [%d]){"
        ty_uint x.n
        num_limbs;
      PP.pp_print_list
        ~pp_sep:(fun fmt _ -> PP.pp_print_string fmt ", ")
        constant_u32
        fmt
        limbs;
      PP.fprintf fmt "}, false)"
    end

  let empty_bits (fmt : PP.formatter) (_ : unit) : unit = PP.pp_print_string fmt "ac_int<0,false>(0)"

  let unop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%s %a)"
      op
      RT.pp_expr x

  let binop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a %s %a)"
      RT.pp_expr x
      op
      RT.pp_expr y

  (* signed sized integers *)

  let eq_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "==" x y
  let ne_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "!=" x y
  let ge_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">=" x y
  let gt_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">" x y
  let le_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<=" x y
  let lt_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<" x y
  let add_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "+" x y
  let neg_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = unop fmt "-" x
  let sub_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "-" x y
  let mul_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "*" x y
  let shr_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">>" x y
  let shl_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<<" x y
  let exact_div_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let zdiv_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "/" x y
  let zrem_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "%" x y

  let fdiv_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "({ %a __tmp1 = %a; "
      ty_sint n
      RT.pp_expr x;
    PP.fprintf fmt "%a __tmp2 = %a; "
      ty_sint n
      RT.pp_expr y;
    PP.fprintf fmt "%a __tmp3 = __tmp1 / __tmp2; " ty_sint n;
    PP.fprintf fmt "%a __tmp4 = __tmp1 %% __tmp2; " ty_sint n;
    PP.fprintf fmt "if (__tmp4 != 0 && (__tmp1 < %a || __tmp2 < %a)) { __tmp3 = __tmp3 - 1; } "
      zero_sintN n
      zero_sintN n;
    PP.fprintf fmt "__tmp3; })"

  let frem_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "({ %a __tmp1 = %a; "
      ty_sint n
      RT.pp_expr x;
    PP.fprintf fmt "%a __tmp2 = %a; "
      ty_sint n
      RT.pp_expr y;
    PP.fprintf fmt "%a __tmp3 = __tmp1 / __tmp2; " ty_sint n;
    PP.fprintf fmt "%a __tmp4 = __tmp1 %% __tmp2; " ty_sint n;
    PP.fprintf fmt "if (__tmp4 != 0 && (__tmp1 < %a || __tmp2 < %a)) { __tmp4 = __tmp4 + __tmp2; } "
      zero_sintN n
      zero_sintN n;
    PP.fprintf fmt "__tmp4; })"

  let is_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "({ %a __tmp = %a; "
      ty_sint n
      RT.pp_expr x;
    PP.fprintf fmt "__tmp != 0 && (__tmp & (__tmp - 1)) == 0; })"

  let pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a << %a)"
      int_literal Z.one
      RT.pp_expr x

  let align_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    (* x & (~((1 << y) - 1)) *)
    PP.fprintf fmt "(%a & (~((%a << %a) - %a)))"
      RT.pp_expr x
      int_literal Z.one
      RT.pp_expr y
      int_literal Z.one

  let mod_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    (* x & ((1 << y) - 1) *)
    PP.fprintf fmt "(%a & ((%a << %a) - %a))"
      RT.pp_expr x
      int_literal Z.one
      RT.pp_expr y
      int_literal Z.one

  (* A generalization of cvt_bits_ssintN that can either
   * - convert bits(N) to __sint(N)
   * or
   * - convert bits(N) to integer (i.e., __sint(int_width))
   *)
  let cvt_bits_sint_aux (fmt : PP.formatter) (n : int) (target_width : int) (x : RT.rt_expr) : unit =
    if target_width = 0 then begin
      (* Although we return zero, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; %a; })"
        RT.pp_expr x
        (intN_literal target_width) Z.zero
    end else if target_width = 1 then begin
      (* signed _BitInt must be at least 2 bits *)
      PP.fprintf fmt "(%a ? %a : %a)"
        RT.pp_expr x
        (intN_literal target_width) Z.one
        (intN_literal target_width) Z.zero
    end else begin
      PP.fprintf fmt "((%a)((%a)%a))"
        ty_sint target_width
        ty_sint n
        RT.pp_expr x
    end

  let cvt_bits_ssintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_sint_aux fmt n n x

  (* A generalization of cvt_bits_usintN that can either
   * - convert bits(N) to __sint(N+1)
   * or
   * - convert bits(N) to integer (i.e., __sint(int_width))
   *)
  let cvt_bits_uint_aux (fmt : PP.formatter) (n : int) (target_width : int) (x : RT.rt_expr) : unit =
    if target_width = 0 then begin
      (* Although we return zero, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; %a; })"
        RT.pp_expr x
        int_literal Z.zero
    end else begin
      PP.fprintf fmt "((%a)((%a)%a))"
        ty_sint target_width
        ty_uint target_width
        RT.pp_expr x
    end

  let cvt_bits_usintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_uint_aux fmt n (n+1) x

  let cvt_sintN_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; %a; })"
        RT.pp_expr x
        empty_bits ()
    else
      PP.fprintf fmt "((%a)((%a)%a))"
        ty_uint n
        ty_uint m
        RT.pp_expr x

  let cvt_sintN_int (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint n
      RT.pp_expr x

  let cvt_int_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint int_width
      RT.pp_expr x

  let resize_sintN (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint int_width
      RT.pp_expr x

  let print_sint64_decimal (fmt : PP.formatter) (n : int) (add_size : bool) (x : string) : unit =
    if add_size then begin
      PP.fprintf fmt "    if (%s == %a) {@," x min_sintN n;
      PP.fprintf fmt "      printf(\"-i%d'd%s\");@," n (Z.to_string (Z.shift_left Z.one (n - 1)));
      PP.fprintf fmt "    } else {@,";
      PP.fprintf fmt "      if (%s < 0) {@," x;
      PP.fprintf fmt "        %s = -%s;@," x x;
      PP.fprintf fmt "        printf(\"-\");@,";
      PP.fprintf fmt "      }@,";
      PP.fprintf fmt "      printf(\"i%d'd%%llu\", %s.to_uint64());@," n x;
      PP.fprintf fmt "    }"
    end else begin
      PP.fprintf fmt "    printf(\"%%lld\", %s.to_uint64());@," x
    end

  let print_sintN_decimal (fmt : PP.formatter) (n : int) ~(add_size : bool) (x : RT.rt_expr) : unit =
    if n <= 64 then begin
        PP.fprintf fmt "@[<v>{ %a __tmp = %a;@," ty_sint n RT.pp_expr x;
        print_sint64_decimal fmt n add_size "__tmp";
        PP.fprintf fmt "}@]"
    end else begin
        (* Print small numbers in decimal, large numbers in hex *)
        PP.fprintf fmt "@[<v>{ %a __tmp = %a;@," ty_sint n RT.pp_expr x;
        PP.fprintf fmt "  if (__tmp >= %a && __tmp <= %a) {@,"
          min_sintN 63
          max_sintN 63;
        print_sint64_decimal fmt n add_size "__tmp";
        PP.fprintf fmt "  } else {@,";
        PP.fprintf fmt "    if (__tmp < %a) {@," zero_sintN n;
        PP.fprintf fmt "      __tmp = -__tmp;@,";
        PP.fprintf fmt "      printf(\"-\");@,";
        PP.fprintf fmt "    }@,";
        (if add_size then PP.fprintf fmt "    printf(\"%d'x\");@," n
                     else PP.fprintf fmt "    printf(\"0x\");@,");
        PP.fprintf fmt "    bool leading = true;@,";
        PP.fprintf fmt "    for(int i = (%d-1)&~3; i >= 0; i -= 4) {@," n;
        PP.fprintf fmt "      unsigned c = ((unsigned) __tmp.slc<4>(i)) & 15;@,";
        PP.fprintf fmt "      if (leading) {@,";
        PP.fprintf fmt "        if (i == 0 || c) {@,";
        PP.fprintf fmt "          printf(\"%%x\", c);@,";
        PP.fprintf fmt "          leading = false;@,";
        PP.fprintf fmt "        }@,";
        PP.fprintf fmt "      } else {@,";
        PP.fprintf fmt "        printf(\"%%x\", c);@,";
        PP.fprintf fmt "      }@,";
        PP.fprintf fmt "    }@,";
        PP.fprintf fmt "  }@,";
        PP.fprintf fmt "}@]"
    end

  let print_sintN_hexadecimal (fmt : PP.formatter) (n : int) ~(add_size : bool) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "@[<v>{ %a __tmp = %a;@," ty_sint n RT.pp_expr x;
    PP.fprintf fmt "  if (__tmp < %a) {@," zero_sintN n;
    PP.fprintf fmt "    __tmp = -__tmp;@,";
    PP.fprintf fmt "    printf(\"-\");@,";
    PP.fprintf fmt "  }@,";
    (if add_size then PP.fprintf fmt "  printf(\"i%d'x\");@," n
                 else PP.fprintf fmt "  printf(\"0x\");@,");
    PP.fprintf fmt "  bool leading = true;@,";
    PP.fprintf fmt "  for(int i = (%d-1)&~3; i >= 0; i -= 4) {@," n;
    PP.fprintf fmt "    unsigned c = ((unsigned) __tmp.slc<4>(i)) & 15;@,";
    PP.fprintf fmt "    if (leading) {@,";
    PP.fprintf fmt "      if (i == 0 || c) {@,";
    PP.fprintf fmt "        printf(\"%%x\", c);@,";
    PP.fprintf fmt "        leading = false;@,";
    PP.fprintf fmt "      }@,";
    PP.fprintf fmt "    } else {@,";
    PP.fprintf fmt "      printf(\"%%x\", c);@,";
    PP.fprintf fmt "    }@,";
    PP.fprintf fmt "  }@,";
    PP.fprintf fmt "}@]"

  let print_sintN_dec (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    print_sintN_decimal fmt n ~add_size:true x

  let print_sintN_hex (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    print_sintN_hexadecimal fmt n ~add_size:true x


  (* signed unbounded integers *)

  let add_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = add_sintN fmt int_width x y
  let sub_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = sub_sintN fmt int_width x y
  let neg_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = neg_sintN fmt int_width x
  let shr_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = shr_sintN fmt int_width x y
  let shl_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = shl_sintN fmt int_width x y
  let mul_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = mul_sintN fmt int_width x y
  let exact_div_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = exact_div_sintN fmt int_width x y
  let zdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = zdiv_sintN fmt int_width x y
  let zrem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = zrem_sintN fmt int_width x y
  let fdiv_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = fdiv_sintN fmt int_width x y
  let frem_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = frem_sintN fmt int_width x y
  let eq_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = eq_sintN fmt int_width x y
  let ne_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = ne_sintN fmt int_width x y
  let ge_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = ge_sintN fmt int_width x y
  let gt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = gt_sintN fmt int_width x y
  let le_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = le_sintN fmt int_width x y
  let lt_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = lt_sintN fmt int_width x y
  let is_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = is_pow2_sintN fmt int_width x
  let pow2_int (fmt : PP.formatter) (x : RT.rt_expr) : unit = pow2_sintN fmt int_width x
  let align_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = align_sintN fmt int_width x y
  let mod_pow2_int (fmt : PP.formatter) (x : RT.rt_expr) (y : RT.rt_expr) : unit = mod_pow2_sintN fmt int_width x y
  let cvt_int_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_sintN_bits fmt int_width n x
  let cvt_bits_sint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_sint_aux fmt n int_width x
  let cvt_bits_uint (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit = cvt_bits_uint_aux fmt n int_width x

  let print_int_dec (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    print_sintN_decimal fmt int_width ~add_size:false x

  let print_int_hex (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    print_sintN_hexadecimal fmt int_width ~add_size:false x

  let get_slice (fmt : PP.formatter) (n : int) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) : unit =
    if w = 1 then
      PP.fprintf fmt "(%a[(int)(%a)])"
        RT.pp_expr l
        RT.pp_expr i
    else
      PP.fprintf fmt "(%a.slc<%d>((int)(%a)))"
        RT.pp_expr l
        w
        RT.pp_expr i

  let set_slice (fmt : PP.formatter) (n : int) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) (r : RT.rt_expr) : unit =
    if w = 1 then
      PP.fprintf fmt "%a[(int)(%a)] = %a;"
        RT.pp_expr l
        RT.pp_expr i
        RT.pp_expr r
    else
      PP.fprintf fmt "%a.set_slc((int)(%a), %a);"
        RT.pp_expr l
        RT.pp_expr i
        RT.pp_expr r

  let eq_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "==" x y
  let ne_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "!=" x y
  let add_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "+" x y
  let sub_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "-" x y
  let mul_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "*" x y
  let and_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "&" x y
  let or_bits  (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "|" x y
  let eor_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "^" x y

  let not_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; %a; })"
        RT.pp_expr x
        empty_bits ()
    else
      unop fmt "~" x

  let lsl_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt "<<" x y
  let lsr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit = binop fmt ">>" x y

  let asr_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)(((%a)%a) >> %a))"
      ty_uint n
      ty_sint n
      RT.pp_expr x
      RT.pp_expr y

  let zeros_bits (fmt : PP.formatter) (n : int) : unit =
    bits_literal fmt (Primops.mkBits n Z.zero)

  let ones_bits (fmt : PP.formatter) (n : int) : unit =
    let x = Z.sub (Z.shift_left Z.one n) Z.one in
    bits_literal fmt (Primops.mkBits n x)

  let mk_mask (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; %a; })"
        RT.pp_expr x
        empty_bits ()
    else
      PP.fprintf fmt "(%a >> (%d - %a))"
        ones_bits n
        n
        RT.pp_expr x

  let zero_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
    ty_uint n
    RT.pp_expr x

  let sign_extend_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)(%a)(%a)%a)"
    ty_uint n
    ty_sint n
    ty_sint m
    RT.pp_expr x

  let print_bits_hex (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then begin
      (* Although we print empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "(void)%a;@," RT.pp_expr x;
      PP.fprintf fmt "printf(\"0'x0\")"
    end else begin
      let chunks = (n+63) / 64 in
      PP.fprintf fmt "{ @[<v>%a __tmp = %a;@,"
        ty_bits n
        RT.pp_expr x;

      PP.fprintf fmt "printf(\"%d'x\");@," n;
      PP.fprintf fmt "bool leading = true;@,";
      PP.fprintf fmt "for (int i = %d; i >= 0; --i) {@," (chunks-1);
      PP.fprintf fmt "  uint64_t chunk = (__tmp >> (64 * i)).to_uint64();@,";
      PP.fprintf fmt "  if (leading) {@,";
      PP.fprintf fmt "    if (i == 0 || chunk) {@,";
      PP.fprintf fmt "      printf(\"%%lx\", chunk);@,";
      PP.fprintf fmt "      leading = false;@,";
      PP.fprintf fmt "    }@,";
      PP.fprintf fmt "  } else {@,";
      PP.fprintf fmt "    printf(\"%%08lx\", chunk);@,";
      PP.fprintf fmt "  }@,";
      PP.fprintf fmt "}@,";
      PP.fprintf fmt "}@]"
    end

  let in_bits (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (m : Primops.mask) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; true; })" RT.pp_expr x
    else
      PP.fprintf fmt "((%a & %s) == %s)"
        RT.pp_expr x
        (Z.format "%#xuwb" m.m)
        (Z.format "%#xuwb" m.v)

  let append_bits (fmt : PP.formatter) (m : int) (n : int)  (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    let result_width = m + n in
    let x' fmt = zero_extend_bits fmt m result_width x in
    let y' fmt = zero_extend_bits fmt n result_width y in
    PP.fprintf fmt "((%a << %d) | %a)"
      RT.pp_expr x'
      n
      RT.pp_expr y'

  (* Replicate{m,n}(x : bits(m), n : integer) 
   *
   * This generates inline code that calculates the result in O(log2(n))
   * by ORing together the result of appending 2^p copies of x.
   *
   * Replicate{m,11}(x, 11)   (0d11 == 0b1101)
   * ==>
   * ({
   * bits(11*m) _tmp1 = (5*m)x;
   * bits(11*m) _tmp2 = (tmp1 << (1*m)) | tmp1;
   * bits(11*m) _tmp4 = (tmp2 << (2*m)) | tmp2;
   * bits(11*m) _tmp8 = (tmp4 << (4*m)) | tmp4;
   * 0 | tmp1 << (0*m) | _tmp4 << (1*m) | tmp8 << (5*m)
   * })
   *
   * This is a significant improvement over the more obvious O(n) algorithm
   * because n is often 8, 32 or even 512.
   *)
  let replicate_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    if n = 0 then begin
      empty_bits fmt ()
    end else begin
      let result_width = m * n in
      let n = Z.of_int n in
      let logn = Z.log2 n in

      (* calculate intermediate results *)
      PP.fprintf fmt "({ @[<v>%a __tmp1 = ((%a)%a);@,"
        ty_bits result_width
        ty_bits result_width
        RT.pp_expr x;
      for i = 1 to logn do
        PP.fprintf fmt "%a __tmp%d = (__tmp%d << %d) | __tmp%d;@,"
          ty_bits result_width
          (pow2 i)
          (pow2 (i-1))
          (m * pow2 (i-1))
          (pow2 (i-1))
      done;

      (* calculate final result *)
      zeros_bits fmt result_width;
      let shift = ref 0 in (* amount to shift next value by *)
      for i = 0 to logn do
        if Z.testbit n i then begin
          PP.fprintf fmt " | __tmp%d << %d" (pow2 i) !shift;
          shift := !shift + m * pow2 i
        end
      done;
      PP.fprintf fmt "; @]})"
    end

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

  let print_char (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "putchar(%a.to_int())" RT.pp_expr x

  let print_str (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "fputs(%a, stdout)" RT.pp_expr x

  (* Foreign Function Interface (FFI) *)
  let ffi_c2asl_integer_small (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)" ty_sint int_width RT.pp_expr x

  let ffi_asl2c_integer_small (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "%a.to_int64()" RT.pp_expr x

  let ffi_c2asl_bits_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    assert (List.mem n [8; 16; 32; 64]);
    PP.fprintf fmt "((%a) %a)"
      ty_bits n
      RT.pp_expr x

  let ffi_asl2c_bits_small (n : int) (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    assert (List.mem n [8; 16; 32; 64]);
    PP.fprintf fmt "((uint%d_t)(%a.to_uint64()))"
      n
      RT.pp_expr x

  let ffi_c2asl_bits_large (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    let num_limbs = (n + 31) / 32 in
    PP.fprintf fmt "%a %a;@,"
      ty_bits n
      RT.pp_expr x;
    PP.fprintf fmt "%a = ac::bit_fill<%a>((const int [%d]){"
      RT.pp_expr x
      ty_uint n
      num_limbs;
    for limb = num_limbs - 1 downto 0 do
      (* generate either "(int)(x[limb] >> 0)" or "(int)(x[limb] >> 32)" *)
      let shift_distance = 32 * (limb mod 2) in
      PP.fprintf fmt "(int)(%a[%d] >> %d)"
        RT.pp_expr y
        (limb / 2)
        shift_distance;
      if limb != 0 then PP.fprintf fmt ", "
    done;
    PP.fprintf fmt "});"

  let ffi_asl2c_bits_large (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    let array_size = (n + 63) / 64 in
    for limb = 0 to array_size - 1 do
      let slice_size = min 64 (n - limb * 64) in (* handle final slice *)
      PP.fprintf fmt "%a[%d] = %a.slc<%d>(%d).to_uint64();@,"
        RT.pp_expr x
        limb
        RT.pp_expr y
        slice_size
        (limb * 64)
    done

end

(****************************************************************
 * End
 ****************************************************************)
