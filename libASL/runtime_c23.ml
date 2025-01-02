(****************************************************************
 * Bit-precise runtime library support
 * This uses the C23 "Bit-precise integer" feature "_BitInt"
 *
 * Copyright (C) 2022-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to C runtime library support (fallback) *)

module PP = Format
module V = Value
module RT = Runtime
open Utils

module Runtime : RT.RuntimeLib = struct

  (* All definitions in the runtime library use the "ASL_" prefix *)
  let asl_keyword (fmt : PP.formatter) (s : string) : unit = PP.pp_print_string fmt ("ASL_" ^ s)

  let int_width = 128

  let max_sintN (n : int) = Z.sub (Z.shift_left Z.one (n-1)) Z.one
  let min_sintN (n : int) = Z.neg (Z.shift_left Z.one (n-1))

  (* signed and unsigned ints
   *
   * Note that "unsigned _BitInt(0)" and "signed _BitInt(1)" are
   * not supported by C so we have to use the next size up instead
   * and some of the operations that produce a bits(0) value have to make sure
   * that the value is always 0 (to make other operations that consume a bits(0)
   * value simpler).
   *
   * In addition, when calling external functions, all backends
   * need to pass values using the same representation so,
   * for simplicity, we use 'uint<n>_t' for bits(8|16|32|64)
   * and 'unsigned|signed __int128' for bits128 and integer
   *)
  let ty_sint (fmt : PP.formatter) (width : int) : unit =
    if width = 128 then
      PP.fprintf fmt "__int128"
    else if List.mem width [8; 16; 32; 64] then
      PP.fprintf fmt "int%d_t" width
    else
      PP.fprintf fmt "signed _BitInt(%d)" (max 2 width)

  let ty_uint (fmt : PP.formatter) (width : int) : unit =
    if width = 128 then
      PP.fprintf fmt "unsigned __int128"
    else if List.mem width [8; 16; 32; 64] then
      PP.fprintf fmt "uint%d_t" width
    else
      PP.fprintf fmt "unsigned _BitInt(%d)" (max 1 width)

  (* file header needed by this runtime variant *)
  let file_header : string list = [
      "#include <assert.h>";
      "#include <stdbool.h>";
      "#include <stdint.h>";
      "#ifndef ASL_C23";
      "#define ASL_C23";
      "#endif";
      "#include \"asl/runtime.h\"";
  ]

  (* Minimum sized signed int is 2 bits *)
  let ty_int (fmt : PP.formatter) : unit = ty_sint fmt (max 2 int_width)
  let ty_sintN (fmt : PP.formatter) (width : int) : unit = ty_sint fmt (max 2 width)
  (* Minimum sized signed int is 1 bit *)
  let ty_bits (fmt : PP.formatter) (width : int) : unit = ty_uint fmt (max 1 width)
  let ty_ram (fmt : PP.formatter) : unit = asl_keyword fmt "ram_t"

  let intN_literal (n : int) (fmt : PP.formatter) (x : Z.t) : unit =
    if Z.geq x Z.zero then
      PP.fprintf fmt "((%a)0x%swb)"
        ty_sint n
        (Z.format "%x" x)
    else if Z.equal x (min_sintN n) then
      PP.fprintf fmt "((%a)(-0x%swb) - 1wb)"
        ty_sint n
        (Z.format "%x" (Z.sub Z.minus_one x))
    else (* negative values *)
      PP.fprintf fmt "((%a)-0x%swb)"
        ty_sint n
        (Z.format "%x" (Z.neg x))

  let int_literal (fmt : PP.formatter) (x : Z.t) : unit = intN_literal int_width fmt x
  let sintN_literal (fmt : PP.formatter) (x : Primops.sintN) : unit = intN_literal x.n fmt x.v

  let empty_bits (fmt : PP.formatter) : unit = PP.pp_print_string fmt "0uwb"

  let bits_literal (fmt : PP.formatter) (x : Primops.bitvector) : unit =
    if x.n = 0 then
      empty_bits fmt
    else
      PP.fprintf fmt "((%a)0x%suwb)"
        ty_bits x.n
        (Z.format "%x" x.v)

  let unop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%s %a)"
      op
      RT.pp_expr x

  let binop (fmt : PP.formatter) (op : string) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a %s %a)"
      RT.pp_expr x
      op
      RT.pp_expr y

  (* signed sized integer functions *)
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
    PP.fprintf fmt "if (__tmp4 != 0 && (__tmp1 < 0 || __tmp2 < 0)) { __tmp3 = __tmp3 - 1; } ";
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
    PP.fprintf fmt "if (__tmp4 != 0 && (__tmp1 < 0 || __tmp2 < 0)) { __tmp4 = __tmp4 + __tmp2; } ";
    PP.fprintf fmt "__tmp4; })"

  let is_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "({ %a __tmp = %a; "
      ty_sint n
      RT.pp_expr x;
    PP.fprintf fmt "__tmp != 0 && (__tmp & (__tmp - 1)) == 0; })"

  let pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "(%a << %a)"
      (intN_literal n) Z.one
      RT.pp_expr x

  let align_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    (* x & (~((1 << y) - 1)) *)
    PP.fprintf fmt "(%a & (~((%a << %a) - %a)))"
      RT.pp_expr x
      (intN_literal n) Z.one
      RT.pp_expr y
      (intN_literal n) Z.one

  let mod_pow2_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) (y : RT.rt_expr) : unit =
    (* x & ((1 << y) - 1) *)
    PP.fprintf fmt "(%a & ((%a << %a) - %a))"
      RT.pp_expr x
      (intN_literal n) Z.one
      RT.pp_expr y
      (intN_literal n) Z.one

  let cvt_sintN_bits (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
    else
      PP.fprintf fmt "((%a)((%a)%a))"
        ty_uint n
        ty_uint m
        RT.pp_expr x

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

  let cvt_sintN_int (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint int_width
      RT.pp_expr x

  let cvt_int_sintN (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint n
      RT.pp_expr x

  let resize_sintN (fmt : PP.formatter) (m : int) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)%a)"
      ty_sint n
      RT.pp_expr x

  let print_sint64_decimal (fmt : PP.formatter) (n : int) (add_size : bool) (x : string) : unit =
    if add_size then begin
      PP.fprintf fmt "    if (%s == %a) {@," x (intN_literal n) (min_sintN n);
      PP.fprintf fmt "      printf(\"-i%d'd%s\");@," n (Z.to_string (Z.shift_left Z.one (n - 1)));
      PP.fprintf fmt "    } else {";
      PP.fprintf fmt "      if (%s < 0) {@," x;
      PP.fprintf fmt "        %s = -%s;@," x x;
      PP.fprintf fmt "        printf(\"-\");@,";
      PP.fprintf fmt "      }@,";
      PP.fprintf fmt "      printf(\"i%d'd%%ld\", (int64_t)%s);@," n x;
      PP.fprintf fmt "    }"
    end else begin
      PP.fprintf fmt "    printf(\"%%ld\", (int64_t)%s);@," x
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
          (intN_literal 64) (min_sintN 63)
          (intN_literal 64) (max_sintN 63);
        print_sint64_decimal fmt n add_size "__tmp";
        PP.fprintf fmt "  } else {@,";
        PP.fprintf fmt "    if (__tmp < 0) {@,";
        PP.fprintf fmt "      __tmp = -__tmp;@,";
        PP.fprintf fmt "      printf(\"-\");@,";
        PP.fprintf fmt "    }@,";
        (if add_size then PP.fprintf fmt "    printf(\"%d'x\");@," n
                     else PP.fprintf fmt "    printf(\"0x\");@,");
        PP.fprintf fmt "    bool leading = true;@,";
        PP.fprintf fmt "    for(int i = (%d-1)&~3; i >= 0; i -= 4) {@," n;
        PP.fprintf fmt "      unsigned c = (__tmp >> i) & 15;@,";
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
    PP.fprintf fmt "  if (__tmp < 0) {@,";
    PP.fprintf fmt "    __tmp = -__tmp;@,";
    PP.fprintf fmt "    printf(\"-\");@,";
    PP.fprintf fmt "  }@,";
    (if add_size then PP.fprintf fmt "      printf(\"i%d'x\");@," n
                 else PP.fprintf fmt "      printf(\"0x\");@,");
    PP.fprintf fmt "  bool leading = true;@,";
    PP.fprintf fmt "  for(int i = (%d-1)&~3; i >= 0; i -= 4) {@," n;
    PP.fprintf fmt "    unsigned c = (__tmp >> i) & 15;@,";
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

  let zeros_bits (fmt : PP.formatter) (n : int) : unit =
    bits_literal fmt (Primops.mkBits n Z.zero)

  let ones_bits (fmt : PP.formatter) (n : int) : unit =
    let x = Z.sub (Z.shift_left Z.one n) Z.one in
    bits_literal fmt (Primops.mkBits n x)

  let mk_mask (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    if n = 0 then
      (* Although we return empty_bits, we may still need to execute 'x' for any side effects *)
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
    else
      PP.fprintf fmt "(%a >> (%d - %a))"
        ones_bits n
        n
        RT.pp_expr x

  let get_slice (fmt : PP.formatter) (n : int) (w : int) (x : RT.rt_expr) (i : RT.rt_expr) : unit =
    PP.fprintf fmt "((%a)(%a >> %a))"
      ty_bits w
      RT.pp_expr x
      RT.pp_expr i

  let set_slice (fmt : PP.formatter) (n : int) (w : int) (l : RT.rt_expr) (i : RT.rt_expr) (r : RT.rt_expr) : unit =
    PP.fprintf fmt "{ %a __index = %a; "
      ty_uint int_width
      RT.pp_expr i;
    PP.fprintf fmt "%a __mask = %a >> %d; "
      ty_bits n
      ones_bits n
      (n - w);
    PP.fprintf fmt "%a = (%a & ~(__mask << __index)) | (((%a)%a) << __index);"
      RT.pp_expr l
      RT.pp_expr l
      ty_bits n
      RT.pp_expr r;
    PP.fprintf fmt " }"

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
      PP.fprintf fmt "({ (void)%a; 0uwb; })" RT.pp_expr x
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
      PP.fprintf fmt "  long long chunk = __tmp >> (64 * i);@,";
      PP.fprintf fmt "  if (leading) {@,";
      PP.fprintf fmt "    if (i == 0 || chunk) {@,";
      PP.fprintf fmt "      printf(\"%%llx\", chunk);@,";
      PP.fprintf fmt "      leading = false;@,";
      PP.fprintf fmt "    }@,";
      PP.fprintf fmt "  } else {@,";
      PP.fprintf fmt "    printf(\"%%08llx\", chunk);@,";
      PP.fprintf fmt "  }@,";
      PP.fprintf fmt "}@,";
      PP.fprintf fmt "@]}"
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
      empty_bits fmt
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
    PP.fprintf fmt "putchar(%a)" RT.pp_expr x

  let print_str (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "fputs(%a, stdout)" RT.pp_expr x

  (* Foreign Function Interface (FFI) *)
  let ffi_integer_to_c_int (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "%a" RT.pp_expr x

  let ffi_integer_to_c_sint64 (fmt : PP.formatter) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((int64_t) %a)" RT.pp_expr x

  let ffi_bits_to_c_uint64 (fmt : PP.formatter) (n : int) (x : RT.rt_expr) : unit =
    PP.fprintf fmt "((uint64_t) %a)" RT.pp_expr x
end

(****************************************************************
 * End
 ****************************************************************)
