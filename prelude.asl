////////////////////////////////////////////////////////////////
// ASL standard prelude
//
// Copyright Arm Limited (c) 2017-2019
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

__builtin type real;
__builtin type string;
__builtin type __mask; // todo: should have a type parameter
__builtin type __RAM; // todo: should have a type parameter

type bit of bits(1);

enumeration boolean { FALSE, TRUE };
enumeration signal { LOW, HIGH };

__builtin func asl_eq_bool(x : boolean, y : boolean) => boolean;
__builtin func asl_ne_bool(x : boolean, y : boolean) => boolean;
__builtin func asl_not_bool(x : boolean) => boolean;
__builtin func asl_and_bool(x : boolean, y : boolean) => boolean;
__builtin func asl_or_bool(x : boolean, y : boolean) => boolean;
__builtin func asl_equiv_bool(x : boolean, y : boolean) => boolean;
__builtin func asl_implies_bool(x : boolean, y : boolean) => boolean;

__builtin func asl_eq_int(x : integer, y : integer) => boolean;
__builtin func asl_ne_int(x : integer, y : integer) => boolean;
__builtin func asl_gt_int(x : integer, y : integer) => boolean;
__builtin func asl_ge_int(x : integer, y : integer) => boolean;
__builtin func asl_le_int(x : integer, y : integer) => boolean;
__builtin func asl_lt_int(x : integer, y : integer) => boolean;
__builtin func asl_is_pow2_int(x : integer) => boolean;
__builtin func asl_add_int(x : integer, y : integer) => integer;
__builtin func asl_neg_int(x : integer) => integer;
__builtin func asl_sub_int(x : integer, y : integer) => integer;
__builtin func asl_shl_int(x : integer, y : integer) => integer;
__builtin func asl_shr_int(x : integer, y : integer) => integer;
__builtin func asl_mul_int(x : integer, y : integer) => integer;
__builtin func asl_exact_div_int(x : integer, y : integer) => integer;
__builtin func asl_zdiv_int(x : integer, y : integer) => integer;
__builtin func asl_zrem_int(x : integer, y : integer) => integer;
__builtin func asl_fdiv_int(x : integer, y : integer) => integer;
__builtin func asl_frem_int(x : integer, y : integer) => integer;
__builtin func asl_mod_pow2_int(x : integer, y : integer) => integer;
__builtin func asl_align_int(x : integer, y : integer) => integer;
__builtin func asl_pow2_int(y : integer) => integer;

__builtin func asl_cvt_int_real(x : integer) => real;
__builtin func asl_eq_real(x : real, y : real) => boolean;
__builtin func asl_ne_real(x : real, y : real) => boolean;
__builtin func asl_le_real(x : real, y : real) => boolean;
__builtin func asl_lt_real(x : real, y : real) => boolean;
__builtin func asl_gt_real(x : real, y : real) => boolean;
__builtin func asl_ge_real(x : real, y : real) => boolean;
__builtin func asl_add_real(x : real,    y : real) => real;
__builtin func asl_neg_real(x : real) => real;
__builtin func asl_sub_real(x : real, y : real) => real;
__builtin func asl_mul_real(x : real, y : real) => real;
__builtin func asl_divide_real(x : real, y : real) => real;
__builtin func asl_pow2_real(y : integer) => real;
__builtin func asl_round_tozero_real(x : real) => integer;
__builtin func asl_round_down_real(x : real) => integer;
__builtin func asl_round_up_real(x : real) => integer;
__builtin func asl_sqrt_real(x : real) => real;

__builtin func asl_cvt_int_bits(x : integer, N : integer) => bits(N);
__builtin func asl_cvt_bits_sint{N}(x : bits(N)) => integer;
__builtin func asl_cvt_bits_uint{N}(x : bits(N)) => integer;
__builtin func asl_in_mask{N}(x : bits(N), y : __mask(N)) => boolean;
__builtin func asl_notin_mask{N}(x : bits(N), y : __mask(N)) => boolean;
__builtin func asl_eq_bits{N}(x : bits(N), y : bits(N)) => boolean;
__builtin func asl_ne_bits{N}(x : bits(N), y : bits(N)) => boolean;
__builtin func asl_add_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_sub_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_mul_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_and_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_or_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_xor_bits{N}(x : bits(N), y : bits(N)) => bits(N);
__builtin func asl_not_bits{N}(x : bits(N)) => bits(N);
__builtin func asl_zeros_bits(N : integer) => bits(N);
__builtin func asl_ones_bits(N : integer) => bits(N);
__builtin func asl_lsl_bits{N}(x : bits(N), i : integer) => bits(N);
__builtin func asl_lsr_bits{N}(x : bits(N), i : integer) => bits(N);
__builtin func asl_asr_bits{N}(x : bits(N), i : integer) => bits(N);

// Construct 'ZeroExtend(Ones(w), N)'
// e.g. mk_mask(3, 8) == '00000 111'
// This is used in the bitmask lowering transformation
__builtin func asl_mk_mask(w : integer, N : integer) => bits(N);

func asl_add_bits_int{N}(x : bits(N), y : integer) => bits(N)
begin
    return asl_add_bits(x, asl_cvt_int_bits(y, N));
end

func asl_sub_bits_int{N}(x : bits(N), y : integer) => bits(N)
begin
    return asl_sub_bits(x, asl_cvt_int_bits(y, N));
end

func asl_mul_bits_int{N}(x : bits(N), y : integer) => bits(N)
begin
    return asl_mul_bits(x, asl_cvt_int_bits(y, N));
end

// Bit slice helper functions used in some backends
func asl_extract_bits(x : bits(N), lo : integer, W : integer) => bits(W)
begin
    return x[lo +: W];
end

// Bit slice helper functions used in some backends
func asl_bits_set(x : bits(N), lo : integer, v : bits(W)) => bits(N)
begin
    var y = x;
    y[lo +: W] = v;
    return y;
end

__operator2 + = asl_add_int, asl_add_real, asl_add_bits, asl_add_bits_int;
__operator2 - = asl_sub_int, asl_sub_real, asl_sub_bits, asl_sub_bits_int;
__operator1 - = asl_neg_int, asl_neg_real;
__operator2 * = asl_mul_int, asl_mul_real, asl_mul_bits, asl_mul_bits_int;
__operator2 / = asl_divide_real;

__builtin func asl_replicate_bits{M}(x : bits(M), N : integer) => bits(M*N);
__builtin func asl_append_bits{M, N}(x : bits(M), y : bits(N)) => bits(M+N);
__builtin func asl_zero_extend_bits{M}(x : bits(M), N : integer) => bits(N);
__builtin func asl_sign_extend_bits{M}(x : bits(M), N : integer) => bits(N);

__builtin func asl_eq_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_ne_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_gt_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_ge_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_le_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_lt_sintN(x : __sint(N), y : __sint(N)) => boolean;
__builtin func asl_add_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_neg_sintN(x : __sint(N)) => __sint(N);
__builtin func asl_sub_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_shl_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_shr_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_mul_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_exact_div_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_zdiv_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_zrem_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_fdiv_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_frem_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_is_pow2_sintN(x : __sint(N)) => boolean;
__builtin func asl_pow2_sintN(x : __sint(N)) => __sint(N);
__builtin func asl_align_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_mod_pow2_sintN(x : __sint(N), y : __sint(N)) => __sint(N);
__builtin func asl_cvt_sintN_bits(x : __sint(M), N : integer) => bits(N);
__builtin func asl_cvt_bits_ssintN(x : bits(N)) => __sint(N);
__builtin func asl_cvt_bits_usintN(x : bits(N)) => __sint(N+1);
__builtin func asl_cvt_sintN_int(x : __sint(N)) => integer;
__builtin func asl_cvt_int_sintN(x : integer, N : integer) => __sint(N);
__builtin func asl_resize_sintN(x : __sint(M), N : integer) => __sint(N);
__builtin func print_sintN_hex(x : __sint(N)) => ();
__builtin func print_sintN_dec(x : __sint(N)) => ();

__operator1 - = asl_neg_sintN;

__builtin func asl_cvt_int_hexstr(x : integer) => string;
__builtin func asl_cvt_int_decstr(x : integer) => string;
__builtin func asl_cvt_bool_str(x : boolean) => string;
__builtin func asl_cvt_bits_str(N : integer, x : bits(N)) => string;
__builtin func asl_cvt_real_str(x : real) => string;
__builtin func asl_append_str_str(x : string, y : string) => string;
__builtin func asl_eq_str(x : string, y : string) => boolean;
__builtin func asl_ne_str(x : string, y : string) => boolean;
__builtin func print_str(x : string) => ();
__builtin func print_char(x : integer) => ();
__builtin func print_int_hex(x : integer) => ();
__builtin func print_int_dec(x : integer) => ();
__builtin func print_bits_hex(x : bits(N)) => ();

__builtin func pragma(x : string) => ();

__builtin func asl_file_open(name : string, mode : string) => integer;
__builtin func asl_file_write(fd : integer, data : string) => integer;
__builtin func asl_file_getc(fd : integer) => integer;

__builtin func asl_ram_init(A : integer, ram : __RAM(A), val : bits(64)) => ();
__builtin func asl_ram_read(A : integer, N : integer, ram : __RAM(A), address : bits(A)) => bits(8*N);
__builtin func asl_ram_write(A : integer, N : integer, ram : __RAM(A), address : bits(A), val : bits(8*N)) => ();


// backwards compatibility layer until the specs are updated to use the new asl_ prefix versions
// uses the bottom 64 bits of val to initialize memory
func ram_init(A : integer, N : integer, ram : __RAM(A), val : bits(8*N))
begin
    if asl_gt_int(N, 8) then
        asl_ram_init(A, ram, val[0 +: 64]);
    else
        asl_ram_init(A, ram, asl_replicate_bits(val, 8)[0 +: 64]);
    end
end

// backwards compatibility layer until the specs are updated to use the new asl_ prefix versions
func ram_read(A : integer, N : integer, ram : __RAM(A), address : bits(A)) => bits(8*N)
begin
    return asl_ram_read(A, N, ram, address);
end

// backwards compatibility layer until the specs are updated to use the new asl_ prefix versions
func ram_write(A : integer, N : integer, ram : __RAM(A), address : bits(A), val : bits(8*N))
begin
    asl_ram_write(A, N, ram, address, val);
end

func __InitRAM(A : integer, N : integer, ram : __RAM(A), val : bits(8*N))
begin
    if asl_gt_int(N, 8) then
        asl_ram_init(A, ram, val[0 +: 64]);
    else
        asl_ram_init(A, ram, asl_replicate_bits(val, 8)[0 +: 64]);
    end
end

func __ReadRAM(A : integer, N : integer, ram : __RAM(A), address : bits(A)) => bits(8*N)
begin
    return asl_ram_read(A, N, ram, address);
end

func __WriteRAM(A : integer, N : integer, ram : __RAM(A), address : bits(A), val : bits(8*N))
begin
    asl_ram_write(A, N, ram, address, val);
end

// Advance trace to next instruction
__builtin func __TraceNext() => ();

// Trace a read/write of data/instruction from a physical memory address
__builtin func __TracePhysicalMemory
    (is_read : boolean, is_data : boolean, PA : integer, N : integer,
    physical_address : bits(PA), val : bits(N)
    ) => ();

// Trace a read/write of data/instruction from a virtual memory address
// providing both the context ID and the physical address that the access maps to.
__builtin func __TraceVirtualMemory
    (is_read : boolean, is_data : boolean, VA : integer, PA : integer, N : integer,
    context : bits(C), virtual_address : bits(VA), physical_address : bits(PA), val : bits(N)
    ) => ();

// Trace a read of a page table entry from a physical address
// providing both the context ID and the level of this entry in the page table tree.
__builtin func __TracePageTableWalk
    (PA : integer, N : integer,
    context : bits(C), level : integer,
    physical_address : bits(PA), val : bits(N)
    ) => ();

// Emit an error message to trace
__builtin func __TraceError(kind : string, event : string) => ();

// Emit an informational memory to trace
__builtin func __TraceEvent(kind : string, event : string) => ();

func putchar(c : integer)
begin
    print_char(c);
end

__operator1 !       = asl_not_bool;
__operator2 &&      = asl_and_bool;
__operator2 ||      = asl_or_bool;
__operator2 <->     = asl_equiv_bool;
__operator2 -->     = asl_implies_bool;

// omit since they are auto-generated
// __operator2 == = eq_bool;
// __operator2 != = ne_bool;

__operator2 == = asl_eq_int, asl_eq_real, asl_eq_bits, asl_eq_str, asl_in_mask;
__operator2 != = asl_ne_int, asl_ne_real, asl_ne_bits, asl_ne_str, asl_notin_mask;
__operator2 <= = asl_le_int, asl_le_real;
__operator2 >= = asl_ge_int, asl_ge_real;
__operator2 <  = asl_lt_int, asl_lt_real;
__operator2 >  = asl_gt_int, asl_gt_real;

__operator2 << = asl_shl_int;
__operator2 >> = asl_shr_int;

func IsPowerOfTwo(x : integer) => boolean
begin
    return asl_is_pow2_int(x);
end

func asl_pow_int_int(x : integer, y : integer) => integer
begin
    if x == 2 then
        return asl_pow2_int(y); // optimized case
    else
        assert y >= 0;
        var result : integer = 1;
        for i = 1 to y do
            result = result * x;
        end
        return result;
    end
end

func asl_pow_real_int(x : real, y : integer) => real
begin
    assert x == 2.0;
    return asl_pow2_real(y);
end

__operator2 ^ = asl_pow_int_int, asl_pow_real_int;

func asl_frem_bits_int{N}(x : bits(N), y : integer) => integer
begin
    assert y > 0;
    return asl_frem_int(asl_cvt_bits_uint(x), y);
end

// Division: error if division is not exact
__operator2 DIV   = asl_exact_div_int;

// Division: round to zero
__operator2 QUOT  = asl_zdiv_int;
__operator2 REM   = asl_zrem_int;

// Division: round to -infinity (floor)
__operator2 DIVRM = asl_fdiv_int;
__operator2 MOD   = asl_frem_int, asl_frem_bits_int;

__operator2 AND   = asl_and_bits;
__operator2 OR    = asl_or_bits;
__operator2 EOR   = asl_xor_bits; // deprecated synonym for XOR
__operator2 XOR   = asl_xor_bits;
__operator1 NOT   = asl_not_bits;

func asl_append_str_bool(x : string, y : boolean) => string
begin
    return asl_append_str_str(x, asl_cvt_bool_str(y));
end

func asl_append_bool_str(x : boolean, y : string) => string
begin
    return asl_append_str_str(asl_cvt_bool_str(x), y);
end

func asl_append_str_bits{N}(x : string, y : bits(N)) => string
begin
    return asl_append_str_str(x, asl_cvt_bits_str(N, y));
end

func asl_append_bits_str{N}(x : bits(N), y : string) => string
begin
    return asl_append_str_str(asl_cvt_bits_str(N, x), y);
end

func asl_append_str_real(x : string, y : real) => string
begin
    return asl_append_str_str(x, asl_cvt_real_str(y));
end

func asl_append_real_str(x : real, y : string) => string
begin
    return asl_append_str_str(asl_cvt_real_str(x), y);
end

func asl_append_str_int(x : string, y : integer) => string
begin
    return asl_append_str_str(x, asl_cvt_int_decstr(y));
end

func asl_append_int_str(x : integer, y : string) => string
begin
    return asl_append_str_str(asl_cvt_int_decstr(x), y);
end

__operator2 ++ = asl_append_str_str;
__operator2 ++ = asl_append_str_bool, asl_append_bool_str;
__operator2 ++ = asl_append_str_real, asl_append_real_str;
__operator2 ++ = asl_append_str_bits, asl_append_bits_str;
__operator2 ++ = asl_append_str_int,  asl_append_int_str;

////////////////////////////////////////////////////////////////
// 9.1 Standard integer functions and procedures
////////////////////////////////////////////////////////////////

// Absolute value of an integer.
func Abs(x : integer) => integer
begin
    return if x >= 0 then x else -x;
end

// Convert a bitvector to an unsigned integer, where bit 0 is LSB.
// This is the recommended way to convert a bit vector to an integer.
func UInt{N}(x : bits(N)) => integer {0 .. 2^N-1}
begin
    return asl_cvt_bits_uint(x);
end

// Convert a 2s complement bitvector to a signed integer.
func SInt{N}(x : bits(N)) => integer {-(2^(N-1)) .. 2^(N-1)-1}
begin
    return asl_cvt_bits_sint(x);
end

// Maximum of two integers.
func Max(a : integer, b : integer) => integer
begin
    return if a >= b then a else b;
end

// Minimum of two integers.
func Min(a : integer, b : integer) => integer
begin
    return if a <= b then a else b;
end

// Calculate the logarithm base 2 of the input. Input must be a power of 2.
func Log2(a : integer) => integer
begin
    assert IsPowerOfTwo(a);
    var b = a;
    var r = 0;
    while b > 1 do
       b = b DIV 2;
       r = r + 1;
    end
    return r;
end

// align down to nearest multiple of 2^y
func AlignDown(x : integer, y : integer) => integer
begin
    return asl_align_int(x, y);
end

// align up to nearest multiple of 2^y
func AlignUp(x : integer, y : integer) => integer
begin
    return asl_align_int(x + 2^y - 1, y);
end

// test whether x is even
func IsEven(x : integer) => boolean
begin
    return x[0] == '0';
end

// test whether x is odd
func IsOdd(x : integer) => boolean
begin
    return x[0] == '1';
end

////////////////////////////////////////////////////////////////
// 9.2 Standard real functions and procedures
////////////////////////////////////////////////////////////////

// Convert integer to rational value.
func Real(x : integer) => real
begin
    return asl_cvt_int_real(x);
end

// Nearest integer, rounding towards negative infinity.
func RoundDown(x : real) => integer
begin
    return asl_round_down_real(x);
end

// Nearest integer, rounding towards positive infinity.
func RoundUp(x : real) => integer
begin
    return asl_round_up_real(x);
end

// Nearest integer, rounding towards zero.
func RoundTowardsZero(x : real) => integer
begin
    return asl_round_tozero_real(x);
end

// Absolute value.
func Abs(x : real) => real
begin
    return if x >= 0.0 then x else -x;
end

// Maximum of reals.
func Max(a : real, b : real) => real
begin
    return if a >= b then a else b;
end

// Minimum of reals.
func Min(a : real, b : real) => real
begin
    return if a <= b then a else b;
end

func Sqrt(x : real) => real
begin
    return asl_sqrt_real(x);
end

////////////////////////////////////////////////////////////////
// 9.3 Standard bitvector functions and procedures
////////////////////////////////////////////////////////////////

// Return the concatenation of 1 or more copies of a bitvector.
func Replicate{M}(x : bits(M), N : integer) => bits(M*N)
begin
    return asl_replicate_bits(x, N);
end

// Return a bitvector consisting entirely of N '0' bits.
func Zeros(N : integer) => bits(N)
begin
    return asl_zeros_bits(N);
end

// Return a bitvector consisting entirely of '1' bits.
func Ones(N : integer) => bits(N)
begin
    return asl_ones_bits(N);
end

// Return true if bitvector consists entirely of '0' bits.
func IsZero{N}(x : bits(N)) => boolean
begin
    return x == Zeros(N);
end

// Return true if bitvector consists entirely of '1' bits.
func IsOnes{N}(x : bits(N)) => boolean
begin
    return x == Ones(N);
end

// Zero-extend a bitvector to the same or a wider width.
func ZeroExtend{M}(x : bits(M), N : integer) => bits(N)
begin
    assert N >= M;
    return asl_zero_extend_bits(x, N);
end

// Sign-extend a bitvector (treated as 2s complement) to the same or a wider width.
func SignExtend{M}(x : bits(M), N : integer) => bits(N)
begin
    assert N >= M;
    return asl_sign_extend_bits(x, N);
end

// Extend a bitvector to a specified width, treating as signed or unsigned.
// The output width might be narrower than the input, in which case the
// function is equivalent to a bit slice.
func Extend{M}(x : bits(M), N : integer, unsigned : boolean) => bits(N)
begin
    assert N >= M;
    return (if unsigned then ZeroExtend(x, N) else SignExtend(x, N));
end

// Return the width of a bitvector argument, without regard to its value.
func Len{N}(x : bits(N)) => integer {N}
begin
    return N;
end

func SignedSat(x : integer, N : integer) => bits(N)
begin
    let r = if x >= 2^(N-1) then 2^(N-1) - 1
            elsif x < - 2^(N-1) then - 2^(N-1)
            else x;
    return r[0 +: N];
end

func UnsignedSat(x : integer, N : integer) => bits(N)
begin
    let r = if x >= 2^N then 2^N - 1
            elsif x < 0 then 0
            else x;
    return r[0 +: N];
end

func Sat(x : integer, N : integer, unsigned : boolean) => bits(N)
begin
    return (if unsigned then UnsignedSat(x, N) else SignedSat(x, N));
end

// Count the number of 1 bits in a bitvector.
func BitCount(x : bits(N)) => integer {0 .. N}
begin
    var result : integer {0 .. N} = 0;
    for i = 0 to N-1 do
        if x[i] == '1' then
            result = (result + 1) as {0 .. N};
        end
    end
    return result;
end

// Position of the lowest 1 bit in a bitvector.
// If the bitvector is entirely zero, return the width.
func LowestSetBit(x : bits(N)) => integer {0 .. N}
begin
    for i = 0 to N-1 do
        if x[i] == '1' then
            return i as {0..N};
        end
    end
    return N;
end

// Position of the highest 1 bit in a bitvector.
// If the bitvector is entirely zero, return -1
func HighestSetBit(x : bits(N)) => integer {-1 .. N-1}
begin
    for i = N-1 downto 0 do
        if x[i] == '1' then
            return i as {0..N-1};
        end
    end
    return -1;
end

// Leading zero bits in a bitvector.
func CountLeadingZeroBits(x : bits(N)) => integer {0 .. N}
begin
    return N - 1 - HighestSetBit(x);
end

// Leading sign bits in a bitvector. Count the number of consecutive
// bits following the leading bit, that are equal to it.
func CountLeadingSignBits(x : bits(N)) => integer {0 .. N}
begin
    return CountLeadingZeroBits(x[N-1:1] XOR x[N-2:0]);
end

// Treating input as an integer, align down to nearest multiple of 2^y.
func AlignDown{N}(x : bits(N), y : integer) => bits(N)
begin
    var result = x;
    result[y-1:0] = Zeros(y);
    return result;
end

// Treating input as an integer, align up to nearest multiple of 2^y.
// Returns zero if the result is not representable in N bits.
func AlignUp{N}(x : bits(N), y : integer) => bits(N)
begin
    if IsZero(x[y-1:0]) then
        return x;
    else
        return [x[N-1 : y]+1, Zeros(y)];
    end
end

// Logical left shift
func ShiftLeft(x : bits(N), distance : integer) => bits(N)
begin
    assert distance IN {0 .. N-1};
    return asl_lsl_bits(x, distance);
end

// Logical right shift, shifting zeroes into higher bits.
func ShiftRightLogical(x : bits(N), distance : integer) => bits(N)
begin
    assert distance IN {0 .. N-1};
    return asl_lsr_bits(x, distance);
end

// Arithmetic right shift, shifting sign bits into higher bits.
func ShiftRightArithmetic(x : bits(N), distance : integer) => bits(N)
begin
    assert distance IN {0 .. N-1};
    return asl_asr_bits(x, distance);
end

func RotateLeft(x : bits(N), distance : integer) => bits(N)
begin
    assert distance IN {0 .. N-1};
    return [x,x][(N-distance) +: N];
end

func RotateRight(x : bits(N), distance : integer) => bits(N)
begin
    assert distance IN {0 .. N-1};
    return [x,x][distance +: N];
end

func IsParityEven(x : bits(N)) => boolean
begin
    var r : bit = '0';
    for i = 0 to N - 1 do
        r = r XOR x[i];
    end
    return r == '0';
end

func IsParityOdd(x : bits(N)) => boolean
begin
    var r : bit = '0';
    for i = 0 to N - 1 do
        r = r XOR x[i];
    end
    return r == '1';
end

////////////////////////////////////////////////////////////////
// 9.5 Other functions and procedures
////////////////////////////////////////////////////////////////

// Print one or more arguments, to an implementation defined output channel.
// This function is provided for diagnostics and does not form part of an architectural specification.
func print{N}(x : bits(N))
begin
    print_bits_hex(x);
end

func print(x : string)
begin
    print_str(x);
end

func print(x : boolean)
begin
    if x then
        print("TRUE");
    else
        print("FALSE");
    end
end

func print(x : integer)
begin
    print_int_hex(x);
end

func println()
begin
    print_char(10);
end

func println(x : string)
begin
    print_str(x);
    print_char(10);
end

// Convert an integer to a decimal string, prefixing with '-' if negative.
func DecStr(x : integer) => string
begin
    return asl_cvt_int_decstr(x);
end

func DecStr(x : bits(N)) => string
begin
    return DecStr(asl_cvt_bits_uint(x));
end

// Convert an integer to a hexadecimal string, prefixing with '-' if negative.
// The exact format of the string is implementation defined.
func HexStr(x : integer) => string
begin
    return asl_cvt_int_hexstr(x);
end

func HexStr(x : bits(N)) => string
begin
    return HexStr(asl_cvt_bits_uint(x));
end

// Unreachable() is used to indicate that part of a subprogram should be unreachable.
// This can be used to guarantee termination of subprograms on error conditions.
func Unreachable()
begin
    println("Unreachable() function called.");
    println("This should be impossible - report a bug in the specification");
    assert FALSE;
end

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
