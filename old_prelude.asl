////////////////////////////////////////////////////////////////
// ASL standard prelude
// This is provided as a temporary measure until existing specs have been converted
//
// Copyright Arm Limited (c) 2017-2019
// Copyright (C) 2022-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

use String as string; // deprecated

use Bit as bit; // deprecated

use Boolean as boolean; // deprecated
use True as TRUE;
use False as FALSE;

__operator1 !       = Std::Boolean::Not;
__operator2 &&      = Std::Boolean::Lazy_And;
__operator2 ||      = Std::Boolean::Lazy_Or;

// backwards compatibility: deprecated
use Std::Bits::All_Ones as Ones;
use Std::Bits::Count_Set_Bits as BitCount;
use Std::Bits::Count_Leading_Sign_Bits as CountLeadingSignBits;
use Std::Bits::Count_Leading_Zero_Bits as CountLeadingZeroBits;
use Std::Bits::Count_Set_Bits as CountSetBits;
use Std::Bits::Count_Trailing_Zero_Bits as CountTrailingZeroBits;
use Std::Bits::Is_All_Ones as IsOnes;
use Std::Bits::Is_Parity_Even as IsParityEven;
use Std::Bits::Is_Parity_Odd as IsParityOdd;
use Std::Bits::Is_Zero as IsZero;
use Std::Bits::Length as Len;
use Std::Bits::Repeat as Replicate;
use Std::Bits::Rotate_Left as RotateLeft;
use Std::Bits::Rotate_Right as RotateRight;
use Std::Bits::Saturate as Saturate;
use Std::Bits::Shift_Left_Logical as ShiftLeft;
use Std::Bits::Shift_Right_Arithmetic as ShiftRightArithmetic;
use Std::Bits::Shift_Right_Logical as ShiftRightLogical;
use Std::Bits::Sign_Extend as SignExtend;
use Std::Bits::Signed as SInt;
use Std::Bits::Signed as SInt;
use Std::Bits::Signed_Saturate as SignedSat;
use Std::Bits::Unsigned as UInt;
use Std::Bits::Unsigned as UInt;
use Std::Bits::Unsigned_Saturate as UnsignedSat;
use Std::Bits::Zero as Zeros;
use Std::Bits::Zero_Extend as ZeroExtend;
use Std::Integer::Is_Even as IsEven;
use Std::Integer::Is_Odd as IsOdd;
use Std::Integer::Is_Power_Of_Two as IsPowerOfTwo;
use Std::Integer::Truncated_Divide as Truncated_Divide;
use Std::Integer::Truncated_Remainder as Truncated_Remainder;
use Std::Unreachable as Unreachable;

// Can't use this style of renaming on overloaded function names
// use Std::Bits::Align_Down as AlignDown;
// use Std::Bits::Align_Up as AlignUp;
// use Std::Integer::Align_Down as AlignDown;
// use Std::Integer::Align_Up as AlignUp;

// deprecated
// align down to nearest multiple of 2**y
function AlignDown(x : Integer, y : Integer) -> Integer
begin
    return Std::Integer::Align_Down(x, 2**y);
end

// deprecated
// align up to nearest multiple of 2**y
function AlignUp(x : Integer, y : Integer) -> Integer
begin
    return Std::Integer::Align_Up(x, 2**y);
end

function AlignDown(x : Bits(n), y : Integer) -> Bits(n)
begin
    return Std::Bits::Align_Down(x, 2**y);
end

// deprecated
// Treating input as an Integer, align up to nearest multiple of 2**y.
// Returns zero if the result is not representable in n Bits.
function AlignUp(x : Bits(n), y : Integer) -> Bits(n)
begin
    return Std::Bits::Align_Up(x, 2**y);
end


// all division operators are deprecated
// Division: error if division is not exact (* deprecated *)
__builtin function builtin_exact_div_int(left : Integer, right : Integer) -> Integer;
__builtin function builtin_exact_div_sintN(left : __sint(n), right : __sint(n)) -> __sint(n);

// todo: deprecated
function Std::Bits::Floor_Remainder(implicit size : Integer, left : Bits(size), right : Integer) -> Integer
begin
    assert right > 0;
    return Std::Integer::Floor_Remainder(Std::Bits::Unsigned(left), right);
end

__operator2 DIV   = builtin_exact_div_int;

// Division: round to zero
__operator2 QUOT  = Std::Integer::Truncated_Divide;
__operator2 REM   = Std::Integer::Truncated_Remainder;

// Division: round to -infinity (floor)
__operator2 DIVRM = Std::Integer::Floor_Divide;
__operator2 MOD   = Std::Integer::Floor_Remainder, Std::Bits::Floor_Remainder;

// deprecated
function LowestSetBit(implicit size : Integer, x : Bits(size)) -> {0 .. size}
begin
    return Std::Bits::Count_Trailing_Zero_Bits(x);
end

// deprecated
function HighestSetBit(implicit size : Integer, x : Bits(size)) -> {-1 .. size-1}
begin
    return (size - Std::Bits::Count_Leading_Zero_Bits(x) - 1) as {-1..size-1};
end

////////////////////////////////////////////////////////////////
// Deprecated string functions
////////////////////////////////////////////////////////////////

function DecStr(x : Integer) -> String
begin
    return "DecStr is deprecated: use print functions instead";
end

function HexStr(x : Integer) -> String
begin
    return "HexStr is deprecated: use print functions instead";
end

function HexStr(x : Bits(n)) -> String
begin
    return "HexStr is deprecated: use print functions instead";
end

function deprecated_boolean_string_append(x : Boolean, y : String) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_string_boolean_append(x : String, y : Boolean) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_bits_string_append(x : Bits(n), y : String) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_string_bits_append(x : String, y : Bits(n)) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_int_string_append(x : Integer, y : String) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_string_int_append(x : String, y : Integer) -> String
begin
    return "String append is deprecated: use print functions instead";
end

function deprecated_string_string_append(x : String, y : String) -> String
begin
    return "String append is deprecated: use print functions instead";
end

__operator2 ++ = deprecated_string_boolean_append;
__operator2 ++ = deprecated_boolean_string_append;
__operator2 ++ = deprecated_string_bits_append;
__operator2 ++ = deprecated_bits_string_append;
__operator2 ++ = deprecated_string_int_append;
__operator2 ++ = deprecated_int_string_append;
__operator2 ++ = deprecated_string_string_append;


////////////////////////////////////////////////////////////////
// I/O functions
////////////////////////////////////////////////////////////////

use Std::Print::Bits::Hex    as print_bits_hex;
use Std::Print::Boolean      as builtin_print_bool;
use Std::Print::Char         as print_char;
use Std::Print::Integer::Dec as print_int_dec;
use Std::Print::Integer::Hex as print_int_hex;
use Std::Print::SInt::Dec    as print_sintN_dec;
use Std::Print::SInt::Hex    as print_sintN_hex;
use Std::Print::String       as print_str;

// Print one or more arguments, to an implementation defined output channel.
// This function is provided for diagnostics and does not form part of an architectural specification.
function print(x : Bits(n))
begin
    print_bits_hex(x);
end

function print(x : String)
begin
    print_str(x);
end

function print(x : Boolean)
begin
    builtin_print_bool(x);
end

function print(x : Integer)
begin
    print_int_hex(x);
end

function println()
begin
    print_char(10);
end

function putchar(c : Integer)
begin
    print_char(c);
end

use Std::RAM as __RAM;

function asl_ram_init(a : Integer, ram : Std::RAM(a), val : Bits(64)) -> ()
begin
    Std::RAM::Init(ram, val);
end

function asl_ram_read(a : Integer, n : Integer, ram : Std::RAM(a), address : Bits(a)) -> Bits(8*n)
begin
    return Std::RAM::Read(ram, address, n);
end

function asl_ram_write(a : Integer, n : Integer, ram : Std::RAM(a), address : Bits(a), val : Bits(8*n)) -> ()
begin
    Std::RAM::Write(ram, address, n, val);
end

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
