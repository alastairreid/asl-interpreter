// RUN: %aslrun %s --configuration=%S/ffi_import_01.json --extra-c=%S/ffi_import_01.c | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

// UNSUPPORTED: interpreter

enumeration E {A, B, C};

// to be defined externally

func FFI_null_bits8(x : bits(8)) => bits(8);
func FFI_null_bits16(x : bits(16)) => bits(16);
func FFI_null_bits32(x : bits(32)) => bits(32);
func FFI_null_bits64(x : bits(64)) => bits(64);

func FFI_null_bits17(x : bits(17)) => bits(17);
func FFI_null_bits65(x : bits(65)) => bits(65);
func FFI_null_bits127(x : bits(127)) => bits(127);
func FFI_null_bits128(x : bits(128)) => bits(128);

func FFI_null_string(x : string) => string;
func FFI_null_E(x : E) => E;
func FFI_null_boolean(x : boolean) => boolean;
func FFI_null_integer(x : integer) => integer;
func FFI_null_sint17(x : __sint(17)) => __sint(17);

// Support for functions that return multiple values
// is complicated by the fact that the :xform_tuples
// transformation converts functions that return tuples
// to functions that return records.
// So, to support functions that return tuples in
// the original source code, the code generation
// backend has to support functions that return records.
// But, in the FFI, we don't want to expose that
// implementation detail so the corresponding C function
// takes extra arguments for each field: each argument
// a pointer to write the return value to.

record __Return_FFI_int_bool {
    r0 : integer;
    r1 : boolean;
};

func FFI_int_bool(x : integer) => __Return_FFI_int_bool;

func main() => integer
begin
    print_bits_hex(FFI_null_bits8(8'd8)); println();
    // CHECK: 8'x8
    print_bits_hex(FFI_null_bits16(16'd16)); println();
    // CHECK: 16'x10
    print_bits_hex(FFI_null_bits32(32'd32)); println();
    // CHECK: 32'x20
    print_bits_hex(FFI_null_bits64(64'd64)); println();
    // CHECK: 64'x40

    print_bits_hex(FFI_null_bits17(17'd17)); println();
    // CHECK: 17'x11
    print_bits_hex(FFI_null_bits65(65'd65)); println();
    // CHECK: 65'x41
    print_bits_hex(FFI_null_bits127(127'd127)); println();
    // CHECK: 127'x7f
    print_bits_hex(FFI_null_bits128(128'd128)); println();
    // CHECK: 128'x80

    print(FFI_null_string("abcd")); println();
    // CHECK: abcd
    print(FFI_null_E(C) == C); println();
    // CHECK: TRUE
    print(FFI_null_boolean(TRUE)); println();
    // CHECK: TRUE
    print_int_dec(FFI_null_integer(42)); println();
    // CHECK: 42
    print_sintN_dec(FFI_null_sint17(i17'd42)); println();
    // CHECK: i17'd42

    let ret1 = FFI_int_bool(1);
    print_int_dec(ret1.r0); print(" "); print(ret1.r1); println();
    // CHECK: 1 FALSE
    let ret2 = FFI_int_bool(4);
    print_int_dec(ret2.r0); print(" "); print(ret2.r1); println();
    // CHECK: 4 TRUE

    return 0;
end
