// RUN: %aslrun %s --configuration=%S/ffi_export_01.json --extra-c=%S/ffi_export_01.c | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

// UNSUPPORTED: interpreter

enumeration E {A, B, C};

// to be exported
// each function returns its input argument

func FFI_bits8(x : bits(8)) => bits(8)
begin
    return x;
end

func FFI_bits16(x : bits(16)) => bits(16)
begin
    return x;
end

func FFI_bits32(x : bits(32)) => bits(32)
begin
    return x;
end

func FFI_bits64(x : bits(64)) => bits(64)
begin
    return x;
end


func FFI_bits17(x : bits(17)) => bits(17)
begin
    return x;
end

func FFI_bits65(x : bits(65)) => bits(65)
begin
    return x;
end

func FFI_bits127(x : bits(127)) => bits(127)
begin
    return x;
end

func FFI_bits128(x : bits(128)) => bits(128)
begin
    return x;
end


func FFI_string(x : string) => string
begin
    return x;
end

func FFI_E(x : E) => E
begin
    return x;
end

func FFI_boolean(x : boolean) => boolean
begin
    return x;
end

func FFI_integer(x : integer) => integer
begin
    return x;
end

func FFI_sint17(x : __sint(17)) => __sint(17)
begin
    return x;
end

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

func FFI_int_bool(x : integer) => __Return_FFI_int_bool
begin
    return __Return_FFI_int_bool{ r0=x, r1=x > 3};
end

// Function imported from C
func FFI_test_exports();

func main() => integer
begin
    FFI_test_exports();
    // CHECK: 8'x8
    // CHECK: 16'x10
    // CHECK: 32'x20
    // CHECK: 64'x40

    // CHECK: 17'x11
    // CHECK: 65'x000000041
    // CHECK: 127'x00000007f
    // CHECK: 128'x000000080

    // CHECK: abcd
    // CHECK: TRUE
    // CHECK: TRUE
    // CHECK: 42
    // CHECK: i17'd42

    // CHECK: (1, FALSE)
    // CHECK: (4, TRUE)

    return 0;
end
