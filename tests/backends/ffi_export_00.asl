// RUN: %aslrun %s --import=FFI_Call128 --export=FFI_Extract128_32 --extra-c=%S/ffi_export_00.c | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

// UNSUPPORTED: interpreter

// Function imported from C
func FFI_Call128(x : bits(128)) => bits(32);

// Function exported to C
func FFI_Extract128_32(x : bits(128), i : integer) => bits(32)
begin
    return x[i +: 32];
end

func main() => integer
begin
    print_bits_hex(FFI_Call128(128'xfedcba98765432100123456789abcdef)); println();
    // CHECK: 32'x56789abc

    return 0;
end
