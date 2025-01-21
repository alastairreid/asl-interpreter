// RUN: %aslrun %s --import=FFI_Invert32 --import=FFI_Invert128 --extra-c=%S/ffi_import_00.c | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

// UNSUPPORTED: interpreter

// The following functions will be replaced by
// a version that is defined externally.
// Normally, the internal and external version have
// similar behaviour but, for testing purposes, the
// internal version is the identity function
// and the external version inverts its argument.
func FFI_Invert32(x : bits(32)) => bits(32)
begin
    return x;
end

func FFI_Invert128(x : bits(128)) => bits(128)
begin
    return x;
end

func main() => integer
begin
    print_bits_hex(FFI_Invert32(32'x3)); println();
    // CHECK: 32'xfffffffc

    print_bits_hex(FFI_Invert128(128'x3)); println();
    // CHECK: 128'xfffffffffffffffffffffffffffffffc

    return 0;
end
