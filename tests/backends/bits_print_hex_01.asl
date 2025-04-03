// RUN: %aslrun %s | filecheck %s
// REQUIRES: !c23 || wide_bitint
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_bits_hex(256'xff01020304050607_1011121314151617_2021222324252627_3031323334353637); println();
    // CHECK: 256'xff01020304050607101112131415161720212223242526273031323334353637

    print_bits_hex(32'x0); println();
    // CHECK: 32'x0
    print_bits_hex(32'x8000_0000); println();
    // CHECK: 32'x80000000
    print_bits_hex(32'xffff_ffff); println();
    // CHECK: 32'xffffffff

    return 0;
end
