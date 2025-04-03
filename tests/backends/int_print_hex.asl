// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_int_hex(42); println();
    // CHECK: 0x2a
    print_int_hex(-42); println();
    // CHECK: -0x2a

    // Some runtimes optimize the handling of 64-bit values
    // so test the extreme values and near neighbors.
    let max_64 =  0x7fff_ffff_ffff_ffff;
    let min_64 = -0x8000_0000_0000_0000;
    print_int_hex(max_64); println();
    // CHECK:  0x7fffffffffffffff
    print_int_hex(min_64); println();
    // CHECK: -0x800000000000000

    let max_64m1 =  0x7fff_ffff_ffff_fffe;
    let min_64p1 = -0x7fff_ffff_ffff_ffff;
    print_int_hex(max_64m1); println();
    // CHECK:  0x7ffffffffffffffe
    print_int_hex(min_64p1); println();
    // CHECK: -0x7fffffffffffffff

    // Most runtimes use 128 bits as the default int sizes
    // so test the extreme values and near neighbors.
    let max_128 =  0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff;
    let min_128 = -0x8000_0000_0000_0000_0000_0000_0000_0000;
    print_int_hex(max_128); println();
    // CHECK:  0x7fffffffffffffffffffffffffffffff
    print_int_hex(min_128); println();
    // CHECK: -0x80000000000000000000000000000000

    let max_128m1 =  0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_fffe;
    let min_128p1 = -0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff;
    print_int_hex(max_128m1); println();
    // CHECK:  0x7ffffffffffffffffffffffffffffffe
    print_int_hex(min_128p1); println();
    // CHECK: -0x7fffffffffffffffffffffffffffffff

    return 0;
end

