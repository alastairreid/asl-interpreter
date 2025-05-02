// RUN: %asli --batchmode %s '--exec=main()' | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func main() => integer
begin
    let x = 24'xABCDEF;

    print_bits_hex(x[7:4]); println();
    // CHECK: 4'xe

    print_bits_hex(x[8 +: 4]); println();
    // CHECK: 4'xd

    print_bits_hex(x[15 -: 4]); println();
    // CHECK: 4'xc

    print_bits_hex(x[4 *: 4]); println();
    // CHECK: 4'xb

    return 0;
end
