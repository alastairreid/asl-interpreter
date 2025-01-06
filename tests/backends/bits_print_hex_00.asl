// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func main() => integer
begin
    print_bits_hex(42[0 +: 8]); println();
    // CHECK: 8'x2a
    print_bits_hex(0xcf[0 +: 8]); println();
    // CHECK: 8'xcf
    print_bits_hex(0[0 +: 8]); println();
    // CHECK: 8'x0
    print_bits_hex(''); println();
    // CHECK: 0'x0
    print_bits_hex(16384[0 +: 70]); println();
    // CHECK: 70'x4000
    print_bits_hex((-16384)[0 +: 70]); println();
    // CHECK: 70'x3fffffffffffffc000

    return 0;
end
