// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var R : array [4] of bits(32);

func Test(i : integer) => bits(32)
begin
    return R[i];
end

func main() => integer
begin
    R[0] = 10[0 +: 32];
    R[1] = 11[0 +: 32];
    R[2] = 12[0 +: 32];
    R[3] = 13[0 +: 32];

    print_bits_hex(Test(0)); println();
    // CHECK: 32'xa
    print_bits_hex(Test(1)); println();
    // CHECK: 32'xb
    print_bits_hex(Test(2)); println();
    // CHECK: 32'xc
    print_bits_hex(Test(3)); println();
    // CHECK: 32'xd

    return 0;
end

