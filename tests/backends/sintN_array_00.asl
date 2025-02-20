// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var R : array [4] of bits(32);

func Test(i : __sint(3)) => bits(32)
begin
    return R[asl_cvt_sintN_int(i)];
end

func main() => integer
begin
    R[0] = 10[0 +: 32];
    R[1] = 11[0 +: 32];
    R[2] = 12[0 +: 32];
    R[3] = 13[0 +: 32];

    print_bits_hex(Test(i3'd0)); println();
    // CHECK: 32'xa
    print_bits_hex(Test(i3'd1)); println();
    // CHECK: 32'xb
    print_bits_hex(Test(i3'd2)); println();
    // CHECK: 32'xc
    print_bits_hex(Test(i3'd3)); println();
    // CHECK: 32'xd

    return 0;
end

