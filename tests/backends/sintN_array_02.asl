// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

let R : array [3] of bits(32) = array(32'x3, 32'x4, 32'x5);

func Test(i : __sint(3)) => bits(32)
begin
    return R[asl_cvt_sintN_int(i)];
end

func main() => integer
begin
    print_bits_hex(Test(i3'd0)); println();
    // CHECK: 32'x3
    print_bits_hex(Test(i3'd1)); println();
    // CHECK: 32'x4
    print_bits_hex(Test(i3'd2)); println();
    // CHECK: 32'x5

    return 0;
end

