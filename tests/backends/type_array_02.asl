// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

let R : array [3] of bits(32) = array(32'x3, 32'x4, 32'x5);

func Test(i : integer) => bits(32)
begin
    return R[i];
end

func main() => integer
begin
    print_bits_hex(Test(0)); println();
    // CHECK: 32'x3
    print_bits_hex(Test(1)); println();
    // CHECK: 32'x4
    print_bits_hex(Test(2)); println();
    // CHECK: 32'x5

    return 0;
end

