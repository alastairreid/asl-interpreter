// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(4)) => bits(4)
begin
    var A : array [1] of bits(4);
    A[0] = x;
    return A[0];
end

func main() => integer
begin
    print_bits_hex(Test('0001')); println();
    // CHECK: 4'x1
    print_bits_hex(Test('0101')); println();
    // CHECK: 4'x5

    return 0;
end
