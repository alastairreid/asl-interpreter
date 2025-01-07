// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(3)) => bits(8)
begin
    return asl_zero_extend_bits(x, 8);
end

func main() => integer
begin
    print_bits_hex(Test('110')); println();
    // CHECK: 8'x6
    print_bits_hex(Test('011')); println();
    // CHECK: 8'x3
    return 0;
end
