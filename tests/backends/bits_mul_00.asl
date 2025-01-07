// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(8), y : bits(8)) => bits(8)
begin
    return asl_mul_bits(x, y);
end

func main() => integer
begin
    print_bits_hex(Test(3[0 +: 8], 7[0 +: 8])); println();
    // CHECK: 8'x15
    print_bits_hex(Test(255[0 +: 8], 255[0 +: 8])); println();
    // CHECK: 8'x1
    return 0;
end

