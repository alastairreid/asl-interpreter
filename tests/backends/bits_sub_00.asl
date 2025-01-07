// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(8), y : bits(8)) => bits(8)
begin
    return asl_sub_bits(x, y);
end

func main() => integer
begin
    print_bits_hex(Test(1[0 +: 8], 2[0 +: 8])); println();
    // CHECK: 8'xff
    print_bits_hex(Test(2[0 +: 8], 1[0 +: 8])); println();
    // CHECK: 8'x1
    return 0;
end
