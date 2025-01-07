// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test0() => bits(0)
begin
    return asl_ones_bits(0);
end

func Test6() => bits(6)
begin
    return asl_ones_bits(6);
end

func main() => integer
begin
    print_bits_hex(Test0()); println();
    // CHECK: 0'x0
    print_bits_hex(Test6()); println();
    // CHECK: 6'x3f
    return 0;
end
