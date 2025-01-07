// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test0(x : bits(4)) => bits(0)
begin
    return asl_replicate_bits(x, 0);
end

func Test1(x : bits(4)) => bits(4)
begin
    return asl_replicate_bits(x, 1);
end

func Test5(x : bits(4)) => bits(20)
begin
    return asl_replicate_bits(x, 5);
end

func main() => integer
begin
    print_bits_hex(Test0('0001')); println();
    // CHECK: 0'x0
    print_bits_hex(Test1('0001')); println();
    // CHECK: 4'x1
    print_bits_hex(Test5('0001')); println();
    // CHECK: 20'x11111

    print_bits_hex(Test0('0101')); println();
    // CHECK: 0'x0
    print_bits_hex(Test1('0101')); println();
    // CHECK: 4'x5
    print_bits_hex(Test5('0101')); println();
    // CHECK: 20'x55555
    return 0;
end
