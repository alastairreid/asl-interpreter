// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_arg(x : bits(4)) => bits(4)
begin
    return x;
end

var G : bits(4);

func Test_global() => bits(4)
begin
    return G;
end

config C : bits(4) = '1000';

func Test_config() => bits(4)
begin
    return C;
end

func main() => integer
begin
    print_bits_hex(Test_arg('0001')); println();
    // CHECK: 4'x1
    print_bits_hex(Test_arg('0010')); println();
    // CHECK: 4'x2

    G = '0100';
    print_bits_hex(Test_global()); println();
    // CHECK: 4'x4
    G = '0101';
    print_bits_hex(Test_global()); println();
    // CHECK: 4'x5

    print_bits_hex(Test_config()); println();
    // CHECK: 4'x8

    return 0;
end

