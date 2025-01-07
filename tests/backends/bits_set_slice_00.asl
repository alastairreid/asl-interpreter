// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_8_4(x : bits(8), i : integer, y : bits(4)) => bits(8)
begin
    var r = x;
    r[i +: 4] = y;
    return r;
end

func main() => integer
begin
    print_bits_hex(Test_8_4('0000 0000', 0, '1111')); println();
    // CHECK: 8'xf
    print_bits_hex(Test_8_4('0000 0000', 1, '1111')); println();
    // CHECK: 8'x1e
    print_bits_hex(Test_8_4('0000 0000', 2, '1111')); println();
    // CHECK: 8'x3c
    print_bits_hex(Test_8_4('0000 0000', 3, '1111')); println();
    // CHECK: 8'x78
    print_bits_hex(Test_8_4('0000 0000', 4, '1111')); println();
    // CHECK: 8'xf0

    print_bits_hex(Test_8_4('1111 1111', 0, '0000')); println();
    // CHECK: 8'xf0
    print_bits_hex(Test_8_4('1111 1111', 1, '0000')); println();
    // CHECK: 8'xe1
    print_bits_hex(Test_8_4('1111 1111', 2, '0000')); println();
    // CHECK: 8'xc3
    print_bits_hex(Test_8_4('1111 1111', 3, '0000')); println();
    // CHECK: 8'x87
    print_bits_hex(Test_8_4('1111 1111', 4, '0000')); println();
    // CHECK: 8'xf

    return 0;
end
