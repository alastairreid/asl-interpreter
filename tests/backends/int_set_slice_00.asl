// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_4(x : integer, i : integer, y : bits(4)) => integer
begin
    var r = x;
    r[i +: 4] = y;
    return r;
end

func main() => integer
begin
    print_int_hex(Test_4(0, 0, '1111')); println();
    // CHECK: 0xf
    print_int_hex(Test_4(0, 1, '1111')); println();
    // CHECK: 0x1e
    print_int_hex(Test_4(0, 2, '1111')); println();
    // CHECK: 0x3c
    print_int_hex(Test_4(0, 3, '1111')); println();
    // CHECK: 0x78
    print_int_hex(Test_4(0, 4, '1111')); println();
    // CHECK: 0xf0

    print_int_hex(Test_4(255, 0, '0000')); println();
    // CHECK: 0xf0
    print_int_hex(Test_4(255, 1, '0000')); println();
    // CHECK: 0xe1
    print_int_hex(Test_4(255, 2, '0000')); println();
    // CHECK: 0xc3
    print_int_hex(Test_4(255, 3, '0000')); println();
    // CHECK: 0x87
    print_int_hex(Test_4(255, 4, '0000')); println();
    // CHECK: 0xf

    print_int_dec(Test_4(-1, 0, '0000')); println();
    // CHECK: -16
    print_int_dec(Test_4(-1, 1, '0000')); println();
    // CHECK: -31
    print_int_dec(Test_4(-1, 2, '0000')); println();
    // CHECK: -61
    print_int_dec(Test_4(-1, 3, '0000')); println();
    // CHECK: -121
    print_int_dec(Test_4(-1, 4, '0000')); println();
    // CHECK: -241

    return 0;
end
