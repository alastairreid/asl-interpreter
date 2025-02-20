// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_8_4(x : bits(8), i : __sint(4), y : bits(4)) => bits(8)
begin
    var r = x;
    r[asl_cvt_sintN_int(i) +: 4] = y;
    return r;
end

func main() => integer
begin
    print_bits_hex(Test_8_4('0000 0000', i4'd0, '1111')); println();
    // CHECK: 8'xf
    print_bits_hex(Test_8_4('0000 0000', i4'd1, '1111')); println();
    // CHECK: 8'x1e
    print_bits_hex(Test_8_4('0000 0000', i4'd2, '1111')); println();
    // CHECK: 8'x3c
    print_bits_hex(Test_8_4('0000 0000', i4'd3, '1111')); println();
    // CHECK: 8'x78
    print_bits_hex(Test_8_4('0000 0000', i4'd4, '1111')); println();
    // CHECK: 8'xf0

    print_bits_hex(Test_8_4('1111 1111', i4'd0, '0000')); println();
    // CHECK: 8'xf0
    print_bits_hex(Test_8_4('1111 1111', i4'd1, '0000')); println();
    // CHECK: 8'xe1
    print_bits_hex(Test_8_4('1111 1111', i4'd2, '0000')); println();
    // CHECK: 8'xc3
    print_bits_hex(Test_8_4('1111 1111', i4'd3, '0000')); println();
    // CHECK: 8'x87
    print_bits_hex(Test_8_4('1111 1111', i4'd4, '0000')); println();
    // CHECK: 8'xf

    return 0;
end
