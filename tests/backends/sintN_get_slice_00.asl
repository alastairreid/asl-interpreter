// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_8_4(x : bits(8), i : __sint(4)) => bits(4)
begin
    return x[asl_cvt_sintN_int(i) +: 4];
end

func main() => integer
begin
    print_bits_hex(Test_8_4('1010 0101', i4'd0)); println();
    // CHECK: 4'x5
    print_bits_hex(Test_8_4('1010 0101', i4'd1)); println();
    // CHECK: 4'x2
    print_bits_hex(Test_8_4('1010 0101', i4'd2)); println();
    // CHECK: 4'x9
    print_bits_hex(Test_8_4('1010 0101', i4'd3)); println();
    // CHECK: 4'x4
    print_bits_hex(Test_8_4('1010 0101', i4'd4)); println();
    // CHECK: 4'xa

    return 0;
end
