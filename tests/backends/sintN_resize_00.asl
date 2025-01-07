// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT_8_16(x : __sint(8)) => __sint(16)
begin
    return asl_resize_sintN(x, 16);
end

func FUT_16_8(x : __sint(16)) => __sint(8)
begin
    return asl_resize_sintN(x, 8);
end

func main() => integer
begin
    print_sintN_hex(FUT_8_16(i8'd0)); println();
    // CHECK: i16'x0
    print_sintN_hex(FUT_8_16(i8'x7f)); println();
    // CHECK: i16'x7f
    print_sintN_hex(FUT_8_16(-i8'x1)); println();
    // CHECK: -i16'x1
    print_sintN_hex(FUT_8_16(-i8'x80)); println();
    // CHECK: -i16'x80

    print_sintN_hex(FUT_16_8(i16'd0)); println();
    // CHECK: i8'x0
    print_sintN_hex(FUT_16_8(i16'x7f)); println();
    // CHECK: i8'x7f
    print_sintN_hex(FUT_16_8(-i16'x1)); println();
    // CHECK: -i8'x1
    print_sintN_hex(FUT_16_8(-i16'x80)); println();
    // CHECK: -i8'x80

    return 0;
end

