// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT_8(x : __sint(8)) => integer
begin
    return asl_cvt_sintN_int(x);
end

func main() => integer
begin
    print_int_hex(FUT_8(i8'd0)); println();
    // CHECK: 0x0
    print_int_hex(FUT_8(i8'x7f)); println();
    // CHECK: 0x7f
    print_int_hex(FUT_8(-i8'x1)); println();
    // CHECK: -0x1
    print_int_hex(FUT_8(-i8'x80)); println();
    // CHECK: -0x80

    return 0;
end
