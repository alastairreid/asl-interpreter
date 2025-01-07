// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT_8(x : integer) => __sint(8)
begin
    return asl_cvt_int_sintN(x, 8);
end

func main() => integer
begin
    print_sintN_hex(FUT_8(0x0)); println();
    // CHECK: i8'x0
    print_sintN_hex(FUT_8(0x7f)); println();
    // CHECK: i8'x7f
    print_sintN_hex(FUT_8(-0x1)); println();
    // CHECK: -i8'x1
    print_sintN_hex(FUT_8(-0x80)); println();
    // CHECK: -i8'x80

    return 0;
end
