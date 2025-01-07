// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => __sint(8)
begin
    return asl_mod_pow2_sintN(x, y);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd12, i8'd2)); println();
    // CHECK: i8'd0
    print_sintN_dec(FUT(i8'd13, i8'd2)); println();
    // CHECK: i8'd1
    print_sintN_dec(FUT(i8'd14, i8'd2)); println();
    // CHECK: i8'd2
    print_sintN_dec(FUT(i8'd15, i8'd2)); println();
    // CHECK: i8'd3
    print_sintN_dec(FUT(i8'd16, i8'd2)); println();
    // CHECK: i8'd0
    return 0;
end
