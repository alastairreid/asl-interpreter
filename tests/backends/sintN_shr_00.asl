// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => __sint(8)
begin
    return asl_shr_sintN(x, y);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd27, i8'd0)); println();
    // CHECK: i8'd27
    print_sintN_dec(FUT(i8'd27, i8'd1)); println();
    // CHECK: i8'd13
    print_sintN_dec(FUT(i8'd27, i8'd2)); println();
    // CHECK: i8'd6
    return 0;
end
