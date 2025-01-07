// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => __sint(8)
begin
    return asl_mul_sintN(x, y);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd4, i8'd5)); println();
    // CHECK: i8'd20
    print_sintN_dec(FUT(i8'd0, i8'd2)); println();
    // CHECK: i8'd0
    print_sintN_dec(FUT(i8'd3, -i8'd2)); println();
    // CHECK: -i8'd6
    return 0;
end
