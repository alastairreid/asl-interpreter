// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => __sint(8)
begin
    return asl_zdiv_sintN(x, y);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd6, i8'd3)); println();
    // CHECK: i8'd2
    print_sintN_dec(FUT(-i8'd6, i8'd3)); println();
    // CHECK: -i8'd2
    print_sintN_dec(FUT(i8'd5, i8'd3)); println();
    // CHECK: i8'd1
    print_sintN_dec(FUT(-i8'd5, i8'd3)); println();
    // CHECK: -i8'd1
    print_sintN_dec(FUT(i8'd6, -i8'd3)); println();
    // CHECK: -i8'd2
    return 0;
end
