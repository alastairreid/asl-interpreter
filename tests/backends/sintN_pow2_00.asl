// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8)) => __sint(8)
begin
    return asl_pow2_sintN(x);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd3)); println();
    // CHECK: i8'd8
    print_sintN_dec(FUT(i8'd6)); println();
    // CHECK: i8'd64
    return 0;
end
