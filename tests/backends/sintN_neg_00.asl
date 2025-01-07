// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8)) => __sint(8)
begin
    return asl_neg_sintN(x);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd4)); println();
    // CHECK: -i8'd4
    print_sintN_dec(FUT(-i8'd5)); println();
    // CHECK: i8'd5
    print_sintN_dec(FUT(i8'd0)); println();
    // CHECK: i8'd0
    return 0;
end
