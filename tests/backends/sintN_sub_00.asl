// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : __sint(8), y : __sint(8)) => __sint(8)
begin
    return asl_sub_sintN(x, y);
end

func main() => integer
begin
    print_sintN_dec(FUT(i8'd1, i8'd2)); println();
    // CHECK: -i8'd1
    return 0;
end
