// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : __sint(5)) => integer
begin
    var s = 0;
    for i : __sint(5) = i5'd0 to x do
        s = s + asl_cvt_sintN_int(i);
    end
    return s;
end

func main() => integer
begin
    print_int_dec(Test(i5'd0)); println();
    // CHECK: 0
    print_int_dec(Test(i5'd1)); println();
    // CHECK: 1
    print_int_dec(Test(i5'd3)); println();
    // CHECK: 6

    return 0;
end
