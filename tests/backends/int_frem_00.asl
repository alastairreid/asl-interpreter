// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return asl_frem_int(x, y);
end

func main() => integer
begin
    print_int_dec(Test(6, 3)); println();
    // CHECK: 0
    print_int_dec(Test(-6, 3)); println();
    // CHECK: 0
    print_int_dec(Test(5, 3)); println();
    // CHECK: 2
    print_int_dec(Test(-5, 3)); println();
    // CHECK: 1
    print_int_dec(Test(6, -3)); println();
    // CHECK: 0
    return 0;
end
