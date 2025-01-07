// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x << y;
end

func main() => integer
begin
    print_int_dec(Test(3, 0)); println();
    // CHECK: 3
    print_int_dec(Test(3, 1)); println();
    // CHECK: 6
    print_int_dec(Test(3, 2)); println();
    // CHECK: 12
    return 0;
end
