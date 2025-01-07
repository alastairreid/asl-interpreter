// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x * y;
end

func main() => integer
begin
    print_int_dec(Test(4, 5)); println();
    // CHECK: 20
    print_int_dec(Test(0, 2)); println();
    // CHECK: 0
    print_int_dec(Test(3, -2)); println();
    // CHECK: -6
    return 0;
end
