// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return AlignDown(x, y);
end

func main() => integer
begin
    print_int_dec(Test(12, 2)); println();
    // CHECK: 12
    print_int_dec(Test(13, 2)); println();
    // CHECK: 12
    print_int_dec(Test(14, 2)); println();
    // CHECK: 12
    print_int_dec(Test(15, 2)); println();
    // CHECK: 12
    print_int_dec(Test(16, 2)); println();
    // CHECK: 16
    return 0;
end
