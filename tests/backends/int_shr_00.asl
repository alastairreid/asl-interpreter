// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x >> y;
end

func main() => integer
begin
    print_int_dec(Test(27, 0)); println();
    // CHECK: 27
    print_int_dec(Test(27, 1)); println();
    // CHECK: 13
    print_int_dec(Test(27, 2)); println();
    // CHECK: 6
    return 0;
end
