// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return asl_mod_pow2_int(x, y);
end

func main() => integer
begin
    print_int_dec(Test(12, 2)); println();
    // CHECK: 0
    print_int_dec(Test(13, 2)); println();
    // CHECK: 1
    print_int_dec(Test(14, 2)); println();
    // CHECK: 2
    print_int_dec(Test(15, 2)); println();
    // CHECK: 3
    print_int_dec(Test(16, 2)); println();
    // CHECK: 0
    return 0;
end
