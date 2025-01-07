// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    return -x;
end

func main() => integer
begin
    print_int_dec(Test(4)); println();
    // CHECK: -4
    print_int_dec(Test(-5)); println();
    // CHECK: 5
    print_int_dec(Test(0)); println();
    // CHECK: 0
    return 0;
end
