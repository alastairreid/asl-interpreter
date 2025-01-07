// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    return asl_pow2_int(x);
end

func main() => integer
begin
    print_int_dec(Test(3)); println();
    // CHECK: 8
    print_int_dec(Test(7)); println();
    // CHECK: 128
    return 0;
end
