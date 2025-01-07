// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(8)) => bits(8)
begin
    return (NOT x);
end

func main() => integer
begin
    print_bits_hex(Test('1011 0011')); println();
    // CHECK: 8'x4c
    return 0;
end
