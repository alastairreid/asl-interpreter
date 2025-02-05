// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : bits(8), i : integer) => bit
begin
    return __assert 0 <= i && i < 8 __in x[i +: 1];
end

func main() => integer
begin
    // assertion that do not fail
    print_bits_hex(FUT('1100 1111', 0)); println();
    // CHECK: 1'x1
    print_bits_hex(FUT('1100 1111', 4)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT('1100 1111', 7)); println();
    // CHECK: 1'x1

    return 0;
end
