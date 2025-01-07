// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : bits(4), i : integer) => bit
begin
    return x[i +: 1];
end

func main() => integer
begin
    // Positive numbers
    print_bits_hex(FUT(4'x5, 3)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(4'x5, 2)); println();
    // CHECK: 1'x1
    print_bits_hex(FUT(4'x5, 1)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(4'x5, 0)); println();
    // CHECK: 1'x1

    return 0;
end
