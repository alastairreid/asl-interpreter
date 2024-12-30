// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : integer) => bit
begin
    return x[0 +: 1];
end

func main() => integer
begin
    print_bits_hex(FUT(0)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(1)); println();
    // CHECK: 1'x1
    print_bits_hex(FUT(2)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(3)); println();
    // CHECK: 1'x1

    return 0;
end
