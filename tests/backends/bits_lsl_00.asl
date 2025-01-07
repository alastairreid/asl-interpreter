// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(8), y : integer) => bits(8)
begin
    return asl_lsl_bits(x, y);
end

func main() => integer
begin
    print_bits_hex(Test('1010 0101', 3)); println();
    // CHECK: 8'x28
    print_bits_hex(Test('0101 1010', 2)); println();
    // CHECK: 8'x68
    return 0;
end
