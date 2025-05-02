// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func Test_8_4(x : bits(8), i : integer, y : bits(4), j : integer, z : bit) => bits(8)
begin
    return x with { [i +: 4] = y, [j +: 1] = z };
end

func main() => integer
begin
    print_bits_hex(Test_8_4('0000 0000', 0, '1111', 7, '1')); println();
    // CHECK: 8'x8f

    print_bits_hex(Test_8_4('0000 0000', 0, '1111', 6, '1')); println();
    // CHECK: 8'x4f

    print_bits_hex(Test_8_4('0000 0000', 0, '1111', 5, '1')); println();
    // CHECK: 8'x2f

    return 0;
end
