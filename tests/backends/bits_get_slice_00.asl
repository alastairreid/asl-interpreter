// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_8_4(x : bits(8), i : integer) => bits(4)
begin
    return x[i +: 4];
end

func main() => integer
begin
    print_bits_hex(Test_8_4('1010 0101', 0)); println();
    // CHECK: 4'x5
    print_bits_hex(Test_8_4('1010 0101', 1)); println();
    // CHECK: 4'x2
    print_bits_hex(Test_8_4('1010 0101', 2)); println();
    // CHECK: 4'x9
    print_bits_hex(Test_8_4('1010 0101', 3)); println();
    // CHECK: 4'x4
    print_bits_hex(Test_8_4('1010 0101', 4)); println();
    // CHECK: 4'xa

    return 0;
end
