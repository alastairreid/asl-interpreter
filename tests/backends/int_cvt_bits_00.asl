// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => bits(8)
begin
    return asl_cvt_int_bits(x, 8);
end

func main() => integer
begin
    print_bits_hex(Test(42)); println();
    // CHECK: 8'x2a
    print_bits_hex(Test(-1)); println();
    // CHECK: 8'xff
    return 0;
end

