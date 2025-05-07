// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func Test(x : integer) => bits(512)
begin
    return asl_cvt_int_bits(x, 512);
end

func main() => integer
begin
    print_bits_hex(Test(42)); println();
    // CHECK: 512'x2a
    print_bits_hex(Test(-1)); println();
    // CHECK: 512'xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    return 0;
end

