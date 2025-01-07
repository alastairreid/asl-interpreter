// RUN: %aslrun %s | filecheck %s
// REQUIRES: !c23 || wide_bitint
// Copyright (C) 2023-2025 Intel Corporation

func Test_70_140(x : bits(70)) => bits(140)
begin
    return asl_sign_extend_bits(x, 140);
end

func main() => integer
begin
    print_bits_hex(Test_70_140(16384[0 +: 70])); println();
    // CHECK: 140'x4000
    print_bits_hex(Test_70_140((-16384)[0 +: 70])); println();
    // CHECK: 140'xfffffffffffffffffffffffffffffffc000

    return 0;
end
