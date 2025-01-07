// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(4)) => integer {-8..7}
begin
    return asl_cvt_bits_sint(x);
end

func main() => integer
begin
    print_int_dec(Test('0010')); println();
    // CHECK: 2
    print_int_dec(Test('1010')); println();
    // CHECK: -6
    return 0;
end
