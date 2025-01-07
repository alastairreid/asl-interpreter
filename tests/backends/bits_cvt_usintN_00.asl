// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT4(x : bits(4)) => __sint(5)
begin
    return asl_cvt_bits_usintN(x);
end

func main() => integer
begin
    print_sintN_dec(FUT4('0010')); println();
    // CHECK: i5'd2
    print_sintN_dec(FUT4('1010')); println();
    // CHECK: i5'd10
    return 0;
end
