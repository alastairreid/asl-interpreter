// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT4(x : bits(4)) => __sint(4)
begin
    return asl_cvt_bits_ssintN(x);
end

func main() => integer
begin
    print_sintN_dec(FUT4('0010')); println();
    // CHECK: i4'd2
    print_sintN_dec(FUT4('1010')); println();
    // CHECK: -i4'd6
    return 0;
end
