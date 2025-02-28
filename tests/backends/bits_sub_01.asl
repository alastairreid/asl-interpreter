// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(x : bits(16)) => bits(64)
begin
    // This test used to fail in c23 backend due to the subtraction
    // being promoted to signed.
    return asl_zero_extend_bits(asl_sub_bits(x, 16'x1), 64);
end

func main() => integer
begin
    print_bits_hex(FUT(16'x0)); println();
    // CHECK: 64'xf
    return 0;
end
