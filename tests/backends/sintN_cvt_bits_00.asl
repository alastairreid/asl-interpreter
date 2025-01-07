// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : __sint(8)) => bits(8)
begin
    return asl_cvt_sintN_bits(x, 8);
end

func main() => integer
begin
    print_bits_hex(FUT(i8'd42)); println();
    // CHECK: 8'x2a
    print_bits_hex(FUT(-i8'd1)); println();
    // CHECK: 8'xff
    return 0;
end

