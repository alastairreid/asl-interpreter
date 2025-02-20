// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : bits(4), i : __sint(3)) => bit
begin
    return x[asl_cvt_sintN_int(i) +: 1];
end

func main() => integer
begin
    // Positive numbers
    print_bits_hex(FUT(4'x5, i3'd3)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(4'x5, i3'd2)); println();
    // CHECK: 1'x1
    print_bits_hex(FUT(4'x5, i3'd1)); println();
    // CHECK: 1'x0
    print_bits_hex(FUT(4'x5, i3'd0)); println();
    // CHECK: 1'x1

    return 0;
end
