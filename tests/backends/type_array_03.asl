// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var R : array [4] of bits(32);

func FUT(i : integer) => bits(32)
begin
    return R[i];
end

func main() => integer
begin
    R[0] = 10[0 +: 32];
    R[1] = 11[0 +: 32];
    R[2] = 12[0 +: 32];
    R[3] = 13[0 +: 32];

    print_bits_hex(FUT(4)); println();
    // CHECK: Evaluation error: assertion failure

    return 0;
end
