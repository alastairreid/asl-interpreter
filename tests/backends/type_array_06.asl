// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var R : array [3] of bits(32);

func FUT(i : integer, x : bits(32))
begin
    R[i] = x;
end

func main() => integer
begin
    FUT(-1, asl_zeros_bits(32));
    // CHECK: Evaluation error: assertion failure

    return 0;
end
