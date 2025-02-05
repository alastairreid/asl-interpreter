// RUN: %asli --batchmode --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

var R : array [16] of bits(32);

func FUT1(i : integer {0..15}, val : bits(32))
begin
    R[i] = val;
    // CHECK: assert asl_lt_int.0{}(i, 16);
    // CHECK: assert asl_le_int.0{}(0, i);
end
