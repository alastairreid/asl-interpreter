// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

var R : array [16] of bits(32);

func FUT1(i : integer {0..15}) => bits(32)
begin
    return R[i];
    // CHECK: __assert asl_lt_int.0{}(i, 16) __in __assert asl_le_int.0{}(0, i) __in R[i];
end
