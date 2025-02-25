// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT1(x : integer, y : integer) => integer
begin
    return x DIV y;
    // CHECK: __assert asl_ne_int.0{}(0, y) __in asl_exact_div_int.0{}(x, y);
end

func FUT2(x : integer, y : integer) => integer
begin
    return x MOD y;
    // CHECK: __assert asl_ne_int.0{}(0, y) __in asl_frem_int.0{}(x, y);
end

func FUT3(x : integer, y : integer) => integer
begin
    return x QUOT y;
    // CHECK: __assert asl_ne_int.0{}(0, y) __in asl_zdiv_int.0{}(x, y);
end

func FUT4(x : integer, y : integer) => integer
begin
    return x REM y;
    // CHECK: __assert asl_ne_int.0{}(0, y) __in asl_zrem_int.0{}(x, y);
end
