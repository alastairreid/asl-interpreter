// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT1(x : bits(32), i : integer {0..31}) => bits(32)
begin
    return x with { [i] = Ones(1) };
    // CHECK: return (__assert asl_lt_int.0{}(i, 32) __in (__assert asl_le_int.0{}(0, i) __in
end

func FUT2(x : bits(32), h : integer {0..31}, l : integer {0..4}) => bits(32)
begin
    return x with { [h:l] = Ones(h-l+1) };
    // CHECK: return (__assert asl_lt_int.0{}(h, 32) __in (__assert asl_le_int.0{}(l, asl_add_int.0{}(h, 1)) __in (__assert asl_le_int.0{}(0, l) __in
end


func FUT3(x : bits(32), j : integer {0..15}, w : integer) => bits(32)
begin
    return x with { [j+:w] = Ones(w) };
    // CHECK: return (__assert asl_le_int.0{}(asl_add_int.0{}(j, w), 32) __in (__assert asl_le_int.0{}(0, w) __in (__assert asl_le_int.0{}(0, j) __in
end

func FUT4(x : bits(32), j : integer {0..8}, w : integer) => bits(32)
begin
    return x with { [j*:w] = Ones(w) };
    // CHECK: return (__assert asl_le_int.0{}(asl_mul_int.0{}(j, w), asl_sub_int.0{}(32, w)) __in (__assert asl_le_int.0{}(0, w) __in (__assert asl_le_int.0{}(0, j) __in
end

func FUT5(x : bits(32), j : integer {0..15}, w : integer) => bits(32)
begin
    return x with { [j-:w] = Ones(w) };
    // CHECK: return (__assert asl_le_int.0{}(w, asl_add_int.0{}(j, 1)) __in (__assert asl_le_int.0{}(0, w) __in (__assert asl_lt_int.0{}(j, 32) __in
end
