// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT1(x : bits(32), i : integer {0..31}) => bits(32)
begin
    var r : bits(32) = x;
    r[i] = Ones(1);
    // CHECK: assert asl_lt_int.0{}(i, 32);
    // CHECK: assert asl_le_int.0{}(0, i);
    return r;
end

func FUT2(x : bits(32), h : integer {0..31}, l : integer {0..4}) => bits(32)
begin
    var r : bits(32) = x;
    r[h:l] = Ones(h-l+1);
    // CHECK: assert asl_lt_int.0{}(h, 32);
    // CHECK: assert asl_le_int.0{}(l, asl_add_int.0{}(h, 1));
    // CHECK: assert asl_le_int.0{}(0, l);
    return r;
end


func FUT3(x : bits(32), j : integer {0..15}, w : integer) => bits(32)
begin
    var r : bits(32) = x;
    r[j+:w] = Ones(w);
    // CHECK: assert asl_le_int.0{}(asl_add_int.0{}(j, w), 32);
    // CHECK: assert asl_le_int.0{}(0, w);
    // CHECK: assert asl_le_int.0{}(0, j);
    return r;
end

func FUT4(x : bits(32), j : integer {0..8}, w : integer) => bits(32)
begin
    var r : bits(32) = x;
    r[j*:w] = Ones(w);
    // CHECK: assert asl_le_int.0{}(asl_mul_int.0{}(j, w), asl_sub_int.0{}(32, w));
    // CHECK: assert asl_le_int.0{}(0, w);
    // CHECK: assert asl_le_int.0{}(0, j);
    return r;
end

func FUT5(x : bits(32), j : integer {0..15}, w : integer) => bits(32)
begin
    var r : bits(32) = x;
    r[j-:w] = Ones(w);
    // CHECK: assert asl_le_int.0{}(w, asl_add_int.0{}(j, 1));
    // CHECK: assert asl_le_int.0{}(0, w);
    // CHECK: assert asl_lt_int.0{}(j, 32);
    return r;
end
