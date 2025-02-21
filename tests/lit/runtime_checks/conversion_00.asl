// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT1(i : integer {0..15}) => integer {0..7, 15}
begin
    let r = if i < 8 then i else 15;
    return (r as {0..7, 15});
    // CHECK: return __assert asl_or_bool.0{}(asl_and_bool.0{}(asl_le_int.0{}(0, r), asl_le_int.0{}(r, 7)), asl_eq_int.0{}(r, 15)) __in (r as {0..7, 15});
end

func FUT2(i : integer {0..15}) => integer {0..7, 15}
begin
    let r = if i < 8 then i else 15;
    return (r as integer {0..7, 15});
    // CHECK: return __assert asl_or_bool.0{}(asl_and_bool.0{}(asl_le_int.0{}(0, r), asl_le_int.0{}(r, 7)), asl_eq_int.0{}(r, 15)) __in (r as integer{0..7, 15});
end
