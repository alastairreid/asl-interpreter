// RUN: %asli --batchmode --exec=:xform_hoist_lets --exec=":show --format=raw FUT" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(x : bits(32), i : integer {0..32}) => boolean
begin
    // The original motivating example - this prevented use
    // of a transformation that looked for "IsZero(x[_ +: _])"
    return IsZero(__let wd : integer = 32-i __in
                  __let ix : integer = 31-i __in
                  x[ix +: wd]);
end

// CHECK: let wd : integer = asl_sub_int.0{}(32, i);
// CHECK: let ix : integer = asl_sub_int.0{}(31, i);
// CHECK: return IsZero.0{wd}({bits(32)}x[ix +: wd]);
