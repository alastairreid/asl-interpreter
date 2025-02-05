// RUN: %asli --batchmode --exec=:xform_hoist_lets --exec=":show --format=raw FUT" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(x : bits(32), i : integer {0..32}) => boolean
begin
    // A variant of the original motivating example that prevented use
    // of a transformation that looked for "IsZero(x[_ +: _])"
    return IsZero(__let wd : integer = 32-i __in
                  __let ix : integer = 31-i __in
                  __assert 0 <= ix && ix < 32 __in
                  __assert 0 <= wd && ix + wd <= 32 __in
                  x[ix +: wd]);
end

// CHECK: let wd : integer = asl_sub_int.0{}(32, i);
// CHECK: let ix : integer = asl_sub_int.0{}(31, i);
// CHECK: assert asl_and_bool.0{}(asl_le_int.0{}(0, ix), asl_lt_int.0{}(ix, 32));
// CHECK: assert asl_and_bool.0{}(asl_le_int.0{}(0, wd), asl_le_int.0{}(asl_add_int.0{}(ix, wd), 32));
// CHECK: return IsZero.0{wd}({bits(32)}x[ix +: wd]);
