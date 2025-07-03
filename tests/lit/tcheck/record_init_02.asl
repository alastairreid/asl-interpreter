// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

record R{
    x : integer;
    y : integer;
};

func T() => R
begin
    return R{y=3, x=1};
// CHECK:      Type error: record initializer must set fields in the same order as the type declaration.
// CHECK-NEXT: Order of fields in type declaration: x, y
// CHECK-NEXT: Order of fields in record initializer: y, x
end

