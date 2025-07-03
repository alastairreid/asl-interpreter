// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

record R{
    x : integer;
    y : integer;
};

func T() => R
begin
    return R{x=1, z=3};
// CHECK: Type error: record initializer is missing field[s] y and/or has extra field[s] z
end
