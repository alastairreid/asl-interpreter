// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

record R{
    x : integer;
    y : integer;
};

func T() => R
begin
    return R{x=1, z=3};
// CHECK: Type error: record initializer is missing fields y and/or has extra fields z
end
