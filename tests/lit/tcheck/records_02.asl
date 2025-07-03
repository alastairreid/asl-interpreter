// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

record R {
    x : integer;
    y : integer;
};

func F() => R
begin
    return R{x = 1};
// CHECK: Type error: record initializer is missing field[s] y and/or has extra field[s]
end
