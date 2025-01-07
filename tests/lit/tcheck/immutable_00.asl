// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func F(x : integer)
begin
    x = 1;
// CHECK: Type error: assignment to immutable variable `x` declared at
end
