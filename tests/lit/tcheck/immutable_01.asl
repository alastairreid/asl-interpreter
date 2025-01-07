// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func F(x : bits(N))
begin
    N = 1;
// CHECK: Type error: assignment to immutable variable `N` declared at
end
