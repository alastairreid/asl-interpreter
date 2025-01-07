// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func F(x : integer)
begin
    let x = 42;
// CHECK: Type error: variable `x` previously declared
end
