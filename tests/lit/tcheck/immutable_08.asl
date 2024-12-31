// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

constant C : integer = 1;

func F()
begin
    C = 2;
// CHECK: Type error: assignment to immutable variable `C` declared at
end
