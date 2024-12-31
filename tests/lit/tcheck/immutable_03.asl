// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F()
begin
    let x = 1;
    x = 2;
// CHECK: Type error: assignment to immutable variable `x` declared at
end
