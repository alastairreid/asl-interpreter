// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

var x : integer;

func F(x : integer)
begin
    let x = 42;
// CHECK: Type error: variable `x` previously declared
end
