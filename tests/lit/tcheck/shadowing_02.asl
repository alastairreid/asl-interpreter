// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F{N}(x : bits(N))
begin
    let N = 32;
// CHECK: Type error: variable `N` previously declared
end
