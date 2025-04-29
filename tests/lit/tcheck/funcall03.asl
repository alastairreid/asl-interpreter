// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : integer, y : integer = 0, z : integer);

func main() => integer
begin
    F(0, 1, 2, z=3);
    // CHECK: Type error: multiple bindings for argument 'z'
end
