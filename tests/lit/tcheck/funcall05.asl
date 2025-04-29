// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : integer, y : integer = 0, z : integer);

func main() => integer
begin
    F(0, 1, 2, p=3);
    // CHECK: Type error: named argument 'p' does not match any argument of function 'F(x, y, z)'
end
