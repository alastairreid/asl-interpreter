// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F(x : boolean) => integer
begin
    return x;
// CHECK: Type error: type integer does not match boolean
end
