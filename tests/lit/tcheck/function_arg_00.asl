// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func IntFunction(x : integer)
begin
end

func T()
begin
    IntFunction(TRUE);
// CHECK: Type error: function arguments
end
