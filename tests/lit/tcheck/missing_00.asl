// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func T()
begin
    UndefinedFunction();
// CHECK: Type error: Unknown procedure UndefinedFunction
end
