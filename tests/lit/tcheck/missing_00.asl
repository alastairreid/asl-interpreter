// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func T()
begin
    UndefinedFunction();
// CHECK: Type error: Unknown procedure UndefinedFunction
end
