// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func IntFunction(x : integer)
begin
end

func T()
begin
    IntFunction(TRUE);
// CHECK: Type error: function arguments
end
