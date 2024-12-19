// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F(x : integer)
begin
    let x = 42;
// CHECK: Type error: variable `x` previously declared
end
