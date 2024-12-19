// RUN: not %asli --max-errors=10 --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

type E of exception;

func T() => integer
begin
    return 1;
end

func F() => integer
begin
    return T?();
// CHECK: Type error: call to function `T.0` is incorrectly marked with `?` but it cannot throw an exception
end

