// RUN: not %asli --max-errors=10 --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

type E of exception;

func T?() => integer
begin
    // Note: functions that return a value cannot be noreturn
    // so we add an if statement to make it look optional
    if TRUE then throw E; end
    return 0;
end

func G() => integer
begin
    return T?() + 1;
end

func F() => integer
begin
    return G();
// CHECK: Type error: call to function `G.0` should be marked with `?` or `!` because it can throw an exception
end

