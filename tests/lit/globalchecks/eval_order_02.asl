// RUN: not %asli --max-errors=10 --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

var X : integer;

func WX() => integer
begin
    X = 1;
    return 0;
end

type E of exception;

func T?() => integer
begin
    // Note: functions that return a value cannot be noreturn
    // so we add an if statement to make it look optional
    if TRUE then throw E; end
    return 0;
end

func F() => integer
begin
    return WX() + T?();
// CHECK: Type error: expression behaviour depends on evaluation order
end
