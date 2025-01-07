// RUN: %asli --batchmode %s
// Copyright (C) 2023-2025 Intel Corporation

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
    return T?();
end

