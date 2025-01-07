// RUN: %asli --max-errors=10 --check-call-markers --batchmode %s
// Copyright (C) 2023-2025 Intel Corporation

type E of exception;

func NoThrow() => integer
begin
    return 0;
end

func MayThrow?() => integer
begin
    // Note: functions that return a value cannot be noreturn
    // so we add an if statement to make it look optional
    if TRUE then throw E; end
    return 0;
end

func AlwaysThrow!()
begin
    throw E;
end

func F() => integer
begin
    // All of the following calls are correctly marked and should not report an error.
    let x = NoThrow();
    let y = MayThrow?();
    AlwaysThrow!();
end
