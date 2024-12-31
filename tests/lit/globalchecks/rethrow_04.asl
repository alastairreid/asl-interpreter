// RUN: not %asli --max-errors=10 --check-exception-markers --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

type E of exception;

func T()
begin
    throw E;
// CHECK: Type error: Function definition 'T.0' always throws an exception but is not marked with '!'
end

func F()
begin
    T();
// CHECK: Type error: call to procedure `T.0` should be marked with `?` or `!` because it can throw an exception
end
