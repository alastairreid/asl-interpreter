// RUN: not %asli --max-errors=10 --check-exception-markers --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func T() => integer
begin
// CHECK: Type error: Function definition 'T.0' should return a value but does not
end

