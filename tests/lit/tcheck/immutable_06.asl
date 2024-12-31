// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

config CFG : integer = 1;

func F()
begin
    CFG = 2;
// CHECK: Type error: assignment to immutable variable `CFG` declared at
end
