// RUN: not %iii --batchmode %s | filecheck %s
// Copyright (C) 2023-2026 Intel Corporation

config CFG : Integer := 1;

function F()
begin
    CFG := 2;
// CHECK: Type error: assignment to immutable variable `CFG` declared at
end
