// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func F{A}(A : integer, src : bits(A)) => boolean
begin
    var B : integer;
    B = A;
    return Zeros(A) == Zeros(B);
// CHECK: Type error: type width parameter B does not match A
end
