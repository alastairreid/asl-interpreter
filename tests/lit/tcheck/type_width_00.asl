// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F{A}(A : integer, src : bits(A))
begin
end

func Test()
begin
    let src = Zeros(10);
    F(5, src);
// CHECK: Type error: type width parameter 10 does not match 5
end
