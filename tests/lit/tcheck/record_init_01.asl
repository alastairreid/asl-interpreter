// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

record R(M) {
    x : bits(M);
};

func S4(r : R(4)) => bits(4)
begin
    return r.x;
end

func T()
begin
    let t = S4(R{x='111'});
// CHECK: Type error: wrong number of type parameters
end
