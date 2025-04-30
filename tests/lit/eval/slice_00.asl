// RUN: %asli --batchmode %s '--exec=F()' | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

type Flags of bits(4) {
    [0] A
    [1] B
    [2] C
    [3] D
};

func F() => Flags
begin
    var r : Flags = Zeros(4);
    // These assignments used to fail because the type of r is a
    // typedef instead of either bits(N) or integer
    r.A = '1';
    r.C = '1';
    return r;
end

// CHECK: 4'x5
