// RUN: %asli --batchmode %s
// Copyright (C) 2023-2025 Intel Corporation

type TwoHalves of bits(16) {
    [7:0]   LO
    [15:8]  HI
};

func IsZeroHalf(x : bits(16)) => boolean
begin
    // This tests for an old bug where r was given the type of x, not TwoHalves
    var r : TwoHalves = x;
    return IsZero(r.LO);
end

func ZeroHalf(x : bits(16)) => TwoHalves
begin
    // This tests for an old bug where r was given the type of x, not TwoHalves
    var r : TwoHalves = x;
    r.LO = Zeros(8);
    return r;
end

func IsZeroHalf_(x : bits(16)) => boolean
begin
    // Variant of IsZeroHalf that (pointlessly) casts and then discards x
    var _ : TwoHalves = x;
    return TRUE;
end
