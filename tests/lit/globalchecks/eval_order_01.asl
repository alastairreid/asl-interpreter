// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var X : integer;

func WX() => integer
begin
    X = 1;
    return 0;
end

func RX() => integer
begin
    return X;
end

func F() => integer
begin
    return WX() + RX();
// CHECK: Type error: expression behaviour depends on evaluation order
end
