// RUN: %asli --batchmode %s
// Copyright (C) 2023-2025 Intel Corporation

var X : integer;
var Y : integer;

func WX() => integer
begin
    X = 1;
    return 0;
end

func RX() => integer
begin
    return X;
end

func WY() => integer
begin
    Y = 1;
    return 0;
end

func RY() => integer
begin
    return Y;
end

func F() => integer
begin
    // writing different globals is ok
    let x = WX() + WY();
    // reading and writing different globals is ok
    let y = WX() + RY();
    // reading the same global is ok
    let z = RX() + RX();
    return x + y + z;
end
