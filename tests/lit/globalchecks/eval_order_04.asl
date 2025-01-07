// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

var ram1 : __RAM(64);

func I1() => integer
begin
    __InitRAM(64, 1, ram1, Zeros(8));
    return 0;
end

func R1() => bits(8)
begin
    return __ReadRAM(64, 1, ram1, Zeros(64));
end

func W1() => integer
begin
    __WriteRAM(64, 1, ram1, Zeros(64), Zeros(8));
    return 0;
end

var ram2 : __RAM(64);

func I2() => integer
begin
    __InitRAM(64, 1, ram2, Zeros(8));
    return 0;
end

func R2() => bits(8)
begin
    return __ReadRAM(64, 1, ram2, Zeros(64));
end

func W2() => integer
begin
    __WriteRAM(64, 1, ram2, Zeros(64), Zeros(8));
    return 0;
end

func F() => integer
begin
    return W1() + W2();
// CHECK: Type error: expression behaviour depends on evaluation order
end
