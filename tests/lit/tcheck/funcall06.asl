// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : integer, y : integer = 1, z : integer = 2) => integer
begin
    return x * 0x100 + y * 0x10 + z;
end

func main() => integer
begin
    print(F(9)); println();
    // CHECK: 912
    print(F(9, y=4)); println();
    // CHECK: 942
    print(F(9, z=5)); println();
    // CHECK: 915
    print(F(9, y=4, z=5)); println();
    // CHECK: 945
    print(F(9, z=4, y=5)); println();
    // CHECK: 954

    return 0;
end
