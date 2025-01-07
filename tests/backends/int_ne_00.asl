// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => boolean
begin
    return x != y;
end

func main() => integer
begin
    print(Test(1, 2)); println();
    // CHECK: TRUE
    print(Test(1, 1)); println();
    // CHECK: FALSE
    return 0;
end
