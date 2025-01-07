// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => boolean
begin
    return IsPowerOfTwo(x);
end

func main() => integer
begin
    print(Test(0)); println();
    // CHECK: FALSE
    print(Test(1)); println();
    // CHECK: TRUE
    print(Test(2)); println();
    // CHECK: TRUE
    print(Test(3)); println();
    // CHECK: FALSE
    print(Test(4)); println();
    // CHECK: TRUE
    return 0;
end
