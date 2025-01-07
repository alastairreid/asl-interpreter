// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(3)) => boolean
begin
    return x IN '10x';
end

func main() => integer
begin
    print(Test('000')); println();
    // CHECK: FALSE
    print(Test('001')); println();
    // CHECK: FALSE
    print(Test('010')); println();
    // CHECK: FALSE
    print(Test('011')); println();
    // CHECK: FALSE
    print(Test('100')); println();
    // CHECK: TRUE
    print(Test('101')); println();
    // CHECK: TRUE
    print(Test('110')); println();
    // CHECK: FALSE
    print(Test('111')); println();
    // CHECK: FALSE

    return 0;
end

