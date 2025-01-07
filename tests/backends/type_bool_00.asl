// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test_id(x : boolean) => boolean
begin
    return x;
end

func Test_not(x : boolean) => boolean
begin
    return !x;
end

func Test_and(x : boolean, y : boolean) => boolean
begin
    return x && y;
end

func Test_or(x : boolean, y : boolean) => boolean
begin
    return x || y;
end

func Test_equiv(x : boolean, y : boolean) => boolean
begin
    return x <-> y;
end

func Test_eq(x : boolean, y : boolean) => boolean
begin
    return x == y;
end

func Test_ne(x : boolean, y : boolean) => boolean
begin
    return x != y;
end

func Test_implies(x : boolean, y : boolean) => boolean
begin
    return x --> y;
end

func main() => integer
begin
    print(TRUE); println();
    // CHECK: TRUE
    print(FALSE); println();
    // CHECK: FALSE

    print(Test_id(TRUE)); println();
    // CHECK: TRUE
    print(Test_id(FALSE)); println();
    // CHECK: FALSE

    print(Test_not(TRUE)); println();
    // CHECK: FALSE
    print(Test_not(FALSE)); println();
    // CHECK: TRUE

    print(Test_and(FALSE, FALSE)); println();
    // CHECK: FALSE
    print(Test_and(FALSE, TRUE)); println();
    // CHECK: FALSE
    print(Test_and(TRUE, FALSE)); println();
    // CHECK: FALSE
    print(Test_and(TRUE, TRUE)); println();
    // CHECK: TRUE

    print(Test_or(FALSE, FALSE)); println();
    // CHECK: FALSE
    print(Test_or(FALSE, TRUE)); println();
    // CHECK: TRUE
    print(Test_or(TRUE, FALSE)); println();
    // CHECK: TRUE
    print(Test_or(TRUE, TRUE)); println();
    // CHECK: TRUE

    print(Test_equiv(FALSE, FALSE)); println();
    // CHECK: TRUE
    print(Test_equiv(FALSE, TRUE)); println();
    // CHECK: FALSE
    print(Test_equiv(TRUE, FALSE)); println();
    // CHECK: FALSE
    print(Test_equiv(TRUE, TRUE)); println();
    // CHECK: TRUE

    print(Test_eq(FALSE, FALSE)); println();
    // CHECK: TRUE
    print(Test_eq(FALSE, TRUE)); println();
    // CHECK: FALSE
    print(Test_eq(TRUE, FALSE)); println();
    // CHECK: FALSE
    print(Test_eq(TRUE, TRUE)); println();
    // CHECK: TRUE

    print(Test_ne(FALSE, FALSE)); println();
    // CHECK: FALSE
    print(Test_ne(FALSE, TRUE)); println();
    // CHECK: TRUE
    print(Test_ne(TRUE, FALSE)); println();
    // CHECK: TRUE
    print(Test_ne(TRUE, TRUE)); println();
    // CHECK: FALSE

    print(Test_implies(FALSE, FALSE)); println();
    // CHECK: TRUE
    print(Test_implies(FALSE, TRUE)); println();
    // CHECK: TRUE
    print(Test_implies(TRUE, FALSE)); println();
    // CHECK: FALSE
    print(Test_implies(TRUE, TRUE)); println();
    // CHECK: TRUE

    return 0;
end
