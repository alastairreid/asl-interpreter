// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

enumeration E { R, G, B };

func Test_eq(x : E) => boolean
begin
    return x == R;
end

func Test_ne(x : E) => boolean
begin
    return x != R;
end

func main() => integer
begin
    print(Test_eq(R)); println();
    // CHECK: TRUE
    print(Test_eq(G)); println();
    // CHECK: FALSE
    print(Test_eq(B)); println();
    // CHECK: FALSE

    print(Test_ne(R)); println();
    // CHECK: FALSE
    print(Test_ne(G)); println();
    // CHECK: TRUE
    print(Test_ne(B)); println();
    // CHECK: TRUE

    return 0;
end
