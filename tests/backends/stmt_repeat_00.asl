// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    var s = 0;
    var i = 1;
    repeat
        s = s + i;
        i = i + 1;
    until i > x;
    return s;
end

func main() => integer
begin
    print_int_dec(Test(0)); println();
    // CHECK: 1
    print_int_dec(Test(1)); println();
    // CHECK: 1
    print_int_dec(Test(3)); println();
    // CHECK: 6

    return 0;
end

