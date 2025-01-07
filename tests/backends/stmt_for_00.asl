// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    var s = 0;
    for i = 0 to x do
        s = s + i;
    end
    return s;
end

func main() => integer
begin
    print_int_dec(Test(0)); println();
    // CHECK: 0
    print_int_dec(Test(1)); println();
    // CHECK: 1
    print_int_dec(Test(3)); println();
    // CHECK: 6

    return 0;
end
