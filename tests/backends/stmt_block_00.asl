// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    begin
        var r = x;
        return r;
    end
end

func main() => integer
begin
    print_int_dec(Test(0)); println();
    // CHECK: 0
    print_int_dec(Test(1)); println();
    // CHECK: 1
    print_int_dec(Test(3)); println();
    // CHECK: 3

    return 0;
end
