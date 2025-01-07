// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    case x of
        when -1 => return 2;
        when 1 => return 3;
        when 2 => return 4;
        when 3,4,5 => return 5;
        otherwise => return 10;
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test(-1)); println();
    // CHECK: 2
    print_int_dec(Test(0)); println();
    // CHECK: 10
    print_int_dec(Test(1)); println();
    // CHECK: 3
    print_int_dec(Test(2)); println();
    // CHECK: 4
    print_int_dec(Test(3)); println();
    // CHECK: 5
    print_int_dec(Test(4)); println();
    // CHECK: 5
    print_int_dec(Test(5)); println();
    // CHECK: 5
    print_int_dec(Test(6)); println();
    // CHECK: 10

    return 0;
end
