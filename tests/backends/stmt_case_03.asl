// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(4)) => integer
begin
    case x of
        when '0001' => return 3;
        when '0010' => return 4;
        when '1111' => return 5;
        otherwise => return 10;
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test('0000')); println();
    // CHECK: 10
    print_int_dec(Test('0001')); println();
    // CHECK: 3
    print_int_dec(Test('0010')); println();
    // CHECK: 4
    print_int_dec(Test('0011')); println();
    // CHECK: 10
    print_int_dec(Test('1111')); println();
    // CHECK: 5

    return 0;
end
