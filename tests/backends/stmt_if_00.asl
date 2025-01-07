// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : boolean, y : boolean) => bits(4)
begin
    if x then
        return '0001';
    elsif y then 
        return '0010';
    else
        return '0011';
    end
    return '0100';
end

func main() => integer
begin
    print_bits_hex(Test(TRUE, FALSE)); println();
    // CHECK: 4'x1
    print_bits_hex(Test(TRUE, TRUE)); println();
    // CHECK: 4'x1
    print_bits_hex(Test(FALSE, TRUE)); println();
    // CHECK: 4'x2
    print_bits_hex(Test(FALSE, FALSE)); println();
    // CHECK: 4'x3

    return 0;
end

