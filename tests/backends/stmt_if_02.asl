// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : boolean, y : boolean) => bits(4)
begin
    var r = '0000';
    if x then
        r = '0001';
    elsif y then
        r = '0010';
    else
        r = '0011';
    end
    return r;
end

func main() => integer
begin
    print_bits_hex(Test(FALSE, FALSE)); println();
    // CHECK: 4'x3
    print_bits_hex(Test(FALSE, TRUE)); println();
    // CHECK: 4'x2
    print_bits_hex(Test(TRUE, FALSE)); println();
    // CHECK: 4'x1
    print_bits_hex(Test(TRUE, TRUE)); println();
    // CHECK: 4'x1

    return 0;
end
