// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : boolean, y : boolean) => bits(4)
begin
    return
        (if x != y then
            '1001'
        else
            '1011'
        );
end

func main() => integer
begin
    print_bits_hex(Test(TRUE, TRUE)); println();
    // CHECK: 4'xb
    print_bits_hex(Test(TRUE, FALSE)); println();
    // CHECK: 4'x9
    print_bits_hex(Test(FALSE, TRUE)); println();
    // CHECK: 4'x9
    print_bits_hex(Test(FALSE, FALSE)); println();
    // CHECK: 4'xb

    return 0;
end
