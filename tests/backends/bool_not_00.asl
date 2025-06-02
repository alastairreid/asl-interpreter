// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : boolean) => bits(4)
begin
    return
        (if !x then
            '1001'
        else
            '1011'
        );
end

func main() => integer
begin
    print_bits_hex(Test(TRUE)); println();
    // CHECK: 4'xb
    print_bits_hex(Test(FALSE)); println();
    // CHECK: 4'x9

    return 0;
end
