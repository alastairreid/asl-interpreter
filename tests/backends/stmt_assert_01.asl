// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : bits(4)) => bits(4)
begin
    assert x != '1111';
    return x;
end

func main() => integer
begin
    // assertion that does not fail
    print_bits_hex(FUT('0001')); println();
    // CHECK: 4'x1

    return 0;
end
