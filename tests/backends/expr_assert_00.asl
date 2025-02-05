// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : bits(8), i : integer) => bit
begin
    return __assert 0 <= i && i < 8 __in x[i +: 1];
end

func main() => integer
begin
    // assertion that does fail
    print_bits_hex(FUT('0000 1111', 9)); println();
    // CHECK: Evaluation error: assertion failure

    return 0;
end
