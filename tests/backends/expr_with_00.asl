// RUN: not %aslrun --runtime-check %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT_8_4(x : bits(8), i : integer, y : bits(4)) => bits(8)
begin
    return x with { [i +: 4] = y };
end

func main() => integer
begin
    print_bits_hex(FUT_8_4('0000 0000', 5, '1111')); println();
    // CHECK: Evaluation error: assertion failure
    return 0;
end
