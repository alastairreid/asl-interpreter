// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT_8_4(x : bits(8), i : integer) => bits(4)
begin
    return x[i +: 4];
end

func main() => integer
begin
    print_bits_hex(FUT_8_4('1010 0101', 5)); println();
    // CHECK: Evaluation error: assertion failure
    return 0;
end
