// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : bits(4)) => bits(4)
begin
    assert x != '1111';
    return x;
end

func main() => integer
begin
    // assertion that does fail
    print_bits_hex(FUT('1111')); println();
    // CHECK: Evaluation error: assertion failure

    return 0;
end
