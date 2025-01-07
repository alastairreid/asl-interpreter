// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : bits(4), y : bits(4)) => bits(8)
begin
    return asl_append_bits(x, y);
end

func main() => integer
begin
    print_bits_hex(Test('1101', '0010')); println();
    // CHECK: 8'xd2
    return 0;
end
