// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test0(w : integer) => bits(0)
begin
    return asl_mk_mask(w, 0);
end

func Test8(w : integer) => bits(8)
begin
    return asl_mk_mask(w, 8);
end

func main() => integer
begin
    print_bits_hex(Test0(0)); println();
    // CHECK: 0'x0
    print_bits_hex(Test8(3)); println();
    // CHECK: 8'x7
    return 0;
end
