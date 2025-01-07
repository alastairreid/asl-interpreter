// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_sintN_dec(i8'd42); println();
    // CHECK: i8'd42
    print_sintN_dec(-i8'd42); println();
    // CHECK: -i8'd42

    print_sintN_dec(i8'd127); println();
    // CHECK: i8'd127
    print_sintN_dec(-i8'd127); println();
    // CHECK: -i8'd127
    print_sintN_dec(-i8'd128); println();
    // CHECK: -i8'd128
    return 0;
end
