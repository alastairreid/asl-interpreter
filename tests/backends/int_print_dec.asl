// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_int_dec(42); println();
    // CHECK: 42
    print_int_dec(-42); println();
    // CHECK: -42
    return 0;
end
