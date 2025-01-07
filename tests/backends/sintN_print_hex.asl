// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_int_hex(42); println();
    // CHECK: 0x2a
    print_int_hex(-42); println();
    // CHECK: -0x2a
    return 0;
end

