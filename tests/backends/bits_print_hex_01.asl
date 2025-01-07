// RUN: %aslrun %s | filecheck %s
// REQUIRES: !c23 || wide_bitint
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_bits_hex(256'xff01020304050607_1011121314151617_2021222324252627_3031323334353637); println();
    // CHECK: 256'xff01020304050607101112131415161720212223242526273031323334353637

    return 0;
end
