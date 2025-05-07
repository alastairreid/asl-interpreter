// RUN: %asli --batchmode --runtime-checks --exec=":show --format=raw FUT*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT1(x : bits(4), y : integer, z : string, b : boolean)
begin
    Print("x={x} y={y} z={z} b={b}");
end

// CHECK:      func FUT1.0{}(x : bits(4), y : integer, z : string, b : boolean)
// CHECK_NEXT: begin
// CHECK_NEXT:     print_str.0("x=");
// CHECK_NEXT:     print_bits_hex.0(x);
// CHECK_NEXT:     print_str.0(" y=");
// CHECK_NEXT:     print_int_dec.0(y);
// CHECK_NEXT:     print_str.0(" z=");
// CHECK_NEXT:     print_str.0(z);
// CHECK_NEXT:     print_str.0(" b=");
// CHECK_NEXT:     asl_print_bool.0(b);
// CHECK_NEXT: end
