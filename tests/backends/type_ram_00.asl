// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

// A RAM block with 8-bit addresses
// RAMs should always be declared as global variables
var __Memory : __RAM(8);

func Read1(address : bits(8)) => bits(8)
begin
    return asl_ram_read(8, 1, __Memory, address);
end

func Read8(address : bits(8)) => bits(64)
begin
    return asl_ram_read(8, 8, __Memory, address);
end

func Write1(address : bits(8), value : bits(8))
begin
    asl_ram_write(8, 1, __Memory, address, value);
end

func Write4(address : bits(8), value : bits(32))
begin
    asl_ram_write(8, 4, __Memory, address, value);
end

func main() => integer
begin
    asl_ram_init(8, __Memory, 64'xf0ccac1adeadbeef);

    // Initial values - set to 0xf0ccac1a_dead_beef
    print_bits_hex(Read1(8'd0)); println();
    // CHECK: 8'xef
    print_bits_hex(Read1(8'd1)); println();
    // CHECK: 8'xbe
    print_bits_hex(Read1(8'd2)); println();
    // CHECK: 8'xad
    print_bits_hex(Read1(8'd3)); println();
    // CHECK: 8'xde
    print_bits_hex(Read1(8'd4)); println();
    // CHECK: 8'x1a
    print_bits_hex(Read1(8'd5)); println();
    // CHECK: 8'xac
    print_bits_hex(Read1(8'd6)); println();
    // CHECK: 8'xcc
    print_bits_hex(Read1(8'd7)); println();
    // CHECK: 8'xf0
    print_bits_hex(Read1(8'd8)); println();
    // CHECK: 8'xef
    print_bits_hex(Read1(8'd9)); println();
    // CHECK: 8'xbe

    // Multibyte read - need not be aligned
    print_bits_hex(Read8(8'd0)); println();
    // CHECK: 64'xf0ccac1adeadbeef
    print_bits_hex(Read8(8'd1)); println();
    // CHECK: 64'xeff0ccac1adeadbe
    print_bits_hex(Read8(8'd2)); println();
    // CHECK: 64'xbeeff0ccac1adead

    Write1(8'd5, 8'x00);

    print_bits_hex(Read8(8'd0)); println();
    // CHECK: 64'xf0cc001adeadbeef
    print_bits_hex(Read8(8'd8)); println();
    // CHECK: 64'xf0ccac1adeadbeef

    Write4(8'd7, 32'x11223344);

    print_bits_hex(Read8(8'd0)); println();
    // CHECK: 64'x44cc001adeadbeef
    print_bits_hex(Read8(8'd8)); println();
    // CHECK: 64'xf0ccac1ade112233

    return 0;
end
