// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test1(x : bits(3)) => bits(8)
begin
    return asl_sign_extend_bits(x, 8);
end

// checking for problems when representation is different size
func Test2(x : bits(64)) => bits(128)
begin
    return asl_sign_extend_bits(x, 128);
end

// checking for boundary problems
func Test3(x : bits(63)) => bits(64)
begin
    return asl_sign_extend_bits(x, 64);
end

// checking for boundary problems
func Test4(x : bits(64)) => bits(65)
begin
    return asl_sign_extend_bits(x, 65);
end

func main() => integer
begin
    // Basic check
    print_bits_hex(Test1('001')); println();
    // CHECK: 8'x1
    print_bits_hex(Test1('011')); println();
    // CHECK: 8'x3
    print_bits_hex(Test1('101')); println();
    // CHECK: 8'xfd
    print_bits_hex(Test1('111')); println();
    // CHECK: 8'xff

    // Change of size of representation
    print_bits_hex(Test2(64'x0123_4567_89ab_cdef)); println();
    // CHECK: 128'x123456789abcdef
    print_bits_hex(Test2(64'x4123_4567_89ab_cdef)); println();
    // CHECK: 128'x4123456789abcdef
    print_bits_hex(Test2(64'x8123_4567_89ab_cdef)); println();
    // CHECK: 128'xffffffffffffffff8123456789abcdef
    print_bits_hex(Test2(64'xc123_4567_89ab_cdef)); println();
    // CHECK: 128'xffffffffffffffffc123456789abcdef

    // Boundary checks
    print_bits_hex(Test3(63'x0123_4567_89ab_cdef)); println();
    // CHECK: 64'x123456789abcdef
    print_bits_hex(Test3(63'x2123_4567_89ab_cdef)); println();
    // CHECK: 64'x2123456789abcdef
    print_bits_hex(Test3(63'x4123_4567_89ab_cdef)); println();
    // CHECK: 64'xc123456789abcdef
    print_bits_hex(Test3(63'x6123_4567_89ab_cdef)); println();
    // CHECK: 64'xe123456789abcdef

    // Boundary checks
    print_bits_hex(Test4(64'x0123_4567_89ab_cdef)); println();
    // CHECK: 65'x123456789abcdef
    print_bits_hex(Test4(64'x4123_4567_89ab_cdef)); println();
    // CHECK: 65'x4123456789abcdef
    print_bits_hex(Test4(64'x8123_4567_89ab_cdef)); println();
    // CHECK: 65'x18123456789abcdef
    print_bits_hex(Test4(64'xc123_4567_89ab_cdef)); println();
    // CHECK: 65'x1c123456789abcdef

    return 0;
end
