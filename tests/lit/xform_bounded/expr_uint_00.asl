// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : bits(8)) => integer {0..255}
begin
    return asl_cvt_bits_uint(x);
end

// XFORM-LABEL: func FUT.0{}(x : bits(8)) => __sint(9)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_resize_sintN.0{8, 9}(asl_cvt_bits_usintN.0{8}(x), 9);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_hex(FUT('1110 0111')); println();
    // CHECK: 0xe7
    return 0;
end
