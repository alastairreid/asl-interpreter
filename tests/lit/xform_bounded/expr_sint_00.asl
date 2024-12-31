// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : bits(8)) => integer {-128..127}
begin
    return asl_cvt_bits_sint(x);
end

// XFORM-LABEL: func FUT.0{}(x : bits(8)) => __sint(8)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_cvt_bits_ssintN.0{8}(x);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT('1110 0111')); println();
    // CHECK: -25
    return 0;
end
