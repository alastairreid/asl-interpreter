// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..15}) => integer {-15..0}
begin
    return -x;
end

// Note: the analysis is slightly imprecise: a 6-bit negation is performed when
// 5 bits would be sufficient.
// This will probably result in good code though because LLVM is more than
// capable of further refining the size based on the available information.
// XFORM-LABEL: func FUT.0{}(x : __sint(5)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_resize_sintN.0{6, 5}(asl_neg_sintN.0{6}(asl_resize_sintN.0{5, 6}(x, 6)), 5);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(14));
    // CHECK: -14
    return 0;
end
