// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s
// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..8}) => integer {0..255} begin
    // The node to be transformed is 'hidden' under a node
    // that does not need to be transformed
    return if x < 15 then x + 1 else 255;
end

// Note: the code below could be improved further by not applying
// asl_cvt_sintN_int to both branches of the if-expr.
// We are leaving that improvement for the future.
//
// XFORM-LABEL: func FUT.0{}(x : __sint(5)) => __sint(9)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_cvt_int_sintN.0{9}((if asl_lt_sintN.0{5}(x, i5'x15) then asl_cvt_sintN_int.0{6}(asl_add_sintN.0{6}(asl_resize_sintN.0{5, 6}(x, 6), asl_resize_sintN.0{2, 6}(i2'x1, 6))) else asl_cvt_sintN_int.0{9}(i9'x255)), 9);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(8));
    // CHECK: 9
    return 0;
end
