// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

// Based on UnsignedSat
func FUT(x : integer) => integer {0..255}
begin
    let r = if x >= 256 then 255
            elsif x < 0 then 0
            else x;
    return r as {0..255};
end

// Note: the code below could be improved further by not applying
// asl_cvt_sintN_int to both branches of the if-expr.
// We are leaving that improvement for the future.
//
// XFORM-LABEL: func FUT.0{}(x : integer) => __sint(9)
// XFORM-NEXT:  begin
// XFORM-NEXT:      let r : integer = (if asl_ge_int.0{}(x, asl_cvt_sintN_int.0{10}(i10'x256)) then asl_cvt_sintN_int.0{9}(i9'x255) elsif asl_lt_int.0{}(x, asl_cvt_sintN_int.0{1}(i1'x0)) then asl_cvt_sintN_int.0{1}(i1'x0) else x);
// XFORM-NEXT:      return asl_cvt_int_sintN.0{9}(r, 9);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(-1));
    // CHECK: 0
    print_int_dec(FUT(0));
    // CHECK: 0
    print_int_dec(FUT(1));
    // CHECK: 1
    print_int_dec(FUT(255));
    // CHECK: 255
    print_int_dec(FUT(256));
    // CHECK: 255
    return 0;
end
