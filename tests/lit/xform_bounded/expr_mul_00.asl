// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2025-2025 Intel Corporation

func FUT1(x : integer {-1..1}, y : integer {-2..3}) => integer {-3..3}
begin
    return x * y;
end

func FUT2(x : integer {0..2}, y : integer {0..7}) => integer {0..14}
begin
    return x * y;
end

func FUT3(x : integer {-8..0}, y : integer {-8..0}) => integer {0..64}
begin
    return x * y;
end

func FUT4(x : integer {-8..1}, y : integer {-8..1}) => integer {-8..64}
begin
    return x * y;
end

// XFORM-LABEL: func FUT4.0{}(x : __sint(4), y : __sint(4)) => __sint(8)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_mul_sintN.0{8}(asl_resize_sintN.0{4, 8}(x, 8), asl_resize_sintN.0{4, 8}(y, 8));
// XFORM-NEXT:  end

// XFORM-LABEL: func FUT3.0{}(x : __sint(4), y : __sint(4)) => __sint(8)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_mul_sintN.0{8}(asl_resize_sintN.0{4, 8}(x, 8), asl_resize_sintN.0{4, 8}(y, 8));
// XFORM-NEXT:  end

// XFORM-LABEL: func FUT2.0{}(x : __sint(3), y : __sint(4)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_resize_sintN.0{7, 5}(asl_mul_sintN.0{7}(asl_resize_sintN.0{3, 7}(x, 7), asl_resize_sintN.0{4, 7}(y, 7)), 5);
// XFORM-NEXT:  end

// XFORM-LABEL: func FUT1.0{}(x : __sint(2), y : __sint(3)) => __sint(3)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_resize_sintN.0{5, 3}(asl_mul_sintN.0{5}(asl_resize_sintN.0{2, 5}(x, 5), asl_resize_sintN.0{3, 5}(y, 5)), 3);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT1(-1, -2)); println();
    // CHECK: 2
    print_int_dec(FUT2(2, 7)); println();
    // CHECK: 14
    print_int_dec(FUT3(-8, -1)); println();
    // CHECK: 8
    print_int_dec(FUT4(-8, 1)); println();
    // CHECK: -8
    return 0;
end
