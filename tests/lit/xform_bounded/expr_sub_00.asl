// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..14}) => integer {-1..13}
begin
    return x - 1;
end

// XFORM-LABEL: func FUT.0{}(x : __sint(5)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_resize_sintN.0{6, 5}(asl_sub_sintN.0{6}(asl_resize_sintN.0{5, 6}(x, 6), asl_resize_sintN.0{2, 6}(i2'x1, 6)), 5);
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(14));
    // CHECK: 13
    return 0;
end

