// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s
// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..15}) => integer {0..100}
begin
    var s : integer {0..100} = 0;
    for i : integer {0..15} = 0 to x do
        s = (s + i) as {0..100};
    end
    return s;
end

// XFORM:       func FUT.0{}(x : __sint(5)) => __sint(8)
// XFORM-NEXT:  begin
// XFORM-NEXT:      var s : __sint(8) = asl_resize_sintN.0{1, 8}(i1'x0, 8);
// XFORM-NEXT:       for i : __sint(6) = asl_resize_sintN.0{1, 6}(i1'x0, 6) to asl_resize_sintN.0{5, 6}(x, 6) do
// XFORM-NEXT:           s = asl_resize_sintN.0{9, 8}(asl_add_sintN.0{9}(asl_resize_sintN.0{8, 9}(s, 9), asl_resize_sintN.0{5, 9}(i, 9)), 8);
// XFORM-NEXT:       end
// XFORM-NEXT:      return s;
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(4)); println();
    // CHECK: 10
    return 0;
end
