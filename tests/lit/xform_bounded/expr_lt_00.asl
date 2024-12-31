// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..15}, y : integer {0..15}) => boolean
begin
    return x < y;
end

// XFORM-LABEL: func FUT.0{}(x : __sint(5), y : __sint(5)) => boolean
// XFORM-NEXT:  begin
// XFORM-NEXT:      return asl_lt_sintN.0{5}(x, y);
// XFORM-NEXT:  end

func main() => integer
begin
    print(FUT(13, 14));
    // CHECK: TRUE
    print(FUT(14, 14));
    // CHECK: FALSE
    print(FUT(15, 14));
    // CHECK: FALSE
    return 0;
end
