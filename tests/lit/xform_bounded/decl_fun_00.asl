// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s

// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..10}) => integer {0..10}
begin
    return x;
end

// XFORM-LABEL: func FUT.0{}(x : __sint(5)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      return x;
// XFORM-NEXT:  end

func main() => integer
begin
    print_int_dec(FUT(5));
    // CHECK: 5
    return 0;
end
