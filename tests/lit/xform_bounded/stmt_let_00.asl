// RUN: %aslopt -O0 -Obounded %s | filecheck --check-prefix=XFORM %s
// RUN: %aslrun -O0 -Obounded %s | filecheck %s
// Copyright (C) 2024-2024 Intel Corporation

func FUT(x : integer {0..10}) => integer {0..10}
begin
    let y = x;
    return y;
end

// still not what we want to see - look at 'let y ...'
// XFORM:       func FUT.0{}(x : __sint(5)) => __sint(5)
// XFORM-NEXT:  begin
// XFORM-NEXT:      let y : __sint(5) = x;
// XFORM-NEXT:      return y;
// XFORM-NEXT:  end

func main() => integer
begin
    print(FUT(5));
    // CHECK: 5
    return 0;
end
