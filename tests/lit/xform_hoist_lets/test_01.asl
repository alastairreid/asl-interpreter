// RUN: %asli --batchmode --exec=:xform_hoist_lets --exec=":show --format=raw FUT" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(i : integer) => boolean
begin
    // Check that lets are not lifted past sequencing points

    // AND
    let r1 = i >= 0 && (__let t : integer = 10 __in i <= t);
    // CHECK: asl_and_bool.0{}(asl_ge_int.0{}(i, 0), __let t : integer = 10 __in asl_le_int.0{}(i, t));
    let r2 = i >= 0 && i <= (__let t : integer = 10 __in t);
    // CHECK: asl_and_bool.0{}(asl_ge_int.0{}(i, 0), __let t : integer = 10 __in asl_le_int.0{}(i, t));

    // OR
    let r3 = i >= 0 || i <= (__let t : integer = 10 __in t);
    // CHECK: asl_or_bool.0{}(asl_ge_int.0{}(i, 0), __let t : integer = 10 __in asl_le_int.0{}(i, t));

    // IMPLIES
    let r4 = i >= 0 --> i <= (__let t : integer = 10 __in t);
    // CHECK: asl_implies_bool.0{}(asl_ge_int.0{}(i, 0), __let t : integer = 10 __in asl_le_int.0{}(i, t));

    // IF expression
    let r5 = if i >= 0 then i <= (__let t : integer = 10 __in t) else FALSE;
    // CHECK: if asl_ge_int.0{}(i, 0) then __let t : integer = 10 __in asl_le_int.0{}(i, t) else FALSE;

    // IF statement
    if __let b1 : boolean = i >= 0 __in b1 then
        let r6 = i <= (__let t : integer = 10 __in t);
    else
        let r7 = i <= (__let t : integer = 10 __in t);
    end
    // CHECK:       let b1 : boolean = asl_ge_int.0{}(i, 0);
    // CHECK-NEXT:  if b1 then
    // CHECK-NEXT:      let t : integer = 10;
    // CHECK-NEXT:      let r6 : boolean = asl_le_int.0{}(i, t);
    // CHECK-NEXT:  else
    // CHECK-NEXT:      let t : integer = 10;
    // CHECK-NEXT:      let r7 : boolean = asl_le_int.0{}(i, t);
    // CHECK-NEXT:  end

    // WHILE loop
    while i <= (__let t : integer = 10 __in t) do
    end
    // CHECK:       while __let t : integer = 10 __in asl_le_int.0{}(i, t) do
    // CHECK:       end

    // REPEAT loop
    repeat
        let x = 42;
    until i <= (__let t : integer = 10 __in t);
    // CHECK:       repeat
    // CHECK-NEXT:      let x : integer{42} = 42;
    // CHECK-NEXT:      let t : integer = 10;
    // CHECK-NEXT:  until asl_le_int.0{}(i, t);

    return r1 && r2 && r3 && r4 && r5;
end

