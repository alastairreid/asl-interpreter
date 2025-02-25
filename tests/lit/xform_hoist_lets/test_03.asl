// RUN: %asli --batchmode --no-runtime-checks --exec=:xform_hoist_lets --exec=":show --format=raw FUT" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(i : integer) => boolean
begin
    // Check that asserts are not lifted past sequencing points
    // AND
    let r1 = i >= 0 && (__assert i MOD 2 == 1 __in i <= 10);
    // CHECK:       asl_and_bool.0{}(asl_ge_int.0{}(i, 0), __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10));
    let r2 = i >= 0 && i <= (__assert i MOD 2 == 1 __in 10);
    // CHECK:       asl_and_bool.0{}(asl_ge_int.0{}(i, 0), __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10));

    // OR
    let r3 = i >= 0 || i <= (__assert i MOD 2 == 1 __in 10);
    // CHECK:       asl_or_bool.0{}(asl_ge_int.0{}(i, 0), __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10));

    // IMPLIES
    let r4 = i >= 0 --> i <= (__assert i MOD 2 == 1 __in 10);
    // CHECK:       asl_implies_bool.0{}(asl_ge_int.0{}(i, 0), __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10));

    // IF expression
    let r5 = if i >= 0 then i <= (__assert i MOD 2 == 1 __in 10) else FALSE;
    // CHECK:       if asl_ge_int.0{}(i, 0) then __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10) else FALSE;

    // IF statement
    if __let b1 : boolean = i >= 0 __in b1 then
        let r6 = i <= (__assert i MOD 2 == 1 __in 10);
    else
        let r7 = i <= (__assert i MOD 2 == 1 __in 10);
    end
    // CHECK:       let b1 : boolean = asl_ge_int.0{}(i, 0);
    // CHECK-NEXT:  if b1 then
    // CHECK-NEXT:      assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1);
    // CHECK-NEXT:      let r6 : boolean = asl_le_int.0{}(i, 10);
    // CHECK-NEXT:  else
    // CHECK-NEXT:      assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1);
    // CHECK-NEXT:      let r7 : boolean = asl_le_int.0{}(i, 10);
    // CHECK-NEXT:  end

    // WHILE loop
    while i <= (__assert i MOD 2 == 1 __in 10) do
    end
    // CHECK:       while __assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1) __in asl_le_int.0{}(i, 10) do
    // CHECK:       end

    // REPEAT loop
    repeat
        let x = 42;
    until i <= (__assert i MOD 2 == 1 __in 10);
    // CHECK:       repeat
    // CHECK-NEXT:      let x : integer{42} = 42;
    // CHECK-NEXT:      assert asl_eq_int.0{}(asl_frem_int.0{}(i, 2), 1);
    // CHECK-NEXT:  until asl_le_int.0{}(i, 10);

    return r1 && r2 && r3 && r4 && r5;
end

