(****************************************************************
 * Test Bitslice lowering transform
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open Isa_utils
module TC = Tcheck

(****************************************************************
 * Test Bitslice lowering
 ****************************************************************)

let bitslice_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_bitslices.xform_expr globals prelude in
  let stmts = test_xform_stmts Xform_bitslices.xform_stmts globals prelude in
  [
      (*
    ("combine (lo+:wd)", `Quick, expr
       "var x : Bits(64); var y : Bits(64); var i : Integer;"
       "x[i +: 64-i] ++ y[0 +: i]"
       "Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Shift_Right_Logical_Restricted(x, i) and Std::Bits::Mk_Mask(64-i, 64), i)
        or Std::Bits::And(y, Std::Bits::Mk_Mask(i, 64))");
    ("nested bitvector concat 1", `Quick, expr
       "var x : Bits(2); var y : Bits(2); var z : Bits(4);"
       "(x ++ y) ++ z"
       "Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(x, 8), 2)
                     or Std::Bits::Zero_Extend(y, 8), 4)
        or Std::Bits::Zero_Extend(z, 8)");
    ("nested bitvector concat 2", `Quick, expr
       "var x : Bits(2); var y : Bits(2);"
       "Zero_Extend(x ++ y, 8)"
       "Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(x, 8), 2) or Std::Bits::Zero_Extend(y, 8)");
        *)
    ("All_Ones() 1 lo+:width", `Quick, stmts
       "var x : Bits(64); var i : Integer;"
       "x[0 +: i+1] := Std::Bits::All_Ones(i+1);"
       "x := Std::Bits::And(x, not Std::Bits::Mk_Mask(Std::Integer::Add(i, 1), 64))
            or Std::Bits::Mk_Mask(Std::Integer::Add(i, 1), 64);");
    ("All_Ones() 2 lo+:width", `Quick, stmts
       "var x : Bits(64); var i : Integer;"
       "x[1 +: i] := Std::Bits::All_Ones(i);"
       "x := Std::Bits::And(x, not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(i, 64), 1))
            or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(i, 64), 1);");
    ("Zero() 1 lo+:width", `Quick, stmts
       "var x : Bits(64); var i : Integer;"
       "x[0 +: i + 1] := Std::Bits::Zero(i+1);"
       "x := x and not Std::Bits::Mk_Mask(Std::Integer::Add(i, 1), 64);");
    ("Zero() 2 lo+:width", `Quick, stmts
       "var x : Bits(64); var i : Integer;"
       "x[1 +: i] := Std::Bits::Zero(i);"
       "x := x and not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(i, 64), 1);");
    ("assignment to register Bitslice", `Quick, stmts
       "bitfield T = { i => [0], j => [7 : 1] } : Bits(8); var x : T; var y : Bits(7);"
       "x[1 +: 7] := y;"
       "x := Std::Bits::And(x, not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(7, 8), 1))
            or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(y, 8), 1);");
    ("assignment to array element Bitslice", `Quick, stmts
       "var x : array [1] of array [1] of Bits(8); var y : Bits(7);"
       "x[0][0][1 +: 7] := y;"
       "x[0][0] := Std::Bits::And(x[0][0], not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(7, 8), 1))
                  or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(y, 8), 1);");
    ("assignment to field Bitslice", `Quick, stmts
       "record J { j : Bits(8); }; record I { i : J; }; var x : I; var y : Bits(7);"
       "x.i.j[1 +: 7] := y;"
       "x.i.j := Std::Bits::And(x.i.j, not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(7, 8), 1))
                or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(y, 8), 1);");
    ("Integer Bitslice", `Quick, stmts
       "var result : Bits(32); var x : Bits(8);"
       "let r := Std::Bits::Count_Leading_Zero_Bits(x);
        result[0 +: 8] := r[0 +: 8];"
       "let r := Std::Bits::Count_Leading_Zero_Bits(x);
        result := Std::Bits::And(result, not Std::Bits::Mk_Mask(8, 32))
                 or Std::Bits::Zero_Extend(r[0 +: 8], 32);");
    ("Bitslice, width of r > width of l", `Quick, stmts
       "var l : Bits(8); var r : Bits(16);"
       "l[1 +: 7] := r[1 +: 7];"
       "l := Std::Bits::And(l, not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(7, 8), 1))
            or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(r[1 +: 7], 8), 1);");
    (*
    ("assignment of bitvector concat", `Quick, stmts
       "var x : Bits(8); var y : Bits(3); var z : Bits(4);"
       "x[1 +: 7] := [y, z];"
       "x := (x and not Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(7, 8), 1))
            or Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Zero_Extend(y, 8), 4)
                                             or Std::Bits::Zero_Extend(z, 8)
                                            , 1);");
    ("All_Ones(i) ++ Zero(n-i)", `Quick, expr
       "var i : Integer;"
       "Std::Bits::All_Ones(i) ++ Std::Bits::Zero(64-i)"
       "Std::Bits::Shift_Left_Logical_Restricted(Std::Bits::Mk_Mask(i, 64), Std::Integer::Add(64, -i))");
    *)
    ("Is_Zero(e[i +: w])", `Quick, expr
       "var x : Bits(32); var w : Integer;"
       "Is_Zero(x[0 +: w])"
       "Std::Bits::And(x, Std::Bits::Mk_Mask(w, 32)) == Std::Bits::Zero(32)");
    ("Is_All_Ones(e[i +: w])", `Quick, expr
       "var x : Bits(32); var w : Integer;"
       "Std::Bits::Is_All_Ones(x[0 +: w])"
       "Std::Bits::And(not x, Std::Bits::Mk_Mask(w, 32)) == Std::Bits::Zero(32)");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("Bitslice", bitslice_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
