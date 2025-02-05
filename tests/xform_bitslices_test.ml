(****************************************************************
 * Test bitslice lowering transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test bitslice lowering
 ****************************************************************)

let bitslice_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_bitslices.xform_expr globals prelude in
  let stmts = test_xform_stmts Xform_bitslices.xform_stmts globals prelude in
  [
    ("combine (lo+:wd)", `Quick, expr
       "var x : bits(64); var y : bits(64); var i : integer;"
       "[x[i +: 64-i], y[0 +: i]]"
       "asl_lsl_bits(asl_lsr_bits(x, i) AND asl_mk_mask(64-i, 64), i)
        OR asl_and_bits(y, asl_mk_mask(i, 64))");
    ("bitvector concat", `Quick, expr
       "var x : bits(2); var y : bits(2);"
       "[x, y]"
       "asl_lsl_bits(asl_zero_extend_bits(x, 4), 2) OR asl_zero_extend_bits(y, 4)");
    ("nested bitvector concat 1", `Quick, expr
       "var x : bits(2); var y : bits(2); var z : bits(4);"
       "[[x, y], z]"
       "asl_lsl_bits(asl_zero_extend_bits(asl_lsl_bits(asl_zero_extend_bits(x, 4), 2)
                                  OR asl_zero_extend_bits(y, 4), 8), 4)
        OR asl_zero_extend_bits(z, 8)");
    ("nested bitvector concat 2", `Quick, expr
       "var x : bits(2); var y : bits(2);"
       "ZeroExtend([x, y], 8)"
       "asl_zero_extend_bits(asl_lsl_bits(asl_zero_extend_bits(x, 4), 2)
                         OR asl_zero_extend_bits(y, 4), 8)");
    ("Ones() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i+1] = Ones(i+1);"
       "x = asl_and_bits(x, NOT asl_mk_mask(asl_add_int(i, 1), 64))
            OR asl_mk_mask(asl_add_int(i, 1), 64);");
    ("Ones() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Ones(i);"
       "x = asl_and_bits(x, NOT asl_lsl_bits(asl_mk_mask(i, 64), 1))
            OR asl_lsl_bits(asl_mk_mask(i, 64), 1);");
    ("Zeros() 1 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[0 +: i + 1] = Zeros(i+1);"
       "x = x AND NOT asl_mk_mask(asl_add_int(i, 1), 64);");
    ("Zeros() 2 lo+:width", `Quick, stmts
       "var x : bits(64); var i : integer;"
       "x[1 +: i] = Zeros(i);"
       "x = x AND NOT asl_lsl_bits(asl_mk_mask(i, 64), 1);");
    ("assignment to register bitslice", `Quick, stmts
       "var x : bits(8) { [0] i [7 : 1] j }; var y : bits(7);"
       "x[1 +: 7] = y;"
       "x = asl_and_bits(x, NOT asl_lsl_bits(asl_mk_mask(7, 8), 1))
            OR asl_lsl_bits(asl_zero_extend_bits(y, 8), 1);");
    ("assignment to array element bitslice", `Quick, stmts
       "var x : array [1] of array [1] of bits(8); var y : bits(7);"
       "x[0][0][1 +: 7] = y;"
       "x[0][0] = asl_and_bits(x[0][0], NOT asl_lsl_bits(asl_mk_mask(7, 8), 1))
                  OR asl_lsl_bits(asl_zero_extend_bits(y, 8), 1);");
    ("assignment to field bitslice", `Quick, stmts
       "record J { j : bits(8); }; record I { i : J; }; var x : I; var y : bits(7);"
       "x.i.j[1 +: 7] = y;"
       "x.i.j = asl_and_bits(x.i.j, NOT asl_lsl_bits(asl_mk_mask(7, 8), 1))
                OR asl_lsl_bits(asl_zero_extend_bits(y, 8), 1);");
    ("integer bitslice", `Quick, stmts
       "var result : bits(32); var x : bits(8);"
       "let r = CountLeadingZeroBits(x);
        result[0 +: 8] = r[0 +: 8];"
       "let r = CountLeadingZeroBits(x);
        result = asl_and_bits(result, NOT asl_mk_mask(8, 32))
                 OR asl_zero_extend_bits(r[0 +: 8], 32);");
    ("bitslice, width of r > width of l", `Quick, stmts
       "var l : bits(8); var r : bits(16);"
       "l[1 +: 7] = r[1 +: 7];"
       "l = asl_and_bits(l, NOT asl_lsl_bits(asl_mk_mask(7, 8), 1))
            OR asl_lsl_bits(asl_zero_extend_bits(r[1 +: 7], 8), 1);");
    ("assignment of bitvector concat", `Quick, stmts
       "var x : bits(8); var y : bits(3); var z : bits(4);"
       "x[1 +: 7] = [y, z];"
       "x = asl_and_bits(x, NOT asl_lsl_bits(asl_mk_mask(7, 8), 1))
            OR asl_lsl_bits(asl_zero_extend_bits(asl_lsl_bits(asl_zero_extend_bits(y, 7), 4)
                                         OR asl_zero_extend_bits(z, 7), 8), 1);");
    ("ZeroExtend(Ones(i), n)", `Quick, expr
       "var i : integer;"
       "ZeroExtend(Ones(i), 64)"
       "asl_mk_mask(i, 64)");
    ("[Ones(i), Zeros(n-i)]", `Quick, expr
       "var i : integer;"
       "[Ones(i), Zeros(64-i)]"
       "asl_lsl_bits(asl_mk_mask(i, 64), asl_add_int(64, -i))");
    ("IsZero(e[i +: w])", `Quick, expr
       "var x : bits(32); var w : integer;"
       "IsZero(x[0 +: w])"
       "asl_and_bits(x, asl_mk_mask(w, 32)) == asl_zeros_bits(32)");
    ("IsOnes(e[i +: w])", `Quick, expr
       "var x : bits(32); var w : integer;"
       "IsOnes(x[0 +: w])"
       "asl_and_bits(NOT x, asl_mk_mask(w, 32)) == asl_zeros_bits(32)");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("bitslice", bitslice_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
