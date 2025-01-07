(****************************************************************
 * Test lowering transform
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

let bitslices_hilo_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_lower.xform_expr globals prelude in
  let stmts = test_xform_stmts Xform_lower.xform_stmts globals prelude in
  [
    ("lower x[63:0] where x is a bitvector", `Quick, expr
       "var x : bits(64);"
       "x[63:0]"
       "x[0 +: 64]");
    ("lower x[hi:lo] where x is a bitvector", `Quick, expr
       "var x : bits(64); var hi : integer; var lo : integer;"
       "x[hi:lo]"
       "x[lo +: hi + -lo + 1]");
    ("lower x[2:0] = y[2:0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[2:0] = y[2:0];"
       "x[0 +: 3] = y[0 +: 3];");
    ("lower x[2:0][1:0] = y[2:0][1:0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[2:0][1:0] = y[2:0][1:0];"
       "var __l0 = x[0 +: 3];
        __l0[0 +: 2] = y[0 +: 3][0 +: 2];
        x[0 +: 3] = __l0;");
    ("lower x[hi:lo] = y[hi:lo] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8); var hi : integer; var lo : integer;"
       "x[hi:lo] = y[hi:lo];"
       "x[lo +: hi + -lo + 1] = y[lo +: hi + -lo + 1];");
    ("lower x[0] = y[0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[0] = y[0];"
       "x[0 +: 1] = y[0 +: 1];");
    ("lower x[2:0][0] = y[2:0][0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[2:0][0] = y[2:0][0];"
       "var __l1 = x[0 +: 3];
        __l1[0 +: 1] = y[0 +: 3][0 +: 1];
        x[0 +: 3] = __l1;");
    ("lower x[7:0][3:0][1:0] = y[7:0][3:0][1:0] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8);"
       "x[7:0][3:0][1:0] = y[7:0][3:0][1:0];"
       "var __l2 = x[0 +: 8];
        var __l3 = __l2[0 +: 4];
        __l3[0 +: 2] = y[0 +: 8][0 +: 4][0 +: 2];
        __l2[0 +: 4] = __l3;
        x[0 +: 8] = __l2;");
    ("lower x[i] = y[i] where x and y are bitvectors", `Quick, stmts
       "var x : bits(8); var y : bits(8); var i : integer;"
       "x[i] = y[i];"
       "x[i +: 1] = y[i +: 1];");
    ("lower x[7:0] where x is integer", `Quick, expr
       "var x : integer;"
       "x[7:0]"
       "x[0 +: 8]");
    ("element slice 1", `Quick, expr
       "var r : bits(16);"
       "r[1 *: 7]"
       "r[7 +: 7]");
    ("element slice 2", `Quick, expr
       "var r : bits(16);"
       "r[0 *: 1 + 3, 5, 1 *: 1 + 2]"
       "__let __l4 : integer = 1 + 3 __in
        __let __l5 : integer = 1 + 2 __in
        r[0 * __l4 +: __l4, 5 +: 1, __l5 +: __l5]");
    ("element slice 3, l-expr 1", `Quick, stmts
       "var r : bits(16); var x : bits(7);"
       "r[1 *: 7] = x;"
       "r[7 +: 7] = x;");
    ("element slice 4, l-expr 2", `Quick, stmts
       "var r : bits(16); var x : bits(32);"
       "r[0 *: 1 + 7] = x[0 *: 4 + 4];"
       "let __l6 : integer = 1 + 7;
        r[0 * __l6 +: __l6] = __let __l7 : integer = 4 + 4 __in
                              x[0 * __l7 +: __l7];");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("expr_bitslices_hilo", bitslices_hilo_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
