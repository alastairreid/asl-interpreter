(****************************************************************
 * Test integer bitslicing transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test integer bitslicing
 ****************************************************************)

let int_bitslice_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_int_bitslices.xform_expr globals prelude in
  [
    ("integer bitslice", `Quick, expr
       "var x : integer; var i : integer;"
       "x[i +: 8]"
       "asl_cvt_int_bits(x, i + 8)[i +: 8]");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("int_bitslice", int_bitslice_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
