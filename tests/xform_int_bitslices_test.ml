(****************************************************************
 * Test Integer Bitslicing transform
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open Isa_utils
module TC = Tcheck

(****************************************************************
 * Test Integer Bitslicing
 ****************************************************************)

let int_bitslice_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let expr = test_xform_expr Xform_int_bitslices.xform_expr globals prelude in
  [
    ("Integer Bitslice", `Quick, expr
       "var x : Integer; var i : Integer;"
       "x[i +: 8]"
       "Std::Bits::From_Integer(x, i + 8)[i +: 8]");
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
