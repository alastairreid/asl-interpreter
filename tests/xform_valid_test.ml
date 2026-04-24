(****************************************************************
 * Test transform that helps tracking valid Bits
 *
 * Copyright (C) 2024-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
module TC = Tcheck

(****************************************************************
 * Test transform
 ****************************************************************)

let valid_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let vars = [ Ident.mk_ident "x" ] in
  let stmt = test_xform_stmts (Xform_valid.xform_stmts vars) globals prelude in
  [
    ("field", `Quick, stmt
      "bitfield T = { F0 => [0], F1 => [1 +: 2] } : Bits(4);
       var x : T;
       function builtin_fuzz(var_name : String, low : Integer, width : Integer) -> ();"
      "x.F1 := UNSPECIFIED : Bits(2);"
      "x.F1 := UNSPECIFIED : Bits(2);
       builtin_fuzz(\"x\", 1, 2);");

    ("var", `Quick, stmt
      "let n := 2;
       var x : Bits(n);
       function builtin_fuzz(var_name : String, low : Integer, width : Integer) -> ();"
      "x := UNSPECIFIED : Bits(n);"
      "x := UNSPECIFIED : Bits(n);
       builtin_fuzz(\"x\", 0, n);");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("valid", valid_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
