(****************************************************************
 * Test transform that helps tracking valid bits
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
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
      "var x : bits(3) { [0] F0 [1 +: 2] F1 };
       func ASL_fuzz(var_name : string, low : integer, width : integer) => ();"
      "x.F1 = UNKNOWN : bits(2);"
      "x.F1 = UNKNOWN : bits(2);
       ASL_fuzz(\"x\", 1, 2);");

    ("var", `Quick, stmt
      "let N = 2;
       var x : bits(N);
       func ASL_fuzz(var_name : string, low : integer, width : integer) => ();"
      "x = UNKNOWN : bits(N);"
      "x = UNKNOWN : bits(N);
       ASL_fuzz(\"x\", 0, N);");
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
