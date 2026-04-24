(****************************************************************
 * Test named type transform
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
module TC = Tcheck

(****************************************************************
 * Test named type
 ****************************************************************)

let named_type_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_named_type.xform_decls globals prelude in
  [
    ("after type declaration", `Quick, decl
      ""
      "type X of Integer; var x : X;"
      "type X of Integer; var x : Integer;");

    ("before type declaration", `Quick, decl
      ""
      "var x : X; type X of Integer;"
      "var x : Integer; type X of Integer;");

    ("recursive", `Quick, decl
      ""
      "type Y of Integer; type X of Y; var x : X;"
      "type Y of Integer; type X of Integer; var x : Integer;");

    ("with type parameter", `Quick, decl
      ""
      "type X(n) of Bits(n); var x : X(1);"
      "type X(n) of Bits(n); var x : Bits(1);");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("named_type", named_type_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
