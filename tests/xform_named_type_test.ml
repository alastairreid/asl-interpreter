(****************************************************************
 * Test named type transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
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
      "type X of integer; var x : X;"
      "type X of integer; var x : integer;");

    ("before type declaration", `Quick, decl
      ""
      "var x : X; type X of integer;"
      "var x : integer; type X of integer;");

    ("recursive", `Quick, decl
      ""
      "type Y of integer; type X of Y; var x : X;"
      "type Y of integer; type X of integer; var x : integer;");

    ("with type parameter", `Quick, decl
      ""
      "type X(n) of bits(n); var x : X(1);"
      "type X(n) of bits(n); var x : bits(1);");
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
