(****************************************************************
 * Test tuple lowering transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module AST = Asl_ast
module TC = Tcheck

(****************************************************************
 * Test tuple lowering
 ****************************************************************)

let tuple_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let stmts = test_xform_stmts Xform_tuples.xform_stmts globals prelude in
  [
    ("let", `Quick, stmts
       ""
       "let (x, y) = (1, 2);"
       "let x = 1; let y = 2;");

    ("var", `Quick, stmts
       ""
       "var (x, y) = (1, 2);"
       "var x = 1; var y = 2;");

    ("assign", `Quick, stmts
       "var x : integer; var y : integer;"
       "(x, y) = (1, 2);"
       "x = 1; y = 2;");

    ("let-if", `Quick, stmts
       "var p : boolean;"
       "let (x : integer, y : integer) = if p then (1, 2) else (2, 1);"
       "var x : integer; var y : integer;
        if p then x = 1; y = 2; else x = 2; y = 1; end
       ");

    ("var-if", `Quick, stmts
       "var p : boolean;"
       "var (x : integer, y : integer) = if p then (1, 2) else (2, 1);"
       "var x : integer; var y : integer;
        if p then x = 1; y = 2; else x = 2; y = 1; end
       ");

    ("assign-if", `Quick, stmts
       "var p : boolean; var x : integer; var y : integer;"
       "(x, y) = if p then (1, 2) else (2, 1);"
       "if p then x = 1; y = 2; else x = 2; y = 1; end
       ");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("tuple", tuple_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
