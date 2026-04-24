(****************************************************************
 * Test tuple lowering transform
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open Isa_utils
module AST = Isa_ast
module TC = Tcheck

(****************************************************************
 * Test tuple lowering
 ****************************************************************)

let tuple_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let stmts = test_xform_stmts Xform_tuples.xform_stmts globals prelude in
  [
    ("let", `Quick, stmts
       ""
       "let (x, y) := (1, 2);"
       "let x := 1; let y := 2;");

    ("var", `Quick, stmts
       ""
       "var (x, y) := (1, 2);"
       "var x := 1; var y := 2;");

    ("assign", `Quick, stmts
       "var x : Integer; var y : Integer;"
       "(x, y) := (1, 2);"
       "x := 1; y := 2;");

    ("let-if", `Quick, stmts
       "var p : Boolean;"
       "let (x : Integer, y : Integer) := if p then (1, 2) else (2, 1);"
       "var x : Integer; var y : Integer;
        if p then x := 1; y := 2; else x := 2; y := 1; endif;
       ");

    ("var-if", `Quick, stmts
       "var p : Boolean;"
       "var (x : Integer, y : Integer) := if p then (1, 2) else (2, 1);"
       "var x : Integer; var y : Integer;
        if p then x := 1; y := 2; else x := 2; y := 1; endif;
       ");

    ("assign-if", `Quick, stmts
       "var p : Boolean; var x : Integer; var y : Integer;"
       "(x, y) := if p then (1, 2) else (2, 1);"
       "if p then x := 1; y := 2; else x := 2; y := 1; endif;
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
