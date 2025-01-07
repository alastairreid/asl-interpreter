(****************************************************************
 * Test case lowering transform
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
 * Test case lowering
 ****************************************************************)

(** Test xform_expr *)
let test_case_expr (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  Test_utils.test_xform_expr Xform_case.xform_expr globals prelude decls l r ()

(** Test xform_stmts *)
let test_case_stmts (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  Test_utils.test_xform_stmts Xform_case.xform_stmts globals prelude decls l r ()

let case_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("mask", `Quick, test_case_stmts globals prelude
       "var x : bits(4);
        var z : integer;"
       "case x of
            when '11xx' => z = 1;
            otherwise => z = 2;
        end"
       "if x IN '11xx' then
            z = 1;
        else
            z = 2;
        end");

    ("pattern set", `Quick, test_case_stmts globals prelude
       "var x : integer;
        var z : integer;"
       "case x of
            when 1, 2 => z = 1;
            otherwise => z = 2;
        end"
       "if x == 1 || x == 2 then
            z = 1;
        else
            z = 2;
        end");

    ("tuple", `Quick, test_case_stmts globals prelude
       "var x : integer;
        var y : integer;
        var z : integer;"
       "case (x, y) of
            when (1, 2) => z = 1;
            otherwise => z = 2;
        end"
       "if x == 1 && y == 2 then
            z = 1;
        else
            z = 2;
        end");

    ("guard", `Quick, test_case_stmts globals prelude
       "var x : integer;
        var y : integer;
        var z : integer;"
       "case x of
            when 1 where y < 3 => z = 1;
            otherwise => z = 2;
        end"
       "if x == 1 && y < 3 then
            z = 1;
        else
            z = 2;
        end");

  ]

let in_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("tuple", `Quick, test_case_expr globals prelude
       "var x : integer;
        var y : integer;"
       "(x, y) IN (1, 2)"
       "x == 1 && y == 2");

  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("case", case_tests);
    ("IN", in_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
