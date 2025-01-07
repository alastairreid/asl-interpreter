(****************************************************************
 * Test expression simplification
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
 * Test expression simplification
 ****************************************************************)

let test_simplify_expr (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let l' = LoadASL.read_expr tcenv Loc.Unknown l in
  let r' = LoadASL.read_expr tcenv Loc.Unknown r in
  let what = l ^ " == " ^ r in
  Alcotest.check expr what r' (Xform_simplify_expr.simplify l')

let test_simplify_expr_id (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (x : string) () : unit =
  test_simplify_expr globals prelude decls x x ()

let simplify_expr_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl_x = "var x : integer;" in
  let decl_xy = "var x : integer; var y : integer;" in
  [
    (* test some identities *)
    ("id_const", `Quick, test_simplify_expr_id globals prelude "" "3");
    ("id_var", `Quick, test_simplify_expr_id globals prelude "var x : integer;" "x");
    ("id_add_var_const", `Quick, test_simplify_expr_id globals prelude "var x : integer;" "3 + x");

    (* test that non-integer expressions are not changed *)
    ("non_int", `Quick, test_simplify_expr_id globals prelude "" "3[0]");
    ("non_triv", `Quick, test_simplify_expr_id globals prelude "" "1 + 2^3 + 4");

    (* test that simplifications are being performed *)
    ("add_const", `Quick, test_simplify_expr globals prelude "" "1 + 1" "2");
    ("add_const_var", `Quick, test_simplify_expr globals prelude "var x : integer;" "x + 3" "x + 3");
    ("add_var_var", `Quick, test_simplify_expr globals prelude decl_x "x + x" "2 * x");
    ("add_mul_add1", `Quick, test_simplify_expr globals prelude decl_x "2 * (x + 3)" "2*x + 6");
    ("add_mul_add2", `Quick, test_simplify_expr globals prelude decl_xy "2 * (x + y)" "2*x + 2*y");
    ("add_add_sub", `Quick, test_simplify_expr globals prelude decl_xy "x + y - x" "y");

    (* some 'real world' example expressions *)
    (* the following occurs in expressions like '[a[63:x], b[x-1:0]]' *)
    ("concat_slices", `Quick, test_simplify_expr globals prelude decl_x "0 + ((63-x)+1) + ((x-1-0)+1)" "64");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "asl_utils" [
    ("simplify_expr", simplify_expr_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
