(****************************************************************
 * Test bittuple lowering transform
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
 * Test bittuple lowering
 ****************************************************************)

(** Test xform_stmts *)
let test_bittuple_stmts (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, _) = extend_tcenv globals decls in
  let l' = LoadASL.read_stmts tcenv l in
  let r' = LoadASL.read_stmts tcenv r in
  (*
  let genv = Eval.build_constant_environment ds in
  let l'' = CP.xform_stmts genv l' in
  *)
  let l'' = Xform_bittuples.xform_stmts l' in
  let what = l ^ "\n==>\n" ^ r in
  Alcotest.check stmts what r' l''

let tuple_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("bittuple-lexpr transform 1", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(3); var y : bits(2);"
       "[x, y] = '100 01';"
       "let __a0 : bits(5) = '100 01';
        x = __a0[2 +: 3];
        y = __a0[0 +: 2];
       ");

    ("bittuple-lexpr transform 2", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(3); var y : bits(2);"
       "let m = '100 01';
        [x, y] = m;
       "
       "let m = '100 01';
        let __a1 : bits(5) = m;
        x = __a1[2 +: 3];
        y = __a1[0 +: 2];
       ");

    ("bittuple-lexpr transform 3", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(3); var y : bits(2);
       func t_f() => bits(5)
       begin
           return '100 01';
       end
      "
      "[x, y] = t_f();"
      "let __a2 : bits(5) = t_f();
       x = __a2[2 +: 3];
       y = __a2[0 +: 2];
      ");

    ("bittuple-lexpr transform 4", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(3); var y : bits(2); var c : boolean;"
      "[x, y] = if c then '100 01' else '111 11';"
      "let __a3 : bits(5) = if c then '100 01' else '111 11';
       x = __a3[2 +: 3];
       y = __a3[0 +: 2];
      ");

    ("multiple-slice-lexpr transform", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(5);"
      "x[0 +: 3, 3 +: 2] = '10 001';"
      "let __a4 : bits(5) = '10 001';
       x[0 +: 3] = __a4[2 +: 3];
       x[3 +: 2] = __a4[0 +: 2];
      ");

    ("multiple-slice-rexpr transform", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(5); var y : bits(5);"
      "y = x[0 +: 3, 3 +: 2];"
      "y = [x[0 +: 3], x[3 +: 2]];
      ");

    ("bittuple declaration transform", `Quick, test_bittuple_stmts globals prelude
      ""
      "let [x : bits(3), y : bits(2)] = '100 01';"
      "let __a5 : bits(5) = '100 01';
       let x = __a5[2 +: 3];
       let y = __a5[0 +: 2];
      ");

    ("bittuple-expr transform 1", `Quick, test_bittuple_stmts globals prelude
      "var x : bits(3); var y : bits(2);"
      "y = x[2,0];"
      "y = [x[2], x[0]];
      ");

    ("bittuple-expr transform 2", `Quick, test_bittuple_stmts globals prelude
      "var x : integer; var y : bits(2);"
      "y = (x DIV 3)[2,0];"
      "y = __let __a6 : integer = (x DIV 3) __in [__a6[2], __a6[0]];
      ")
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
