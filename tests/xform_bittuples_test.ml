(****************************************************************
 * Test bittuple lowering transform
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
 * Test bittuple lowering
 ****************************************************************)

(** Test xform_stmts *)
let test_bittuple_stmts (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  TC.enable_runtime_checks := false;
  let (tcenv, _) = extend_tcenv globals decls in
  let l' = LoadISA.read_stmts tcenv l in
  let r' = LoadISA.read_stmts tcenv r in
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
    ("multiple-slice-lexpr transform", `Quick, test_bittuple_stmts globals prelude
      "var x : Bits(5);"
      "x[0 +: 3, 3 +: 2] := 0b10_001;"
      "let __a0 : Bits(5) := 0b10_001;
       x[0 +: 3] := __a0[2 +: 3];
       x[3 +: 2] := __a0[0 +: 2];
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
