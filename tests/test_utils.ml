(****************************************************************
 * Utilities for use in tests
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module TC = Tcheck
module AST = Asl_ast

(****************************************************************
 * Support code for setting up tests
 ****************************************************************)

let load_test_libraries () : AST.declaration list =
  let paths = [ ".." ] in
  try (
    LoadASL.read_file paths "prelude.asl" true false
  ) with e -> begin
    Error.print_exception e;
    exit 1
  end

(****************************************************************
 * Alcotest testable functions
 ****************************************************************)

let format_value f v = Format.fprintf f "%s" (Value.string_of_value v)

let value = Alcotest.testable format_value ( = )

let expr = Alcotest.testable Asl_fmt.expr (=)

let format_expr (e : AST.expr) : string =
  Utils.to_string2 (fun fmt -> Asl_fmt.expr fmt e)

let value = Alcotest.testable format_value ( = )

let stmts =
  (* Format to strings and then compare to avoid comparing location information *)
  let eq_stmts (s1 : AST.stmt list) (s2 : AST.stmt list) : bool =
    let f1 = Utils.to_string2 (fun fmt -> Asl_fmt.indented_block fmt s1) in
    let f2 = Utils.to_string2 (fun fmt -> Asl_fmt.indented_block fmt s2) in
    f1 = f2
  in
  Alcotest.testable Asl_fmt.indented_block eq_stmts

let declarations =
  (* Format to strings and then compare to avoid comparing location information *)
  let eq_decls (s1 : AST.declaration list) (s2 : AST.declaration list) : bool =
    let f1 = Utils.to_string2 (fun fmt -> Asl_fmt.declarations fmt s1) in
    let f2 = Utils.to_string2 (fun fmt -> Asl_fmt.declarations fmt s2) in
    f1 = f2
  in
  Alcotest.testable Asl_fmt.declarations eq_decls

(****************************************************************
 * Support code for parsing, typechecking and building environments
 ****************************************************************)

let extend_tcenv (globals : TC.GlobalEnv.t) (declarations : string) :
    TC.Env.t * AST.declaration list =
  let globals = TC.GlobalEnv.clone globals in
  let tcenv = TC.Env.mkEnv globals in
  let ds = LoadASL.read_declarations_unsorted globals declarations in
  (tcenv, ds)

let extend_global_tcenv (globals : TC.GlobalEnv.t) (declarations : string) :
    TC.GlobalEnv.t * AST.declaration list =
  let globals = TC.GlobalEnv.clone globals in
  let ds = LoadASL.read_declarations_unsorted globals declarations in
  (globals, ds)

let extend_env (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
  : (TC.Env.t * Eval.Env.t) =
  let (tcenv, ds) = extend_tcenv globals decls in
  let env = Eval.build_evaluation_environment (prelude @ ds) in
  (tcenv, env)

(****************************************************************
 * Support code for checking transformations
 ****************************************************************)

type 'a xform = 'a -> 'a

(** Test expression transforms *)
let test_xform_expr
    (f : AST.expr xform)
    (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string) (l : string) (r : string) () : unit =
  try
    let (tcenv, _) = extend_tcenv globals decls in
    let l' = LoadASL.read_expr tcenv Loc.Unknown l in
    let r' = LoadASL.read_expr tcenv Loc.Unknown r in
    let x = f l' in
    let what = l ^ "\n==>\n" ^ r in
    Alcotest.check expr what r' x
  with e ->
    Error.print_exception e;
    Alcotest.fail "exception during test"

(** Test statement transforms *)
let test_xform_stmts
    (f : (AST.stmt list) xform)
    (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string) (l : string) (r : string) () : unit =
  try
    let (tcenv, _) = extend_tcenv globals decls in
    let l' = LoadASL.read_stmts tcenv l in
    let r' = LoadASL.read_stmts tcenv r in
    let x = f l' in
    let what = l ^ "\n==>\n" ^ r in
    Alcotest.check stmts what r' x
  with e ->
    Error.print_exception e;
    Alcotest.fail "exception during test"

let test_xform_decls (f : AST.declaration list xform) (globals : TC.GlobalEnv.t)
    (prelude : AST.declaration list) (decls : string) (l : string) (r : string)
    () : unit =
  try
    let env, _ = extend_global_tcenv globals decls in
    let l_env, l' = extend_global_tcenv env l in
    let r_env, r' = extend_global_tcenv env r in
    let what = l ^ "\n==>\n" ^ r in
    Alcotest.check declarations what r' (f l')
  with e ->
    Error.print_exception e;
    Alcotest.fail "exception during test"

let test_xform_decl
    (f : (AST.declaration list) xform)
    (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (decls : string)
    (decls_mono : string)
    (decl_name : string)
    (l : string) (r : string)
    () : unit =
  try
    let find_decl (name : string) (d : AST.declaration) : bool =
      match d with
      | Decl_FunDefn (f, fty, b, loc) -> Ident.matches f ~name
      | _ -> false
    in

    (* Handle decls *)
    let decls_env = TC.GlobalEnv.clone globals in
    let decls = LoadASL.read_declarations_unsorted decls_env decls in

    (* Handle mono decls *)
    let decls_mono_env = TC.GlobalEnv.clone globals in
    let _ = LoadASL.read_declarations_unsorted decls_mono_env decls_mono in

    (* Handle left side, actual result *)
    let l_env = TC.GlobalEnv.clone decls_env in
    let ls = LoadASL.read_declarations_unsorted l_env l in
    let genv = Eval.build_constant_environment (decls @ ls) in
    let ls = Xform_constprop.xform_decls genv (decls @ ls) in (* Adding decls *)
    let ls = f ls in
    (* Retrieve resulting fun/proc def of decl_name *)
    let l' = List.find (find_decl decl_name) ls in

    (* Handle right side, expected result *)
    let r_env = TC.GlobalEnv.clone decls_mono_env in
    let rs = LoadASL.read_declarations_unsorted r_env r in
    (* Retrieve expected fun/proc def of decl_name *)
    let r' = List.find (find_decl decl_name) rs in

    (* Compare results *)
    let what = l ^ "\n==>\n" ^ r in
    Alcotest.check declarations what [r'] [l']
  with e ->
    Error.print_exception e;
    Alcotest.fail "exception during test"

(****************************************************************
 * End
 ****************************************************************)
