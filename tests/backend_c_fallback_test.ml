(****************************************************************
 * Test new C backend
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
open Test_utils_backend
module Test_cases = Test_cases_backend
module BE = Backend_c_new
module TC = Tcheck

let check_syntax (name : string) (code : string) : unit =
  let rt_header = BE.get_rt_header () in
  let prog = "gcc" in
  let args =
    [
      "-std=c99";
      "-fsyntax-only";
      "-I../runtime/include";
      "-Werror";
      "-xc";
    ]
  in
  let header = String.concat "\n" rt_header in
  check_compiler "C" ".c" prog args name header code

let test_declaration (name : string) (s : string) : unit =
  let fmt = Format.str_formatter in
  let tcenv = TC.env0 in
  let () = BE.catch_labels#reset in
  check_declaration tcenv (BE.declarations fmt) check_syntax name s

let make_cases (cases : Test_cases.test_case list) :
    unit Alcotest.test_case list =
  List.filter_map
    (fun (name, bs, s) ->
      if List.mem Test_cases.Backend_C bs then
        Some (name, `Quick, fun _ -> test_declaration name s)
      else None)
    cases

let () =
  BE.set_runtime "fallback";
  ignore (Test_utils.load_test_libraries ());
  Alcotest.run "backend_c_fallback"
    [
      ("expression",        make_cases Test_cases.expr);
      ("integer ops",       make_cases Test_cases.int_ops);
      ("enum ops",          make_cases Test_cases.enum_ops);
      ("bitvector ops",     make_cases Test_cases.bit_ops);
      ("ram ops",           make_cases Test_cases.ram_ops);
      ("misc ops",          make_cases Test_cases.misc_ops);
      ("function_decl",     make_cases Test_cases.fun_decl);
      ("procedure_decl",    make_cases Test_cases.proc_decl);
      ("statement",         make_cases Test_cases.stmt);
      ("type_decl",         make_cases Test_cases.type_decl);
      ("variable_decl",     make_cases Test_cases.var_decl);
    ]

(****************************************************************
 * End
 ****************************************************************)
