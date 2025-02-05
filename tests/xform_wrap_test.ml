(****************************************************************
 * Test global variable wrapping transform
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test global variable wrapping transform
 ****************************************************************)

let wrap_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_wrap.xform_decls globals prelude in
  [
    ("var", `Quick, decl
      ""
      "var x : integer;
       func f() => integer begin var y : integer; x = 1; y = 1; return x + y; end"
      "var x : integer;
       func x_read() => integer begin return x; end
       func x_write(v : integer) begin x = v; end
       func f() => integer begin var y : integer; x_write(1); y = 1; return x_read() + y; end");

    ("let", `Quick, decl
      ""
      "let x = 1;
       func f() => integer begin let y = 1; return x + y; end"
      "let x = 1;
       func f() => integer begin let y = 1; return x + y; end");

    ("var (array)", `Quick, decl
      ""
      "var x : array [1] of integer;
       func f() => integer begin var y : array [1] of integer; x[0] = 1; y[0] = 1; return x[0] + y[0]; end"
      "var x : array [1] of integer;
       func x_read(i : integer) => integer begin return x[i]; end
       func x_write(i : integer, v : integer) begin x[i] = v; end
       func f() => integer begin var y : array [1] of integer; x_write(0, 1); y[0] = 1; return x_read(0) + y[0]; end");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("wrap", wrap_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
