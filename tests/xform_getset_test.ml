(****************************************************************
 * Test assignment function elimination transform
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open Isa_utils
module TC = Tcheck

(****************************************************************
 * Test elimination of assignment functions
 ****************************************************************)

let getset_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_getset.xform_decls globals prelude in
  [
    ("nullary type", `Quick, decl
      ""
      "function G => Integer;"
      "function G() => Integer;");

    ("function", `Quick, decl
      "var x : Integer;"
      "function G => Integer begin return x; end"
      "function G() => Integer begin return x; end");

    ("assignment function type", `Quick, decl
      ""
      "function S := val : Integer;"
      "function S_write(val : Integer);");

    ("assignment function", `Quick, decl
      "var x : Integer;"
      "function S := val : Integer begin x := val; end"
      "function S_write(val : Integer) begin x := val; end");

    ("array function type", `Quick, decl
      ""
      "function G[i : Integer] => Integer;"
      "function G(i : Integer) => Integer;");

    ("array function", `Quick, decl
      "var x : array [1] of Integer;"
      "function G[i : Integer] => Integer begin return x[i]; end"
      "function G(i : Integer) => Integer begin return x[i]; end");

    ("array assignment function type", `Quick, decl
      ""
      "function S[i : Integer] := val : Integer;"
      "function S_write(i : Integer, val : Integer);");

    ("array assignment function", `Quick, decl
      "var x : array [1] of Integer;"
      "function S[i : Integer] := val : Integer begin x[i] := val; end"
      "function S_write(i : Integer, val : Integer) begin x[i] := val; end");

    ("__write l-expr", `Quick, decl
      "var x : Integer;"
      "function S := val : Integer begin x := val; end
       function P() begin S = 0; end
       function F() => Integer begin S = 0; return 0; end"
      "function S_write(val : Integer) begin x := val; end
       function P() begin S_write(0); end
       function F() => Integer begin S_write(0); return 0; end");

    ("__readwrite l-expr", `Quick, decl
      "var x : Bits(3);"
      "function X => Bits(3) begin return x; end
       function X := val : Bits(3) begin x := val; end
       function P() begin X[1 +: 2] = 0b10; end
       function F() => Bits(3) begin X[1 +: 2] = 0b10; return X; end"
      "function X() => Bits(3) begin return x; end
       function X_write(val : Bits(3)) begin x := val; end
       function P() begin var __rmw0 = X(); __rmw0[1 +: 2] = 0b10; X_write(__rmw0); end
       function F() => Bits(3) begin var __rmw1 = X(); __rmw1[1 +: 2] = 0b10; X_write(__rmw1); return X(); end");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("getset", getset_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
