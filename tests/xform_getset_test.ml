(****************************************************************
 * Test getters and setters elimination transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module TC = Tcheck

(****************************************************************
 * Test getters and setters elimination
 ****************************************************************)

let getset_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_getset.xform_decls globals prelude in
  [
    ("getter type", `Quick, decl
      ""
      "getter G => integer;"
      "func G_read() => integer;");

    ("getter", `Quick, decl
      "var x : integer;"
      "getter G => integer begin return x; end"
      "func G_read() => integer begin return x; end");

    ("setter type", `Quick, decl
      ""
      "setter S = val : integer;"
      "func S_write(val : integer);");

    ("setter", `Quick, decl
      "var x : integer;"
      "setter S = val : integer begin x = val; end"
      "func S_write(val : integer) begin x = val; end");

    ("array getter type", `Quick, decl
      ""
      "getter G[i : integer] => integer;"
      "func G_read(i : integer) => integer;");

    ("array getter", `Quick, decl
      "var x : array [1] of integer;"
      "getter G[i : integer] => integer begin return x[i]; end"
      "func G_read(i : integer) => integer begin return x[i]; end");

    ("array setter type", `Quick, decl
      ""
      "setter S[i : integer] = val : integer;"
      "func S_write(i : integer, val : integer);");

    ("array setter", `Quick, decl
      "var x : array [1] of integer;"
      "setter S[i : integer] = val : integer begin x[i] = val; end"
      "func S_write(i : integer, val : integer) begin x[i] = val; end");

    ("__write l-expr", `Quick, decl
      "var x : integer;"
      "setter S = val : integer begin x = val; end
       func P() begin S = 0; end
       func F() => integer begin S = 0; return 0; end"
      "func S_write(val : integer) begin x = val; end
       func P() begin S_write(0); end
       func F() => integer begin S_write(0); return 0; end");

    ("__readwrite l-expr", `Quick, decl
      "var x : bits(3);"
      "getter X => bits(3) begin return x; end
       setter X = val : bits(3) begin x = val; end
       func P() begin X[1 +: 2] = '10'; end
       func F() => bits(3) begin X[1 +: 2] = '10'; return X; end"
      "func X_read() => bits(3) begin return x; end
       func X_write(val : bits(3)) begin x = val; end
       func P() begin var __rmw0 = X_read(); __rmw0[1 +: 2] = '10'; X_write(__rmw0); end
       func F() => bits(3) begin var __rmw1 = X_read(); __rmw1[1 +: 2] = '10'; X_write(__rmw1); return X_read(); end");
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
