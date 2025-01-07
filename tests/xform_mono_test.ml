(****************************************************************
 * Test mono transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open Asl_utils
module AST = Asl_ast
module TC = Tcheck

let declarations =
   "var TMP : integer;

   func Constrained(a : integer) => integer { 32, 64 }
   begin
     if a < 32 then return 32;
     else return 64;
     end
   end

   func Create{N}(N : integer) => bits(N)
   begin
     var a : bits(N);
     // TMP is used to disable constant evaluation of this function
     TMP = TMP + 1;
     return a;
   end

   func Foo{N, M}(M : integer, a : bits(N))
   begin
     // TMP is used to disable constant evaluation of this function
     TMP = TMP + N + M;
   end

   func FooReturn{N, M}(M : integer, a : bits(N)) => integer
   begin
     // TMP is used to disable constant evaluation of this function
     TMP = TMP + N + M;
     return TMP;
   end
   "
let declarations_monomorphized =
   "var TMP : integer;

   func Constrained(a : integer) => integer { 32, 64 }
   begin
     if a < 32 then return 32;
     else return 64;
     end
   end

    func Create_N_32() => bits(32) begin var a : bits(32); TMP = TMP + 1; return a; end
    func Create_N_64() => bits(64) begin var a : bits(64); TMP = TMP + 1; return a; end

    func Foo_N_32_M_32(a : bits(32)) begin TMP = TMP + 32 + 32; end
    func Foo_N_32_M_64(a : bits(32)) begin TMP = TMP + 32 + 64; end
    func Foo_N_64_M_32(a : bits(64)) begin TMP = TMP + 64 + 32; end
    func Foo_N_64_M_64(a : bits(64)) begin TMP = TMP + 64 + 64; end

    func FooReturn_N_32_M_32(a : bits(32)) => integer begin TMP = TMP + 32 + 32; return TMP; end
    func FooReturn_N_32_M_64(a : bits(32)) => integer begin TMP = TMP + 32 + 64; return TMP; end
    func FooReturn_N_64_M_32(a : bits(64)) => integer begin TMP = TMP + 64 + 32; return TMP; end
    func FooReturn_N_64_M_64(a : bits(64)) => integer begin TMP = TMP + 64 + 64; return TMP; end
   "

(** Test xform_stmts *)
let test_case_decl (globals : TC.GlobalEnv.t)
    (prelude : AST.declaration list)
    (decls : string)
    (decls_mono : string)
    (decl_name : string)
    (l : string)
    (r : string)
    () : unit =
  Xform_mono.enable_auto_case_split := true;
  Test_utils.test_xform_decl Xform_mono.monomorphize
      globals
      prelude
      decls
      decls_mono
      decl_name
      l
      r
      ()

let expand_poly_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    ("stmt_tcall", `Quick, test_case_decl globals prelude
       declarations
       declarations_monomorphized
       "X"
       "func X(c : integer{32, 64}, d : integer{32, 64})
        begin
          Foo(c, Create(d));
        end"
       "func X(c : integer{32, 64}, d : integer{32, 64})
        begin
          case (c, d) of
              when (32, 32) => Foo_N_32_M_32(Create_N_32());
              when (32, 64) => Foo_N_64_M_32(Create_N_64());
              when (64, 32) => Foo_N_32_M_64(Create_N_32());
              when (64, 64) => Foo_N_64_M_64(Create_N_64());
          end
        end"
    );

    ("stmt_assign", `Quick, test_case_decl globals prelude
       declarations
       declarations_monomorphized
       "X"
       "func X()
        begin
          var z : integer;
          let c = Constrained(TMP);
          let d = Constrained(TMP);
          z = FooReturn(c, Create(d));
        end"
       "func X()
        begin
          var z : integer;
          let c = Constrained(TMP);
          let d = Constrained(TMP);
          case (c, d) of
              when (32, 32) => z = FooReturn_N_32_M_32(Create_N_32());
              when (32, 64) => z = FooReturn_N_64_M_32(Create_N_64());
              when (64, 32) => z = FooReturn_N_32_M_64(Create_N_32());
              when (64, 64) => z = FooReturn_N_64_M_64(Create_N_64());
          end
        end"
    );

    ("stmt_vardecl", `Quick, test_case_decl globals prelude
       declarations
       declarations_monomorphized
       "X"
       "func X(c : integer{32}, d : integer{64})
        begin
          var z : integer;
          z = FooReturn(c, Create(d));
        end"
       "func X(c : integer{32}, d : integer{64})
        begin
          var z : integer;
          case (c, d) of
              when (32, 64) => z = FooReturn_N_64_M_32(Create_N_64());
          end
        end"
    );

    ("stmt_constdecl", `Quick, test_case_decl globals prelude
       declarations
       declarations_monomorphized
       "X"
       "func X(c : integer{32}, d : integer{64})
        begin
          let z = FooReturn(c, Create(d));
        end"
       "func X(c : integer{32}, d : integer{64})
        begin
          var z : integer;
          case (c, d) of
              when (32, 64) => z = FooReturn_N_64_M_32(Create_N_64());
          end
        end"
    );
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "mono" [
    ("expand_poly", expand_poly_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)


