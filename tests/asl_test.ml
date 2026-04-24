(****************************************************************
 * Test ISA evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open LibISA.Utils
module TC = Tcheck
module AST = Isa_ast

(* test that checks that an expression lexes, parses and typechecks *)
let check_expr_tcheck (tcenv : TC.Env.t) (test_fmt : bool) (what : string)
    (input : string) _ : unit =
  let loc = Loc.Unknown in
  let e = LoadISA.read_expr tcenv loc input in
  if test_fmt then
    Alcotest.(check string) ("format " ^ what) input (format_expr e)

(* simple test of static semantics: parsing and typechecking of correct expressions
 * and optionally checks that pretty-printing the AST produces exactly the input string
 * optionally extends environment with new declarations first
 *)
let test_static (tcenv : TC.GlobalEnv.t) (test_fmt : bool) (name : string)
    (decls : string) (expr : string) : unit Alcotest.test_case =
  let (tcenv', _) = extend_tcenv tcenv decls in
  (name, `Quick, check_expr_tcheck tcenv' test_fmt name expr)

(* Test that checks that a declaration reports the right error message at the right location
 *
 * Both the error message and the location are optional.
 * If both are omitted, we just check that an error is reported.
 *)
let test_static_error (globals : TC.GlobalEnv.t) (name : string) (declarations : string)
    (oexpect : string option) (oloc : string option) : unit Alcotest.test_case =
  let globals = TC.GlobalEnv.clone globals in
  (name, `Quick, fun _ ->
    let lexbuf = Lexing.from_string declarations in
    let msg = try
        let t = Asl_parser.declarations_start Asl_lexer.token lexbuf in
        let ds = TC.tc_declarations globals ~isPrelude:false ~sort_decls:false t in
        ignore ds;
        Alcotest.fail "error was not detected"
      with
      | Asl_parser.Error -> "ParseError()"
      | AST.Parse_error_locn(loc, msg) -> Printf.sprintf "Parse_error_locn(%s,%s)" (Loc.to_string loc) msg
      | AST.PrecedenceError(loc, op1, op2) ->
        Printf.sprintf "PrecedenceError(%s,%s,%s)" (Loc.to_string loc) (Isa_utils.pp_binop op1) (Isa_utils.pp_binop op2)
      | Asl_lexer.Eof -> "Eof()"
      | Error.UnknownObject(loc, what, x) -> Printf.sprintf "UnknownObject(%s,%s,%s)" (Loc.to_string loc) what x
      | Error.DoesNotMatch(loc, what, x, y) -> Printf.sprintf "DoesNotMatch(%s,%s,%s,%s)" (Loc.to_string loc) what x y
      | Error.IsNotA(loc, what, x) -> Printf.sprintf "IsNotA(%s,%s,%s)" (Loc.to_string loc) what x
      | Error.Ambiguous(loc, what, x) -> Printf.sprintf "Ambiguous(%s,%s,%s)" (Loc.to_string loc) what x
      | Error.TypeError(loc, msg) -> Printf.sprintf "TypeError(%s,%s)" (Loc.to_string loc) msg
      | InternalError(loc, msg, _, _) -> Printf.sprintf "InternalError(%s,%s)" (Loc.to_string loc) msg
      | Value.Return(_) -> Printf.sprintf "Return(_)"
      | Value.EvalError(loc, err) -> Printf.sprintf "EvalError(%s,%s)" (Loc.to_string loc) err
      | Value.Throw(loc, _) -> Printf.sprintf "Throw(%s,_)" (Loc.to_string loc)
    in
    ( match oexpect with
    | Some expect -> Alcotest.(check string) name expect msg;
    | _ -> ()
    );
    ( match oloc with
    | Some loc ->
        let location = Printf.sprintf "'%s' %d %d"
            lexbuf.lex_start_p.pos_fname lexbuf.lex_start_p.pos_lnum (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
        in
        Alcotest.(check string) (name ^ " location") loc location
    | _ -> ()
    )
  )

(* Conditionally execute a test based on condition 'c' *)
let enable_test (c : bool) (test : unit Alcotest.test_case) : (unit Alcotest.test_case) =
  if c then test else ("skipped test", `Quick, (fun _ -> ()))

let eval tcenv env (input : string) : Value.value =
  ( try
      let loc = Loc.Unknown in
      let e = LoadISA.read_expr tcenv loc input in
      Eval.eval_expr loc env e
    with exn ->
      Error.print_exception exn;
      raise exn
  )

let test_bool (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : bool) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ Bool.to_string r in
  Alcotest.check value what (Value.VBool r) (eval tcenv env l)

let test_int (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : int) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ string_of_int r in
  Alcotest.check value what (Value.VInt (Z.of_int r)) (eval tcenv env l)

let test_string (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ r in
  Alcotest.check value what (Value.VString r) (eval tcenv env l)

let test_bits (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, env) = extend_env globals prelude decls in
  let what = l ^ " == " ^ r in
  Alcotest.check value what (Value.from_bitsLit r) (eval tcenv env l)

(* Test that a global transformation f does not change the value of expression x *)
let test_xform (globals : TC.GlobalEnv.t) (prelude : AST.declaration list)
    (f : (AST.declaration list) xform) (decls : string) (x : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let ds1 = prelude @ ds in
  let ds2 = f ds1 in
  let r1 = eval tcenv (Eval.build_evaluation_environment ds1) x in
  let r2 = eval tcenv (Eval.build_evaluation_environment ds2) x in
  Alcotest.check value "transformed code" r1 r2

let tests : unit Alcotest.test_case list =
  (* Control whether constraint checks are performed (and tested) *)
  Tcheck.enable_constraint_checks := false;

  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    test_static globals true "literals (int)" "" "1234";
    test_static globals false "literals (Bits)" "" "0b1111_0000";
    test_static globals false "literals (string)" "" "\"abc\"";
    test_static globals false "literals (string)" "" "\"ab\\nc\"";
    test_static globals false "literals (string)" "" "\"ab\\tc\"";
    test_static globals false "literals (String)" "" "\"ab\\\\c\"";
    test_static globals false "literals (String)" "" "\"ab\\\"c\"";
    test_static globals true "let-expressions" "" "(__let x : Integer := 1 __in x)";
    test_static globals true "expressions (records)"
      "record Pair = {x : Integer, y : Integer};" "Pair{x => 1, y => 2}";
    test_static globals true "expressions (UNSPECIFIED)" "" "UNSPECIFIED : Bits(4)";
    test_static globals false "expressions (bitfields)"
      "bitfield T = { hi => [ 31:16 ], lo => [15:0] } : Bits(32);\n\
      \                      let t : T = 0x12345678[31:0];\n\
      \                     " "t.hi";
    test_static_error globals "unsynthesizable parameters"
     (* parameters cannot be synthesized for this type *)
     "function F1(x : Bits(8*N));
      function T()
      begin
          F1(Std::Bits::Zero(16));
      end
     "
     (Some "TypeError(file  line 4 char 10 - 34,unable to synthesize type parameter N)")
     None;
    test_static globals false "parameter synthesis 1"
      (* parameters can be synthesized from explicit argument values *)
      "function F(N : Integer, x : Bits(8*N));
       function T() -> Integer
       begin
           F(2, Std::Bits::Zero(16));
           return 0;
       end
      " "0";
    test_static globals false "parameter synthesis 2"
      (* order of parameters should not affect synthesis *)
      "function F(x : Bits(8*N), N : Integer);
       function T() -> Integer
       begin
           F(Std::Bits::Zero(16), 2);
           return 0;
       end
      " "0";
    test_static globals false "parameter synthesis 3"
      (* parameters can be synthesized from the type of actual arguments *)
      "function F(x : Bits(8*N), y : Bits(N));
       function T() -> Integer
       begin
           F(Std::Bits::Zero(16), 0b00);
           return 0;
       end
      " "0";
    test_static globals false "Integer subtyping (function return)"
      "function F(x : {10..90}) -> {0..100}
       begin
           return x;
       end
      " "0";
    test_static globals false "var decls"
      "function F(x : Bits(8*N))
       begin
           var a : Bits(8*N) := UNSPECIFIED : Bits(8*N);
           var b : Bits(8*N) := Std::Bits::Zero(8*N);
           var c : Bits(8*N);
           var d := UNSPECIFIED : Bits(8*N);
           var _ := 1;
           var (f, g) := (1, True);
           var (h : Integer, i : Boolean) := (1, True);
           var (j : Integer, - : Boolean) := (1, True);
           var arr1 : array [8] of Integer;
           var arr2 : array [type Boolean] of Integer;

           let m : Bits(8*N) := UNSPECIFIED : Bits(8*N);
           let n : Bits(8*N) := Std::Bits::Zero(8*N);
           let o = UNSPECIFIED : Bits(8*N);
           let - := 1;
           let (p, q) := (1, True);
           let (r : Integer, s : Boolean) := (1, True);
           let (t : Integer, _ : Boolean) := (1, True);
       end" "1";
    test_static globals false "case statements"
      "function F(x : Bits(3), y : Boolean) -> Integer
       begin
           case x of
               when 0b000 => return 0;
               when 0b001 => return 0;
               when 0b01x => return 2;
               when 0b100, 0b111 => return 4;
               when 0b101 if y => return 5;
               otherwise => return 6;
           end
       end" "1";
    ("let-expressions",        `Quick, test_int globals prelude "" "__let x : Integer := 2 __in x+x" 4);
    ("prelude (+ (int))",      `Quick, test_int globals prelude "" "1+1" 2);
    ("prelude (/)",          `Quick, test_int globals prelude "" "5 / 3" 1);
    ("prelude (mul_bits_int)", `Quick, test_bits globals prelude "" "0b00111 * 3" "10101"); (* == 21 *)

    (* This regression test is for a typechecker bug where the typechecker did not handle negations
     * correctly (and subtractions 'x-y' were being transformed into 'x + (-y)' *)
    ("tcheck regression (neg_int)", `Quick, test_bits globals prelude
     "function F(i : {0..}, size : {0..}) -> Bits(size)
      begin
          var result := Std::Bits::All_Ones(size);
          result[size-1 : i] := Std::Bits::Zero(size-i);
          return result;
      end"
    "F(3,8)" "00000111");

    ("function (nullary form)", `Quick, test_bool globals prelude
       "var _A : Boolean;
       function A -> Boolean begin return _A; end
       function A := v : Boolean begin _A := v; end
       function Test(v : Boolean) -> Boolean
       begin
           A := v;
           return A;
       end
       "
       "Test(True)" true);
    ("bittuple LExpr_bitTuple", `Quick, test_bool globals prelude
     "function expand() -> Boolean
      begin
        var x : Bits(4);
        var y : Bits(8);
        var z : Bits(12);
        [x, y, z] := 0b1111_01010101_000000000000;

        return (x == 0b1111 and then y == 0b01010101 and then z == 0b000000000000);
     end"
     "expand()" true);
    ("function (array form)", `Quick, test_bool globals prelude
       "var _A : array [4] of Boolean;
       function A[i : Integer] -> Boolean begin return _A[i]; end
       function A[i : Integer] := v : Boolean begin _A[i] := v; end
       function Test(i : Integer, v : Boolean) -> Boolean
       begin
           A[i] := v;
           return A[i];
       end
       "
       "Test(1,True)" true);
    ("parameterized record",   `Quick, test_bits globals prelude
       "record R(M) = { x : Bits(M) };
       function S(b : Bits(M)) -> R(M) begin return R(M){x=>b}; end
       function T(r : R(M)) -> Bits(M) begin return r.x; end"
       "T(S(0b111))" "111");
    ("parameterized type",     `Quick, test_bits globals prelude
       "type B(M) of Bits(M);
       function T(b : Bits(M)) -> B(M) begin return b; end"
       "T(0b111)" "111");
    ("parameterized record extract", `Quick, test_bits globals prelude
       "record R(M) = { x : Bits(M) };
       function F() -> Bits(8)
       begin
           let a := R(3){x=>0b111};
           return Zero_Extend(a.x, 8);
       end"
       "F()" "00000111");
    ("statements (while)",     `Quick, test_int globals prelude
      "function TestWhile(x : Integer) -> Integer begin var i : Integer := 0; while i < x do i := i + 1; end return i; end"
      "TestWhile(3)" 3);
    ("statements (repeat)",    `Quick, test_int globals prelude
      "function TestRepeat(x : Integer) -> Integer begin var i : Integer := 0; repeat i := i + 1; until i >= x; return i; end"
      "TestRepeat(3)" 3);
    ("statements (array init)", `Quick, test_bits globals prelude
      "function TestArrayInit(x : Integer) -> Bits(3) begin
          var a := array (0b100, 0b101, 0b110, 0b111);
          return a[x];
       end
      "
      "TestArrayInit(2)" "110");
    ("tuple transform",        `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "function F() -> (Integer, Integer) begin return (1,2); end
       function T() -> Integer begin let (x, y) := F(); return x + y; end"
      "T() == 3");
    ("tuple transform (in nullary function)", `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "function F() -> (Integer, Integer) begin return (1,2); end
       function T -> Integer begin let (x, y) := F(); return x + y; end"
      "T == 3");
  ]

let () = ( try
             Alcotest.run "libISA" [ ("asl (static)", tests) ]
           with
           | exn -> Error.print_exception exn; raise exn
         )

(****************************************************************
 * End
 ****************************************************************)
