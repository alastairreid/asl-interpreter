(****************************************************************
 * Test ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2020
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibASL
open LibASL.Utils
module TC = Tcheck
module AST = Asl_ast

(* test that checks that an expression lexes, parses and typechecks *)
let check_expr_tcheck (tcenv : TC.Env.t) (test_fmt : bool) (what : string)
    (input : string) _ : unit =
  let loc = Loc.Unknown in
  let e = LoadASL.read_expr tcenv loc input in
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
        let t = Asl_parser.declarations_start Lexer.token lexbuf in
        let ds = TC.tc_declarations globals ~isPrelude:false ~sort_decls:false t in
        ignore ds;
        Alcotest.fail "error was not detected"
      with
      | Asl_parser.Error -> "ParseError()"
      | AST.Parse_error_locn(loc, msg) -> Printf.sprintf "Parse_error_locn(%s,%s)" (Loc.to_string loc) msg
      | AST.PrecedenceError(loc, op1, op2) ->
        Printf.sprintf "PrecedenceError(%s,%s,%s)" (Loc.to_string loc) (Asl_utils.pp_binop op1) (Asl_utils.pp_binop op2)
      | Lexer.Eof -> "Eof()"
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
  let loc = Loc.Unknown in
  let e = LoadASL.read_expr tcenv loc input in
  Eval.eval_expr loc env e

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
  Tcheck.enable_constraint_checks := true;

  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  [
    test_static globals true "literals (int)" "" "1234";
    test_static globals false "literals (real)" "" "10.0";
    test_static globals false "literals (bits)" "" "'1111 0000'";
    test_static globals false "literals (string)" "" "\"abc\"";
    test_static globals false "literals (string)" "" "\"ab\\nc\"";
    test_static globals false "literals (string)" "" "\"ab\\tc\"";
    test_static globals false "literals (string)" "" "\"ab\\\\c\"";
    test_static globals false "literals (string)" "" "\"ab\\\"c\"";
    test_static globals true "let-expressions" "" "(__let x : integer = 1 __in x)";
    test_static globals true "expressions (records)"
      "record Pair{x : integer; y : integer; };" "Pair{x = 1, y = 2}";
    test_static globals true "expressions (UNKNOWN)" "" "UNKNOWN : bits(4)";
    test_static globals false "expressions (bitfields)"
      "type T of bits(32) { [ 31:16 ] hi, [15:0] lo };\n\
      \                      let t : T = 0x12345678[31:0];\n\
      \                     " "t.hi";
    test_static_error globals "unsynthesizable parameters"
     (* parameters cannot be synthesized for this type *)
     "func F1(x : bits(8*N));
      func T()
      begin
          F1(Zeros(16));
      end
     "
     (Some "TypeError(file \"\" line 4 char 10 - 24,unable to synthesize type parameter N)")
     None;
    test_static globals false "parameter synthesis 1"
      (* parameters can be synthesized from explicit argument values *)
      "func F(N : integer, x : bits(8*N));
       func T() => integer
       begin
           F(2, Zeros(16));
           return 0;
       end
      " "0";
    test_static globals false "parameter synthesis 2"
      (* order of parameters should not affect synthesis *)
      "func F(x : bits(8*N), N : integer);
       func T() => integer
       begin
           F(Zeros(16), 2);
           return 0;
       end
      " "0";
    test_static globals false "parameter synthesis 3"
      (* parameters can be synthesized from the type of actual arguments *)
      "func F(x : bits(8*N), y : bits(N));
       func T() => integer
       begin
           F(Zeros(16), '00');
           return 0;
       end
      " "0";
    test_static globals false "integer subtyping (function return)"
      "func F(x : integer {10..90}) => integer {0..100}
       begin
           return x;
       end
      " "0";
    enable_test !Tcheck.enable_constraint_checks
      (test_static_error globals "integer subtyping (function return)"
      "func F(x : integer {0..100}) => integer {10..90}
       begin
           return x;
       end
      "
      (Some "TypeError(file \"\" line 3 char 11 - 20,`{0..100}` is not a subrange of `{10..90}`)")
      None);
    test_static globals false "integer subtyping (function call)"
      "func F(x : integer {0..100});
       func G(x : integer {10..90})
       begin
           F(x);
       end
      " "0";
    enable_test !Tcheck.enable_constraint_checks
      (test_static_error globals "integer subtyping (function call)"
      "func F(x : integer {10..90});
       func G(x : integer {0..100})
       begin
           F(x);
       end
      "
      (Some "TypeError(file \"\" line 4 char 11 - 16,`{0..100}` is not a subrange of `{10..90}`)")
      None);
    test_static globals false "integer subtyping (assignment)"
      "func F(x : integer {10..90})
       begin
           let y : integer {0..100} = x;
       end
      " "0";
    enable_test !Tcheck.enable_constraint_checks
      (test_static_error globals "integer subtyping (assignment)"
      "func F(x : integer {0..100})
       begin
           let y : integer {10..90} = x;
       end
      "
      (Some "TypeError(file \"\" line 3 char 11 - 40,`{0..100}` is not a subrange of `{10..90}`)")
      None);
    test_static globals false "var decls"
      "func F(x : bits(8*N))
       begin
           var a : bits(8*N) = UNKNOWN : bits(8*N);
           var b : bits(8*N) = Zeros(8*N);
           var c : bits(8*N);
           var d = UNKNOWN : bits(8*N);
           var - = 1;
           var (f, g) = (1, TRUE);
           var (h : integer, i : boolean) = (1, TRUE);
           var (j : integer, - : boolean) = (1, TRUE);
           var arr1 : array [8] of integer;
           var arr2 : array [type boolean] of integer;

           let m : bits(8*N) = UNKNOWN : bits(8*N);
           let n : bits(8*N) = Zeros(8*N);
           let o = UNKNOWN : bits(8*N);
           let - = 1;
           let (p, q) = (1, TRUE);
           let (r : integer, s : boolean) = (1, TRUE);
           let (t : integer, _ : boolean) = (1, TRUE);
       end" "1";
    test_static globals false "case statements"
      "func F(x : bits(3), y : boolean) => integer
       begin
           case x of
               when '000' => return 0;
               when '001' => return 0;
               when '01x' => return 2;
               when '100', '111' => return 4;
               when '101' where y => return 5;
               otherwise => return 6;
           end
       end" "1";
    ("operators (implies)",    `Quick, test_bool globals prelude "" "FALSE --> FALSE" true);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "FALSE --> TRUE" true);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "TRUE --> FALSE" false);
    ("operators (implies)",    `Quick, test_bool globals prelude "" "TRUE --> TRUE" true);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "FALSE <-> FALSE" true);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "FALSE <-> TRUE" false);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "TRUE <-> FALSE" false);
    ("operators (iff)",        `Quick, test_bool globals prelude "" "TRUE <-> TRUE" true);
    ("let-expressions",        `Quick, test_int globals prelude "" "__let x : integer = 2 __in x+x" 4);
    ("prelude (cvt_bits_str)", `Quick, test_string globals prelude "" "asl_cvt_bits_str(1, '0')" "1'x0");
    ("prelude (HexStr (int))", `Quick, test_string globals prelude "" "HexStr(15)" "0xf");
    ("prelude (+ (int))",      `Quick, test_int globals prelude "" "1+1" 2);
    ("prelude (DIV)",          `Quick, test_int globals prelude "" "5 DIV 3" 1);
    ("prelude (mul_bits_int)", `Quick, test_bits globals prelude "" "'00111' * 3" "10101"); (* == 21 *)
    ("prelude (DecStr)",       `Quick, test_string globals prelude "" "DecStr('10101')" "21");
    ("prelude (HexStr)",       `Quick, test_string globals prelude "" "HexStr('10101')" "0x15");

    (* This regression test is for a typechecker bug where the typechecker did not handle negations
     * correctly (and subtractions 'x-y' were being transformed into 'x + (-y)' *)
    ("tcheck regression (neg_int)", `Quick, test_bits globals prelude
     "func F(i : integer, N : integer) => bits(N)
      begin
          var result = Ones(N);
          result[N-1 : i] = Zeros(N-i);
          return result;
      end"
    "F(3,8)" "00000111");

    ("setters (var)",          `Quick, test_bool globals prelude
       "var _A : boolean;
       getter A => boolean begin return _A; end
       setter A = v : boolean begin _A = v; end
       func Test(v : boolean) => boolean
       begin
           A = v;
           return A;
       end
       "
       "Test(TRUE)" true);
    ("bittuple LExpr_BitTuple", `Quick, test_bool globals prelude
     "func expand() => boolean
      begin
        var x : bits(4);
        var y : bits(8);
        var z : bits(12);
        [x, y, z] = '1111 01010101 000000000000';

        return (x == '1111' && y == '01010101' && z == '000000000000');
     end"
     "expand()" true);
    ("setters (array)",        `Quick, test_bool globals prelude
       "var _A : array [4] of boolean;
       getter A[i : integer] => boolean begin return _A[i]; end
       setter A[i : integer] = v : boolean begin _A[i] = v; end
       func Test(i : integer, v : boolean) => boolean
       begin
           A[i] = v;
           return A[i];
       end
       "
       "Test(1,TRUE)" true);
    ("parameterized record",   `Quick, test_bits globals prelude
       "record R(M) { x : bits(M); };
       func S(b : bits(M)) => R(M) begin return R(M){x=b}; end
       func T(r : R(M)) => bits(M) begin return r.x; end"
       "T(S('111'))" "111");
    ("parameterized type",     `Quick, test_bits globals prelude
       "type B(M) of bits(M);
       func T(b : bits(M)) => B(M) begin return b; end"
       "T('111')" "111");
    ("parameterized record extract", `Quick, test_bits globals prelude
       "record R(M) { x : bits(M); };
       func F() => bits(8)
       begin
           let a = R(3){ x = '111' };
           return ZeroExtend(a.x, 8);
       end"
       "F()" "00000111");
    ("statements (while)",     `Quick, test_int globals prelude
      "func TestWhile(x : integer) => integer begin var i : integer = 0; while i < x do i = i + 1; end return i; end"
      "TestWhile(3)" 3);
    ("statements (repeat)",    `Quick, test_int globals prelude
      "func TestRepeat(x : integer) => integer begin var i : integer = 0; repeat i = i + 1; until i >= x; return i; end"
      "TestRepeat(3)" 3);
    ("statements (array init)", `Quick, test_bits globals prelude
      "func TestArrayInit(x : integer) => bits(3) begin
          var a = array ('100', '101', '110', '111');
          return a[x];
       end
      "
      "TestArrayInit(2)" "110");
    ("tuple transform",        `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "func F() => (integer, integer) begin return (1,2); end
       func T() => integer begin let (x, y) = F(); return x + y; end"
      "T() == 3");
    ("tuple transform (in getter)", `Quick, test_xform globals prelude Xform_tuples.xform_decls
      "func F() => (integer, integer) begin return (1,2); end
       getter T => integer begin let (x, y) = F(); return x + y; end"
      "T == 3");
  ]

let () = Alcotest.run "libASL" [ ("asl", tests) ]

(****************************************************************
 * End
 ****************************************************************)
