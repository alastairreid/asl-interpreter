(****************************************************************
 * Test constant propagation transform
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
 * Test constant propagation
 ****************************************************************)

(** Test xform_expr *)
let test_cp_expr (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let genv = Eval.build_constant_environment (prelude @ ds) in
  let env = Xform_constprop.mkEnv genv [] in
  let l' = LoadASL.read_expr tcenv Loc.Unknown l in
  let what = l ^ " == " ^ r in
  Alcotest.check Alcotest.string what r (Asl_utils.pp_expr (Xform_constprop.xform_expr env l'))

let constprop_tests : unit Alcotest.test_case list =
  let prelude = load_test_libraries () in
  let globals = TC.env0 in

  let test_cp_decls (decls : string) (l : string) (r : string) () : unit =
    let (tcenv, ds) = extend_tcenv globals decls in
    let genv = Eval.build_constant_environment (prelude @ ds) in
    test_xform_decls (Xform_constprop.xform_decls genv) globals prelude decls l r ()
  in

  let test_cp_stmts (decls : string) (l : string) (r : string) () : unit =
    let (tcenv, ds) = extend_tcenv globals decls in
    let genv = Eval.build_constant_environment (prelude @ ds) in
    let env = Xform_constprop.mkEnv genv [] in
    test_xform_stmts (Xform_constprop.xform_stmts env) globals prelude decls l r ()
  in
  [
    ("add", `Quick, test_cp_expr globals prelude "" "1 + 1" "2");
    ("add_mul", `Quick, test_cp_expr globals prelude "" "1 + (2 * 3)" "7");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 1" "TRUE");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 2" "FALSE");
    ("IN", `Quick, test_cp_expr globals prelude "" "8 IN {8, 16}" "TRUE");
    ("enum", `Quick, test_cp_expr globals prelude
       "enumeration T { E1, E2 };" "if TRUE then E1 else E2" "E1");
    ("Replicate(_, 0)", `Quick, test_cp_expr globals prelude
       "var x : bits(8);" "Replicate(x, 0)" "0'x0");
    ("Replicate(x, 1)", `Quick, test_cp_expr globals prelude
       "var x : bits(8);" "Replicate(x, 1)" "x");
    ("assert expr dead code", `Quick, test_cp_expr globals prelude
       "var x : bits(8);" "__assert TRUE __in x" "x");
    ("assert expr live code 1", `Quick, test_cp_expr globals prelude
       "var x : bits(8);" "__assert FALSE __in x" "__assert FALSE __in x");
    ("assert expr live code 2", `Quick, test_cp_expr globals prelude
       "var x : bits(8);" "__assert IsZero(x) __in x" "__assert IsZero.0(x) __in x");

    ("bits(SIZE)", `Quick, test_cp_decls
     "constant SIZE : integer = 32;"
     "var V : bits(SIZE);"
     "var V : bits(32);");

    ("let", `Quick, test_cp_stmts ""
       "let x : integer = 1 + 1;" "let x : integer = 2;");
    ("assignment", `Quick, test_cp_stmts ""
      "let c : bits(2) = '11';
       let a : bits(2) = c;"
      "let c : bits(2) = '11'; let a : bits(2) = '11';");
    ("bittuple declaration", `Quick, test_cp_stmts "let N : integer = 2;"
      "let [a : bits(N), b : bits(1)] = '111';"
      "let [a : bits(2), b : bits(1)] = '111';");
    ("if-else", `Quick, test_cp_stmts "var d : boolean;"
      "var c : bits(2);
       if d then
           c = '11';
       else
           c = '11';
       end
       let a = c;"
      "var c : bits(2);
       if d then
           c = '11';
       else
           c = '11';
       end
       let a : bits(2) = '11';");
    ("if-else dead statement elimination 1", `Quick, test_cp_stmts "let b : boolean = TRUE;"
      "if b then
           let c = 1;
       else
           let c = 0;
       end"
      "let c = 1;");
    ("if-else dead statement elimination 2", `Quick, test_cp_stmts "let b : boolean = FALSE;"
      "if b then
           let c = 1;
       else
           let c = 0;
       end"
      "let c = 0;");
    ("if-else dead expression elimination 1", `Quick, test_cp_stmts "let b : boolean = TRUE; var x : integer; var y : integer;"
      "let c = if b then x else y;"
      "let c = x;");
    ("if-else dead expression elimination 2", `Quick, test_cp_stmts "let b : boolean = FALSE; var x : integer; var y : integer;"
      "let c = if b then x else y;"
      "let c = y;");
    (* Make sure c = '11' gets propagated to after loop *)
    ("for loop 1", `Quick, test_cp_stmts "var d : integer;"
      "let c = '11';
       var x : integer;
       for i = 0 to d do
           x = 0;
       end
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       for i = 0 to d do
           x = 0;
       end
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("for loop 2", `Quick, test_cp_stmts "var d : integer;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '10';
       end
       let a = c;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '10';
       end
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("for loop 3", `Quick, test_cp_stmts "var d : integer;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '11';
       end
       let a = c;"
      "var c : bits(2) = '10';
       for i = 0 to d do
           c = '11';
       end
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("for loop 4", `Quick, test_cp_stmts "var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       for i = 0 to n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       for i = 0 to n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");

    (* Make sure c = '11' gets propagated to after loop *)
    ("while loop 1", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "let c = '11';
       var x : integer;
       while i != d do
           x = 0;
       end
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       while i != d do
           x = 0;
       end
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("while loop 2", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       while i != d do
           c = '10';
       end
       let a = c;"
      "var c : bits(2) = '10';
       while i != d do
           c = '10';
       end
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("while loop 3", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       while i != d do
           c = '11';
       end
       let a = c;"
      "var c : bits(2) = '10';
       while i != d do
           c = '11';
       end
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("while loop 4", `Quick, test_cp_stmts "var i : integer; var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       while i != n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       while i != n do
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       end
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");

    (* Make sure c = '11' gets propagated to after loop *)
    ("repeat loop 1", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "let c = '11';
       var x : integer;
       repeat
           x = 0;
       until i != d;
       let a = c;"
      "let c : bits(2) = '11';
       var x : integer;
       repeat
           x = 0;
       until i != d;
       let a : bits(2) = '11';");

    (* Make sure we still except c to be '10' after loop *)
    ("repeat loop 2", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       repeat
           c = '10';
       until i != d;
       let a = c;"
      "var c : bits(2) = '10';
       repeat
           c = '10';
       until i != d;
       let a : bits(2) = '10';");

    (* Make sure a = c is intact when altering c inside loop. *)
    ("repeat loop 3", `Quick, test_cp_stmts "var i : integer; var d : integer;"
      "var c : bits(2) = '10';
       repeat
           c = '11';
       until i != d;
       let a = c;"
      "var c : bits(2) = '10';
       repeat
           c = '11';
       until i != d;
       let a : bits(2) = c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("repeat loop 4", `Quick, test_cp_stmts "var i : integer; var n : integer;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       repeat
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       until i != n;
       let a = x1;
       let b = x2;
       let c = x3;
       let d = x4;"
      "var x1 : integer = 0;
       var x2 : integer = 0;
       var x3 : integer = 0;
       var x4 : integer = 1;
       repeat
           x3 = x2;
           x2 = x1;
           x1 = 1;
           x4 = 1;
       until i != n;
       let a = x1;
       let b = x2;
       let c = x3;
       let d : integer = 1;");
    ("pattern in case stmt" , `Quick, test_cp_stmts
      "constant a : boolean = TRUE;
       constant b : integer = 1;
       constant c : bits(1) = '1';
       enumeration T { E1, E2 }; constant d : T = E2;
       var va : boolean;
       var vb : integer;
       var vc : bits(1);
       var vd : T;
      "

      "case va of when a => return; end
       case vb of when b => return; end
       case vc of when c => return; end
       case vd of when d => return; end"

      "case va of when TRUE => return; end
       case vb of when 1 => return; end
       case vc of when '1' => return; end
       case vd of when E2 => return; end");

    ("case stmt integers", `Quick, test_cp_stmts
     "var i : integer; func Foo(x : integer) begin end"
     "case i of
         when 16 => Foo(i);
         when 0x20 => Foo(i);
      end"
     "case i of
         when 16 => Foo(16);
         when 0x20 => Foo(32);
      end");
    ("case stmt bitvectors", `Quick, test_cp_stmts
     "var i : bits(8); func Foo(x : bits(8)) begin end"
     "case i of
         when '11110000' => Foo(i);
         when '10101010' => Foo(i);
      end"
     "case i of
         when '11110000' => Foo('11110000');
         when '10101010' => Foo('10101010');
      end");
    ("case stmt list", `Quick, test_cp_stmts
     "var i : integer; func Foo(x : integer) begin end"
     "case i of
         when 16, 32 => Foo(i);
      end"
     "case i of
         when 16, 32 => Foo(i);
      end");
    ("case stmt tuple", `Quick, test_cp_stmts
     "var i : integer; var j : integer; func Foo(x : integer, y : integer) begin end"
     "case (i, j) of
         when (16, 32) => Foo(i, j);
         when (24, 64) => Foo(i, j);
      end"
     "case (i, j) of
         when (16, 32) => Foo(16, 32);
         when (24, 64) => Foo(24, 64);
      end");
    ("case stmt nested tuple", `Quick, test_cp_stmts
     "var i : integer; var j : integer; var k : integer; func Foo(x : integer, y : integer, z : integer) begin end"
     "case (i, (j, k)) of
         when (16, (32, 64)) => Foo(i, j, k);
         when (24, (64, 32)) => Foo(i, j, k);
      end"
     "case (i, (j, k)) of
         when (16, (32, 64)) => Foo(16, 32, 64);
         when (24, (64, 32)) => Foo(24, 64, 32);
      end");
    ("case stmt missing const 1", `Quick, test_cp_stmts
     "var i : integer; var j : integer; func Foo(x : integer, y : integer) begin end"
     "case (i, j) of
         when (16, -) => Foo(i, j);
         when (24, -) => Foo(i, j);
      end"
     "case (i, j) of
         when (16, -) => Foo(16, j);
         when (24, -) => Foo(24, j);
      end");
    ("case stmt missing const 2", `Quick, test_cp_stmts
     "var i : integer; func Foo(x : integer) begin end"
     "case i of
         when 32 => Foo(i);
         when - => Foo(i);
      end"
     "case i of
         when 32 => Foo(32);
         when - => Foo(i);
      end");
    ("case stmt missing const 3", `Quick, test_cp_stmts
     "var i : integer; var j : integer; func Foo(x : integer, y : integer) begin end"
     "case (i, j) of
         when (24, 32) => Foo(i, j);
         when (16, -) => Foo(i, j);
      end"
     "case (i, j) of
         when (24, 32) => Foo(24, 32);
         when (16, -) => Foo(16, j);
      end");
    ("case stmt missing const 4", `Quick, test_cp_stmts
     "var i : integer; var j : integer; func Foo(x : integer, y : integer) begin end"
     "case (i, j) of
         when (16, 32) => Foo(i, j);
         when (24, {2..4}) => Foo(i, j);
      end"
     "case (i, j) of
         when (16, 32) => Foo(16, 32);
         when (24, {2..4}) => Foo(24, j);
      end");
    ("case stmt propagate x", `Quick, test_cp_stmts
     "var i : integer; var y : integer;"
     "var x : integer;
      case i of
         when 16 => x = 5;
         when 24 => x = 5;
         otherwise => x = 5;
      end
      y = x;
      "
     "var x : integer;
      case i of
         when 16 => x = 5;
         when 24 => x = 5;
         otherwise => x = 5;
      end
      y = 5;");
    ("case stmt cannot propagate x", `Quick, test_cp_stmts
     "var i : integer; var y : integer;"
     "var x : integer;
      case i of
         when 16 => x = 5;
         when 24 => x = 5;
      end
      y = x;
      "
     "var x : integer;
      case i of
         when 16 => x = 5;
         when 24 => x = 5;
      end
      y = x;");
    ("if stmt dead code (then)", `Quick, test_cp_stmts
     "var x : integer;"
     "if 1 < 2 then x = 1; else x = 2; end"
     "x = 1;");
    ("if stmt dead code (else)", `Quick, test_cp_stmts
     "var x : integer;"
     "if 1 > 2 then x = 1; else x = 2; end"
     "x = 2;");
    ("case stmt dead code (single)", `Quick, test_cp_stmts
     "var x : integer;"
     "case 3 of when 1,5 => x = 0; when 2 => x =  1; when 3 => x =  2; when 4 => x =  3; end"
     "x = 2;");
    ("case stmt dead code (multi)", `Quick, test_cp_stmts
     "var x : integer;"
     "case 5 of when 1,5 => x = 0; when 2 => x =  1; when 3 => x =  2; when 4 => x =  3; end"
     "x = 0;");
    ("case stmt dead code (if 1)", `Quick, test_cp_stmts
     "var x : integer;
      var y : integer;"
     "case 3 of when 1 => x = 0; when 2 where y < 0 => x =  1; when 3 where y > 0 => x =  2; when 4 => x =  3; end"
     "case 3 of when - where y > 0 => x = 2; end");
    ("case stmt dead code (if 2)", `Quick, test_cp_stmts
     "var x : integer;
      var y : integer;"
     "case 3 of when 1 => x = 0; when 3 where y < 0 => x =  1; when 3 where y > 0 => x =  2; when 4 => x =  3; end"
     "case 3 of when - where y < 0 => x = 1; when - where y > 0 => x = 2; end");
    ("assert stmt dead code", `Quick, test_cp_stmts
     ""
     "assert TRUE;"
     "");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "asl_utils" [
    ("constprop", constprop_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
