(****************************************************************
 * Test constant propagation transform
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
 * Test constant propagation
 ****************************************************************)

(** Test xform_expr *)
let test_cp_expr (globals : TC.GlobalEnv.t) (prelude : AST.declaration list) (decls : string)
    (l : string) (r : string) () : unit =
  let (tcenv, ds) = extend_tcenv globals decls in
  let genv = Eval.build_constant_environment (prelude @ ds) in
  let env = Xform_constprop.mkEnv genv [] in
  let l' = LoadISA.read_expr tcenv Loc.Unknown l in
  let what = l ^ " == " ^ r in
  Alcotest.check Alcotest.string what r (Isa_utils.pp_expr (Xform_constprop.xform_expr env l'))

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
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 1" "True");
    ("bool", `Quick, test_cp_expr globals prelude "" "1 == 2" "False");
    ("in", `Quick, test_cp_expr globals prelude "" "8 in {8, 16}" "True");
    ("enum", `Quick, test_cp_expr globals prelude
       "enumeration T { E1, E2 };" "if True then E1 else E2" "E1");
    ("assert expr dead code", `Quick, test_cp_expr globals prelude
       "var x : Bits(8);" "__assert True __in x" "x");
    ("assert expr live code 1", `Quick, test_cp_expr globals prelude
       "var x : Bits(8);" "__assert False __in x" "(__assert False __in x)");
    ("assert expr live code 2", `Quick, test_cp_expr globals prelude
       "var x : Bits(8);" "__assert Std::Bits::Is_Zero(x) __in x" "(__assert Std::Bits::Is_Zero.0(x) __in x)");

    ("Bits(SIZE)", `Quick, test_cp_decls
     "let SIZE : Integer := 32;"
     "var V : Bits(SIZE);"
     "var V : Bits(32);");

    ("let", `Quick, test_cp_stmts ""
       "let x : Integer := 1 + 1;" "let x : Integer := 2;");
    ("assignment", `Quick, test_cp_stmts ""
      "let c : Bits(2) := 0b11;
       let a : Bits(2) := c;"
      "let c : Bits(2) := 0b11; let a : Bits(2) := 0b11;");
    ("if-else", `Quick, test_cp_stmts "var d : Boolean;"
      "var c : Bits(2);
       if d then
           c := 0b11;
       else
           c := 0b11;
       endif;
       let a := c;"
      "var c : Bits(2);
       if d then
           c := 0b11;
       else
           c := 0b11;
       endif;
       let a : Bits(2) := 0b11;");
    ("if-else dead statement elimination 1", `Quick, test_cp_stmts "let b : Boolean := True;"
      "if b then
           let c := 1;
       else
           let c := 0;
       endif;"
      "let c := 1;");
    ("if-else dead statement elimination 2", `Quick, test_cp_stmts "let b : Boolean := False;"
      "if b then
           let c := 1;
       else
           let c := 0;
       endif;"
      "let c := 0;");
    ("if-else dead expression elimination 1", `Quick, test_cp_stmts "let b : Boolean := True; var x : Integer; var y : Integer;"
      "let c := if b then x else y;"
      "let c := x;");
    ("if-else dead expression elimination 2", `Quick, test_cp_stmts "let b : Boolean := False; var x : Integer; var y : Integer;"
      "let c := if b then x else y;"
      "let c := y;");
    (* Make sure c := 0b11 gets propagated to after loop *)
    ("for loop 1", `Quick, test_cp_stmts "var d : Integer;"
      "let c := 0b11;
       var x : Integer;
       for i := 0 to d do
           x := 0;
       endfor;
       let a := c;"
      "let c : Bits(2) := 0b11;
       var x : Integer;
       for i := 0 to d do
           x := 0;
       endfor;
       let a : Bits(2) := 0b11;");

    (* Make sure we still except c to be 0b10 after loop *)
    ("for loop 2", `Quick, test_cp_stmts "var d : Integer;"
      "var c : Bits(2) := 0b10;
       for i := 0 to d do
           c := 0b10;
       endfor;
       let a := c;"
      "var c : Bits(2) := 0b10;
       for i := 0 to d do
           c := 0b10;
       endfor;
       let a : Bits(2) := 0b10;");

    (* Make sure a := c is intact when altering c inside loop. *)
    ("for loop 3", `Quick, test_cp_stmts "var d : Integer;"
      "var c : Bits(2) := 0b10;
       for i := 0 to d do
           c := 0b11;
       endfor;
       let a := c;"
      "var c : Bits(2) := 0b10;
       for i := 0 to d do
           c := 0b11;
       endfor;
       let a : Bits(2) := c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("for loop 4", `Quick, test_cp_stmts "var n : Integer;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       for i := 0 to n do
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       endfor;
       let a := x1;
       let b := x2;
       let c := x3;
       let d := x4;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       for i := 0 to n do
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       endfor;
       let a := x1;
       let b := x2;
       let c := x3;
       let d : Integer := 1;");

    (* Make sure c := 0b11 gets propagated to after loop *)
    ("while loop 1", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "let c := 0b11;
       var x : Integer;
       while i != d do
           x := 0;
       endwhile;
       let a := c;"
      "let c : Bits(2) := 0b11;
       var x : Integer;
       while i != d do
           x := 0;
       endwhile;
       let a : Bits(2) := 0b11;");

    (* Make sure we still except c to be 0b10 after loop *)
    ("while loop 2", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "var c : Bits(2) := 0b10;
       while i != d do
           c := 0b10;
       endwhile;
       let a := c;"
      "var c : Bits(2) := 0b10;
       while i != d do
           c := 0b10;
       endwhile;
       let a : Bits(2) := 0b10;");

    (* Make sure a := c is intact when altering c inside loop. *)
    ("while loop 3", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "var c : Bits(2) := 0b10;
       while i != d do
           c := 0b11;
       endwhile;
       let a := c;"
      "var c : Bits(2) := 0b10;
       while i != d do
           c := 0b11;
       endwhile;
       let a : Bits(2) := c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("while loop 4", `Quick, test_cp_stmts "var i : Integer; var n : Integer;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       while i != n do
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       endwhile;
       let a := x1;
       let b := x2;
       let c := x3;
       let d := x4;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       while i != n do
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       endwhile;
       let a := x1;
       let b := x2;
       let c := x3;
       let d : Integer := 1;");

    (* Make sure c := 0b11 gets propagated to after loop *)
    ("repeat loop 1", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "let c := 0b11;
       var x : Integer;
       repeat
           x := 0;
       until i != d;
       let a := c;"
      "let c : Bits(2) := 0b11;
       var x : Integer;
       repeat
           x := 0;
       until i != d;
       let a : Bits(2) := 0b11;");

    (* Make sure we still except c to be 0b10 after loop *)
    ("repeat loop 2", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "var c : Bits(2) := 0b10;
       repeat
           c := 0b10;
       until i != d;
       let a := c;"
      "var c : Bits(2) := 0b10;
       repeat
           c := 0b10;
       until i != d;
       let a : Bits(2) := 0b10;");

    (* Make sure a := c is intact when altering c inside loop. *)
    ("repeat loop 3", `Quick, test_cp_stmts "var i : Integer; var d : Integer;"
      "var c : Bits(2) := 0b10;
       repeat
           c := 0b11;
       until i != d;
       let a := c;"
      "var c : Bits(2) := 0b10;
       repeat
           c := 0b11;
       until i != d;
       let a : Bits(2) := c;");

    (* This will trigger more than 2 fixpoint iterations *)
    ("repeat loop 4", `Quick, test_cp_stmts "var i : Integer; var n : Integer;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       repeat
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       until i != n;
       let a := x1;
       let b := x2;
       let c := x3;
       let d := x4;"
      "var x1 : Integer := 0;
       var x2 : Integer := 0;
       var x3 : Integer := 0;
       var x4 : Integer := 1;
       repeat
           x3 := x2;
           x2 := x1;
           x1 := 1;
           x4 := 1;
       until i != n;
       let a := x1;
       let b := x2;
       let c := x3;
       let d : Integer := 1;");
    ("bool pattern in case stmt" , `Quick, test_cp_stmts
      "let a : Boolean := True;
       var va : Boolean;"
      "case va of when a => return; endcase;"
      "case va of when True => return; endcase;");
    ("int pattern in case stmt" , `Quick, test_cp_stmts
      "let b : Integer := 1;
       var vb : Integer;"
      "case vb of when b => return; endcase;"
      "case vb of when 1 => return; endcase;");

    ("bit pattern in case stmt" , `Quick, test_cp_stmts
      "let c : Bits(1) := 0b1;
       var vc : Bits(1);"
      "case vc of when c => return; endcase;"
      "case vc of when 0b1 => return; endcase;");
    ("enum pattern in case stmt" , `Quick, test_cp_stmts
      "enumeration T { E1, E2 }; let d : T := E2;
       var vd : T;"
      "case vd of when d => return; endcase;"
      "case vd of when E2 => return; endcase;");
    ("case stmt Integers", `Quick, test_cp_stmts
     "var i : Integer; function Foo(x : Integer) begin end"
     "case i of
         when 16 => Foo(i);
         when 0x20 => Foo(i);
      endcase;"
     "case i of
         when 16 => Foo(16);
         when 0x20 => Foo(32);
      endcase;");
    ("case stmt bitvectors", `Quick, test_cp_stmts
     "var i : Bits(8); function Foo(x : Bits(8)) begin end"
     "case i of
         when 0b11110000 => Foo(i);
         when 0b10101010 => Foo(i);
      endcase;"
     "case i of
         when 0b11110000 => Foo(0b11110000);
         when 0b10101010 => Foo(0b10101010);
      endcase;");
    ("case stmt list", `Quick, test_cp_stmts
     "var i : Integer; function Foo(x : Integer) begin end"
     "case i of
         when 16, 32 => Foo(i);
      endcase;"
     "case i of
         when 16, 32 => Foo(i);
      endcase;");
    ("case stmt tuple", `Quick, test_cp_stmts
     "var i : Integer; var j : Integer; function Foo(x : Integer, y : Integer) begin end"
     "case (i, j) of
         when (16, 32) => Foo(i, j);
         when (24, 64) => Foo(i, j);
      endcase;"
     "case (i, j) of
         when (16, 32) => Foo(16, 32);
         when (24, 64) => Foo(24, 64);
      endcase;");
    ("case stmt nested tuple", `Quick, test_cp_stmts
     "var i : Integer; var j : Integer; var k : Integer; function Foo(x : Integer, y : Integer, z : Integer) begin end"
     "case (i, (j, k)) of
         when (16, (32, 64)) => Foo(i, j, k);
         when (24, (64, 32)) => Foo(i, j, k);
      endcase;"
     "case (i, (j, k)) of
         when (16, (32, 64)) => Foo(16, 32, 64);
         when (24, (64, 32)) => Foo(24, 64, 32);
      endcase;");
    ("case stmt missing const 1", `Quick, test_cp_stmts
     "var i : Integer; var j : Integer; function Foo(x : Integer, y : Integer) begin end"
     "case (i, j) of
         when (16, _) => Foo(i, j);
         when (24, _) => Foo(i, j);
      endcase;"
     "case (i, j) of
         when (16, _) => Foo(16, j);
         when (24, _) => Foo(24, j);
      endcase;");
    ("case stmt missing const 2", `Quick, test_cp_stmts
     "var i : Integer; function Foo(x : Integer) begin end"
     "case i of
         when 32 => Foo(i);
         when _ => Foo(i);
      endcase;"
     "case i of
         when 32 => Foo(32);
         when _ => Foo(i);
      endcase;");
    ("case stmt missing const 3", `Quick, test_cp_stmts
     "var i : Integer; var j : Integer; function Foo(x : Integer, y : Integer) begin end"
     "case (i, j) of
         when (24, 32) => Foo(i, j);
         when (16, _) => Foo(i, j);
      endcase;"
     "case (i, j) of
         when (24, 32) => Foo(24, 32);
         when (16, _) => Foo(16, j);
      endcase;");
    ("case stmt missing const 4", `Quick, test_cp_stmts
     "var i : Integer; var j : Integer; function Foo(x : Integer, y : Integer) begin end"
     "case (i, j) of
         when (16, 32) => Foo(i, j);
         when (24, {2..4}) => Foo(i, j);
      endcase;"
     "case (i, j) of
         when (16, 32) => Foo(16, 32);
         when (24, {2..4}) => Foo(24, j);
      endcase;");
    ("case stmt propagate x", `Quick, test_cp_stmts
     "var i : Integer; var y : Integer;"
     "var x : Integer;
      case i of
         when 16 => x := 5;
         when 24 => x := 5;
         otherwise => x := 5;
      endcase;
      y := x;
      "
     "var x : Integer;
      case i of
         when 16 => x := 5;
         when 24 => x := 5;
         otherwise => x := 5;
      endcase;
      y := 5;");
    ("case stmt cannot propagate x", `Quick, test_cp_stmts
     "var i : Integer; var y : Integer;"
     "var x : Integer;
      case i of
         when 16 => x := 5;
         when 24 => x := 5;
      endcase;
      y := x;
      "
     "var x : Integer;
      case i of
         when 16 => x := 5;
         when 24 => x := 5;
      endcase;
      y := x;");
    ("if stmt dead code (then)", `Quick, test_cp_stmts
     "var x : Integer;"
     "if 1 < 2 then x := 1; else x := 2; endif;"
     "x := 1;");
    ("if stmt dead code (else)", `Quick, test_cp_stmts
     "var x : Integer;"
     "if 1 > 2 then x := 1; else x := 2; endif;"
     "x := 2;");
    ("case stmt dead code (single)", `Quick, test_cp_stmts
     "var x : Integer;"
     "case 3 of when 1,5 => x := 0; when 2 => x :=  1; when 3 => x :=  2; when 4 => x :=  3; endcase;"
     "x := 2;");
    ("case stmt dead code (multi)", `Quick, test_cp_stmts
     "var x : Integer;"
     "case 5 of when 1,5 => x := 0; when 2 => x :=  1; when 3 => x :=  2; when 4 => x :=  3; endcase;"
     "x := 0;");
    ("case stmt dead code (if 1)", `Quick, test_cp_stmts
     "var x : Integer;
      var y : Integer;"
     "case 3 of when 1 => x := 0; when 2 if y < 0 => x :=  1; when 3 if y > 0 => x :=  2; when 4 => x :=  3; endcase;"
     "case 3 of when _ if y > 0 => x := 2; endcase;");
    ("case stmt dead code (if 2)", `Quick, test_cp_stmts
     "var x : Integer;
      var y : Integer;"
     "case 3 of when 1 => x := 0; when 3 if y < 0 => x :=  1; when 3 if y > 0 => x :=  2; when 4 => x :=  3; endcase;"
     "case 3 of when _ if y < 0 => x := 1; when _ if y > 0 => x := 2; endcase;");
    ("assert stmt dead code", `Quick, test_cp_stmts
     ""
     "assert True;"
     "");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "builtin_utils" [
    ("constprop", constprop_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
