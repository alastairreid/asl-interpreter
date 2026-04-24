(****************************************************************
 * Test cases for backends
 *
 * Copyright (C) 2024-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

type backend = Backend_C | Backend_Verilog
type test_case = string * backend list * string

let expr : test_case list =
  [
    ( "Bitslice lowd (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "function F(x : Bits(127)) => Bits(65) begin return x[4 +: 65]; end" );

    ( "literal int",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 01_000; end" );

    ( "literal int (negative)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return -01_000; end" );

    ( "literal int (int64 max)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 9223372036854775807; end" );

    ( "literal int (int64 max + 1)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 9223372036854775808; end" );

    ( "literal int (int64 min)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return -9223372036854775808; end" );

    ( "literal int (int64 min - 1)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return -9223372036854775809; end" );

    ( "literal int (int128 max)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 170141183460469231731687303715884105727; end" );

    ( "literal int (int128 min)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return -170141183460469231731687303715884105728; end" );

    ( "literal hex",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 0x01_0; end" );

    ( "literal hex (negative)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return -0x01_0; end" );

    ( "literal bitvector",
      [ Backend_C; Backend_Verilog ],
      "function F() => Bits(8) begin return 0b1111_0000; end" );

    ( "literal bitvector (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "function F() => Bits(65)
       begin
           return 0b1_0111111111111111111111111111111111111111111111111111111111110000;
       end" );

    ( "literal String",
      [ Backend_C; Backend_Verilog ],
      "function F() => String begin return \"str\"; end" );

    ( "literal String with escapes",
      [ Backend_C; Backend_Verilog ],
      "function F() => String begin return \"Hello \\\" World\"; end" );

    ( "function call",
      [ Backend_C; Backend_Verilog ],
      "function B() => Integer begin return 0; end function F() => Integer begin return B(); end" );

    ( "parentheses",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return ( 0 ); end" );

    ( "bitvector concatenation",
      [ Backend_Verilog ],
      "function F(x : Bits(8), y : Bits(4), z : Bits(2)) => Bits(14) begin return [x, y, z]; end" );
  ]

let int_ops : test_case list =
  [
  ]

let enum_ops : test_case list =
  [
  ]

let bit_ops : test_case list =
  [
  ]

let ram_ops : test_case list =
  [
  ]

let misc_ops : test_case list =
  [
    ( "built-in procedure call (Std::Print::Char)",
      [ Backend_C; Backend_Verilog ],
      "function F() begin Std::Print::Char(0); end" );

    ( "built-in procedure call (Std::Print::String)",
      [ Backend_C; Backend_Verilog ],
      "function F() begin Std::Print::String(\"a String\"); end" );
  ]

let fun_decl : test_case list  =
  [
    ( "built-in",
      [ Backend_C; Backend_Verilog ],
      "__builtin function f() => Integer;" );

    ( "type",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer;" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "function F() => Integer begin return 0; end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "function F(x : Integer, y : Integer) => Integer begin return 0; end" );
  ]

let proc_decl : test_case list  =
  [
    ( "type",
      [ Backend_C; Backend_Verilog ],
      "function F();" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "function F() begin end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "function F(x : Integer, y : Integer) begin end" );
  ]

let stmt : test_case list  =
  [
    ( "uninitialized variable",
      [ Backend_C; Backend_Verilog ],
      "function F() begin var x : Integer; end" );

    ( "uninitialized variables",
      [ Backend_C; Backend_Verilog ],
      "function F() begin var x, y : Integer; end" );

    ( "uninitialized variables (Std::RAM)",
      [ Backend_C; Backend_Verilog ],
      "function F() begin var x, y : Std::RAM(8); end" );

    ( "variable (Std::RAM)",
      [ Backend_C; Backend_Verilog ],
      "function F() begin var x : Std::RAM(8); end" );

    ( "procedure call",
      [ Backend_C; Backend_Verilog ],
      "function B() begin end function F() begin B(); end" );

    ( "procedure call with argument",
      [ Backend_C; Backend_Verilog ],
      "function B(x : Integer) begin end function F() begin B(0); end" );

    ( "procedure return",
      [ Backend_C; Backend_Verilog ],
      "function F() begin return; end" );

    ( "assert",
      [ Backend_C; Backend_Verilog ],
      "function F() begin assert False; end" );

  ]

let type_decl : test_case list  =
  [
    ( "built-in (String)",
      [ Backend_C; Backend_Verilog ],
      "__builtin type String;" );

    ( "built-in (Std::RAM)",
      [ Backend_C ],
      "__builtin type Std::RAM;" );

    ( "typedef",
      [ Backend_C; Backend_Verilog ],
      "type Byte = Bits(8);" );

    ( "typedef (register)",
      [ Backend_C; Backend_Verilog ],
      "bitfield Reg = { i => [8], b => [1] } : Bits(9);" );
  ]

let var_decl : test_case list  =
  [
    ( "Integer",
      [ Backend_C; Backend_Verilog ],
      "var x : Integer;" );

    ( "Std::RAM",
      [ Backend_C; Backend_Verilog ],
      "var x : Std::RAM(8);" );

    ( "const (Integer)",
      [ Backend_C ],
      "let x : Integer = 0;" );
  ]

(****************************************************************
 * End
 ****************************************************************)
