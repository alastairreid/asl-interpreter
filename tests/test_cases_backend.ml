(****************************************************************
 * Test cases for backends
 *
 * Copyright (C) 2024-2024 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type backend = Backend_C | Backend_Verilog
type test_case = string * backend list * string

let expr : test_case list =
  [
    ( "bitslice lowd (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F(x : bits(127)) => bits(65) begin return x[4 +: 65]; end" );

    ( "literal int",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 01_000; end" );

    ( "literal int (negative)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -01_000; end" );

    ( "literal int (int64 max)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 9223372036854775807; end" );

    ( "literal int (int64 max + 1)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 9223372036854775808; end" );

    ( "literal int (int64 min)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -9223372036854775808; end" );

    ( "literal int (int64 min - 1)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -9223372036854775809; end" );

    ( "literal int (int128 max)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 170141183460469231731687303715884105727; end" );

    ( "literal int (int128 min)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -170141183460469231731687303715884105728; end" );

    ( "literal hex",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 0x01_0; end" );

    ( "literal hex (negative)",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return -0x01_0; end" );

    ( "literal bitvector",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(8) begin return '1111 0000'; end" );

    ( "literal bitvector (> 64b)",
      [ Backend_C; Backend_Verilog ],
      "func F() => bits(65)
       begin
           return '1 0111111111111111111111111111111111111111111111111111111111110000';
       end" );

    ( "literal string",
      [ Backend_C; Backend_Verilog ],
      "func F() => string begin return \"str\"; end" );

    ( "literal string with escapes",
      [ Backend_C; Backend_Verilog ],
      "func F() => string begin return \"Hello \\\" World\"; end" );

    ( "function call",
      [ Backend_C; Backend_Verilog ],
      "func B() => integer begin return 0; end func F() => integer begin return B(); end" );

    ( "parentheses",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return ( 0 ); end" );

    ( "bitvector concatenation",
      [ Backend_Verilog ],
      "func F(x : bits(8), y : bits(4), z : bits(2)) => bits(14) begin return [x, y, z]; end" );
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
    ( "built-in procedure call (print_char)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_char(0); end" );

    ( "built-in procedure call (print_str)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin print_str(\"a string\"); end" );
  ]

let fun_decl : test_case list  =
  [
    ( "built-in",
      [ Backend_C; Backend_Verilog ],
      "__builtin func f() => integer;" );

    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer;" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() => integer begin return 0; end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer, y : integer) => integer begin return 0; end" );
  ]

let proc_decl : test_case list  =
  [
    ( "type",
      [ Backend_C; Backend_Verilog ],
      "func F();" );

    ( "definition",
      [ Backend_C; Backend_Verilog ],
      "func F() begin end" );

    ( "definition with params",
      [ Backend_C; Backend_Verilog ],
      "func F(x : integer, y : integer) begin end" );
  ]

let stmt : test_case list  =
  [
    ( "uninitialized variable",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : integer; end" );

    ( "uninitialized variables",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x, y : integer; end" );

    ( "uninitialized variables (__RAM)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x, y : __RAM(8); end" );

    ( "variable (__RAM)",
      [ Backend_C; Backend_Verilog ],
      "func F() begin var x : __RAM(8); end" );

    ( "procedure call",
      [ Backend_C; Backend_Verilog ],
      "func B() begin end func F() begin B(); end" );

    ( "procedure call with argument",
      [ Backend_C; Backend_Verilog ],
      "func B(x : integer) begin end func F() begin B(0); end" );

    ( "procedure return",
      [ Backend_C; Backend_Verilog ],
      "func F() begin return; end" );

    ( "assert",
      [ Backend_C; Backend_Verilog ],
      "func F() begin assert FALSE; end" );

  ]

let type_decl : test_case list  =
  [
    ( "built-in (real)",
      [ Backend_C ],
      "__builtin type real;" );

    ( "built-in (string)",
      [ Backend_C; Backend_Verilog ],
      "__builtin type string;" );

    ( "built-in (__mask)",
      [ Backend_C ],
      "__builtin type __mask;" );

    ( "built-in (__RAM)",
      [ Backend_C ],
      "__builtin type __RAM;" );

    ( "typedef",
      [ Backend_C; Backend_Verilog ],
      "type Byte of bits(8);" );

    ( "typedef (register)",
      [ Backend_C; Backend_Verilog ],
      "type Reg of bits(9) { [8] i [1] b };" );
  ]

let var_decl : test_case list  =
  [
    ( "integer",
      [ Backend_C; Backend_Verilog ],
      "var x : integer;" );

    ( "__RAM",
      [ Backend_C; Backend_Verilog ],
      "var x : __RAM(8);" );

    ( "const (integer)",
      [ Backend_C ],
      "let x : integer = 0;" );
  ]

(****************************************************************
 * End
 ****************************************************************)
