(****************************************************************
 * ASL lexer support
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL lexer support *)

open Lexing
open Asl_parser

let string_of_token (t : Asl_parser.token) : string =
  match t with
  | AMPERSAND_AMPERSAND -> "ampamp"
  | AND -> "and"
  | ARRAY -> "array"
  | AS -> "as"
  | ASSERT -> "assert"
  | BANG -> "bang"
  | BAR_BAR -> "barbar"
  | BITS -> "bits"
  | BEGIN -> "begin"
  | BITSLIT x -> Value.string_of_value (VBits x)
  | UNDERSCORE_UNDERSCORE_ASSERT -> "__assert"
  | UNDERSCORE_UNDERSCORE_BUILTIN -> "__builtin"
  | UNDERSCORE_UNDERSCORE_IN -> "__in"
  | UNDERSCORE_UNDERSCORE_LET -> "__let"
  | UNDERSCORE_UNDERSCORE_OPERATOR_ONE -> "__operator1"
  | UNDERSCORE_UNDERSCORE_OPERATOR_TWO -> "__operator2"
  | CARET -> "caret"
  | CASE -> "case"
  | CATCH -> "catch"
  | COLON -> "colon"
  | COMMA -> "comma"
  | CONFIG -> "config"
  | CONSTANT -> "constant"
  | DIV -> "div"
  | DIVRM -> "divrm"
  | DO -> "do"
  | DOT -> "dot"
  | DOT_DOT -> "dotdot"
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | ELSIF -> "elsif"
  | ENUMERATION -> "enum"
  | EXCEPTION -> "exception"
  | EOF -> "eof"
  | EQ -> "eq"
  | EQ_EQ -> "eqeq"
  | EQ_GT -> "eqgt"
  | REALLIT x -> "real:" ^ x
  | END -> "end"
  | FOR -> "for"
  | FUNC -> "func"
  | GETTER -> "getter"
  | GT -> "gt"
  | GT_EQ -> "gteq"
  | GT_GT -> "gtgt"
  | ID x -> "ident:" ^ x
  | IF -> "if"
  | IN -> "in"
  | INTEGER -> "integer"
  | INTLIT (None, x) -> "int:" ^ Z.to_string x
  | INTLIT (Some w, x) -> "intN:" ^ string_of_int w ^"'d"^ Z.to_string x
  | LBRACE -> "lbrace"
  | LBRACK -> "lbrack"
  | LET -> "let"
  | LPAREN -> "lparen"
  | LT -> "lt"
  | LT_EQ -> "lteq"
  | LT_MINUS_GT -> "iff"
  | LT_LT -> "ltlt"
  | MASKLIT x -> "mask:" ^ x
  | MINUS -> "minus"
  | MINUS_MINUS_GT -> "implies"
  | MOD -> "mod"
  | BANG_EQ -> "neq"
  | NOT -> "not"
  | OF -> "of"
  | OR -> "or"
  | OTHERWISE -> "otherwise"
  | PLUS -> "plus"
  | PLUS_PLUS -> "plusplus"
  | PLUS_COLON -> "pluscolon"
  | MINUS_COLON -> "minuscolon"
  | STAR_COLON -> "starcolon"
  | QUERY -> "query"
  | QUOT -> "quot"
  | RBRACE -> "rbrace"
  | RBRACK -> "rbrack"
  | RECORD -> "record"
  | REM -> "rem"
  | REPEAT -> "repeat"
  | RETURN -> "return"
  | RPAREN -> "rparen"
  | SEMICOLON -> "semi"
  | SETTER -> "setter"
  | SLASH -> "slash"
  | STAR -> "star"
  | STRINGLIT x -> "string:" ^ x
  | THEN -> "then"
  | THROW -> "throw"
  | TO -> "to"
  | TRY -> "try"
  | TYPE -> "type"
  | TYPEOF -> "typeof"
  | UNKNOWN -> "unknown"
  | UNTIL -> "until"
  | VAR -> "var"
  | WHEN -> "when"
  | WHERE -> "where"
  | WHILE -> "while"
  | XOR -> "xor"

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(****************************************************************
 * End
 ****************************************************************)
