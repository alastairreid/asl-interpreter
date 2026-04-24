(****************************************************************
 * ISA lexer
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

{
open Isa_parser       (* The type token is defined in parser.mli *)

exception Eof

let keywords : (string * Isa_parser.token) list = [
    ("UNSPECIFIED",            UNSPECIFIED);
    ("_",                      UNDERSCORE);
    ("__assert",               UNDERSCORE_UNDERSCORE_ASSERT);
    ("__builtin",              UNDERSCORE_UNDERSCORE_BUILTIN);
    ("__in",                   UNDERSCORE_UNDERSCORE_IN);
    ("__let",                  UNDERSCORE_UNDERSCORE_LET);
    ("__operator1",            UNDERSCORE_UNDERSCORE_OPERATOR_ONE);
    ("__operator2",            UNDERSCORE_UNDERSCORE_OPERATOR_TWO);
    ("and",                    AND);
    ("array",                  ARRAY);
    ("as",                     AS);
    ("assert",                 ASSERT);
    ("begin",                  BEGIN);
    ("bitfield",               BITFIELD);
    ("case",                   CASE);
    ("catch",                  CATCH);
    ("do",                     DO);
    ("downto",                 DOWNTO);
    ("else",                   ELSE);
    ("elsif",                  ELSIF);
    ("end",                    END);
    ("endcase",                ENDCASE);
    ("endfor",                 ENDFOR);
    ("endif",                  ENDIF);
    ("endmodule",              ENDMODULE);
    ("endtry",                 ENDTRY);
    ("endwhile",               ENDWHILE);
    ("ensures",                ENSURES);
    ("enumeration",            ENUMERATION);
    ("exception",              EXCEPTION);
    ("export",                 EXPORT);
    ("file",                   FILE);
    ("for",                    FOR);
    ("foreign",                FOREIGN);
    ("function",               FUNCTION);
    ("if",                     IF);
    ("implicit",               IMPLICIT);
    ("import",                 IMPORT);
    ("impure",                 IMPURE);
    ("in",                     IN);
    ("interface",              INTERFACE);
    ("let",                    LET);
    ("module",                 MODULE);
    ("not",                    NOT);
    ("of",                     OF);
    ("optimize",               OPTIMIZE);
    ("or",                     OR);
    ("ordered",                ORDERED);
    ("others",                 OTHERS);
    ("otherwise",              OTHERWISE);
    ("pure",                   PURE);
    ("record",                 RECORD);
    ("repeat",                 REPEAT);
    ("requires",               REQUIRES);
    ("return",                 RETURN);
    ("then",                   THEN);
    ("throw",                  THROW);
    ("to",                     TO);
    ("try",                    TRY);
    ("type",                   TYPE);
    ("unordered",              UNORDERED);
    ("until",                  UNTIL);
    ("use",                    USE);
    ("var",                    VAR);
    ("when",                   WHEN);
    ("while",                  WHILE);
    ("with",                   WITH);
    ("xor",                    XOR);
]

let prev_else_token_pos = ref (-1)

let update_location lexbuf opt_file line =
    let pos = lexbuf.Lexing.lex_curr_p in
    let new_file = match opt_file with
                   | None -> pos.Lexing.pos_fname
                   | Some f -> f
    in
    lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_fname = new_file;
        Lexing.pos_lnum = line;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule token = parse
    (* whitespace and comments *)
    | "```"                       { fenced_code_block lexbuf }
    | ['\n']                      { Lexing.new_line lexbuf; token lexbuf }
    | [' ' '\t']                  { token lexbuf }
    | "//" [^'\n']*               { token lexbuf }
    | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']* ('\"' ([^ '\"']* as name) '\"')? [' ' '\t']* '\n'
                                  { update_location lexbuf name (int_of_string num); token lexbuf }
    | "/*"                        { comment 1 lexbuf }

    (* numbers, strings and identifiers *)
    | '"'                         { let startpos = Lexing.lexeme_start_p lexbuf in
                                    let buffer   = Buffer.create 10 in
                                    ignore (string buffer lexbuf);
                                    lexbuf.lex_start_p <- startpos;
                                    STRINGLIT(Buffer.contents buffer) }

    | "0b" (['0' '1' '_']* as bits)
      { let x = Utils.drop_chars bits '_' in
        BITSLIT(Primops.mkBits (String.length x) (Z.of_string_base 2 x)) }
    | "0b" (['0' '1' 'x' '_']* as bits)
      { let x = Utils.drop_chars bits '_' in MASKLIT(x) }
    | (['0'-'9']+ as len) '\'' 'b' (['0'-'1' '_']+ as bits) (* deprecated *)
      { let x = Utils.drop_chars bits '_' in
        BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 2 x)) }
    | (['0'-'9']+ as len) '\'' 'd' (['0'-'9' '_']+ as digits) (* deprecated *)
      { BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 10 digits)) }
    | (['0'-'9']+ as len) '\'' 'x' (['0'-'9' 'A'-'F' 'a'-'f' '_']+ as nibbles) (* deprecated *)
      { BITSLIT(Primops.mkBits (int_of_string len) (Z.of_string_base 16 nibbles)) }
    | '0''x'(['0'-'9' 'A'-'F' 'a'-'f' '_']+ as nibbles) { INTLIT(None, Z.of_string_base 16 nibbles) } (* todo: change to BITLIT *)
    | 'i' (['0'-'9']+ as len) '\'' 'b' (['0'-'1']+ as bits) { INTLIT(Some (int_of_string len), Z.of_string_base 2 bits) }
    | 'i' (['0'-'9']+ as len) '\'' 'd' (['0'-'9']+ as digits) { INTLIT(Some (int_of_string len), Z.of_string digits) }
    | 'i' (['0'-'9']+ as len) '\'' 'x' (['0'-'9' 'A'-'F' 'a'-'f']+ as nibbles) { INTLIT(Some (int_of_string len), Z.of_string_base 16 nibbles) }
    | '-' 'i' (['0'-'9']+ as len) '\'' 'b' (['0'-'1']+ as bits) { INTLIT(Some (int_of_string len), Z.neg (Z.of_string_base 2 bits)) }
    | '-' 'i' (['0'-'9']+ as len) '\'' 'd' (['0'-'9']+ as digits) { INTLIT(Some (int_of_string len), Z.neg (Z.of_string digits)) }
    | '-' 'i' (['0'-'9']+ as len) '\'' 'x' (['0'-'9' 'A'-'F' 'a'-'f']+ as nibbles) { INTLIT(Some (int_of_string len), Z.neg (Z.of_string_base 16 nibbles)) }
    | ['0'-'9'] ['0'-'9' '_']* as digits { INTLIT(None, Z.of_string digits) }
    | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {
         let tok =
           ( match List.assoc_opt lxm keywords with
           | Some x -> x
           | None   -> ID(lxm)
           )
         in
         (* If the code accidentally uses "else if" instead of "elsif",
          * then parsing will eventually fail with a report about a missing
          * "end". But that error report can come much later (e.g., at the end
          * of the function and it is hard to find the actual cause of the error.
          *
          * So, the following code performs a heuristic check for "else" followed
          * by "if" in the same line - in the hope of catching most places that
          * this can go wrong.
          *)
         let follows_within x y dist = x < y && (y-x) < dist
         in
         if tok = IF && follows_within (!prev_else_token_pos) (Lexing.lexeme_start lexbuf) 2 then begin
             let pos = Lexing.lexeme_start_p lexbuf in
             Printf.printf "Warning: 'else if' should possibly be written as 'elsif' at %s:%d\n%!"
               pos.pos_fname pos.pos_lnum
         end;
         if tok = ELSE then prev_else_token_pos := Lexing.lexeme_end lexbuf;
         tok
    }

    (* delimiters *)
    | "!"  { BANG        }
    | "!=" { BANG_EQ     }
    | "%"  { PERCENT     }
    | "("  { LPAREN      }
    | ")"  { RPAREN      }
    | "*"  { STAR        }
    | "**" { STAR_STAR   }
    | "*:" { STAR_COLON  }
    | "+"  { PLUS        }
    | "++" { PLUS_PLUS   }
    | "+:" { PLUS_COLON  }
    | ","  { COMMA       }
    | "-"  { MINUS       }
    | "-:" { MINUS_COLON }
    | "->" { MINUS_GT    }
    | "."  { DOT         }
    | ".." { DOT_DOT     }
    | "/"  { SLASH       }
    | ":"  { COLON       }
    | "::" { COLON_COLON }
    | ":=" { COLON_EQ    }
    | ";"  { SEMICOLON   }
    | "<"  { LT          }
    | "<<" { LT_LT       }
    | "<=" { LT_EQ       }
    | "="  { EQ          }
    | "==" { EQ_EQ       }
    | "=>" { EQ_GT       }
    | ">"  { GT          }
    | ">=" { GT_EQ       }
    | ">>" { GT_GT       }
    | "?"  { QUERY       }
    | "["  { LBRACK      }
    | "]"  { RBRACK      }
    | "{"  { LBRACE      }
    | "}"  { RBRACE      }
    | eof  { EOF         }
    | _ as c { Printf.printf "%s:%d Unrecognized character '%c'\n"
                   lexbuf.lex_curr_p.pos_fname
                   lexbuf.lex_curr_p.pos_lnum
                   c;
               exit 0 }

and comment depth = parse
  | "/*"     { comment (depth+1) lexbuf }
  | "*/"     { if depth = 1 then token lexbuf else comment (depth-1) lexbuf }
  | "\n"     { Lexing.new_line lexbuf; comment depth lexbuf }
  | _        { comment depth lexbuf }

(* Fenced code blocks (i.e., characters marked by ``` in column 0 at start
 * and end are treated as comments.
 * This makes it easy to include metadata in ISA files.
 *)
and fenced_code_block = parse
  | "\n```"  { Lexing.new_line lexbuf; token lexbuf }
  | "\n"     { Lexing.new_line lexbuf; fenced_code_block lexbuf }
  | _        { fenced_code_block lexbuf }

and string b = parse
  | "\\n"    { Buffer.add_char b '\n'; string b lexbuf }
  | "\\t"    { Buffer.add_char b '\t'; string b lexbuf }
  | "\\\\"   { Buffer.add_char b '\\'; string b lexbuf }
  | '\\' '"' { Buffer.add_char b '"'; string b lexbuf }
  | '"'      { () }
  | _ as c   { Buffer.add_char b c; string b lexbuf }

(****************************************************************
 * End
 ****************************************************************)
