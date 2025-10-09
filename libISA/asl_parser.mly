(****************************************************************
 * ASL grammar file
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

%{
open Isa_ast
open Loc
module Isa_utils = Isa_utils
%}

%token <Primops.bitvector> BITSLIT
%token <int option * Primops.bigint> INTLIT
%token <string> ID
%token <string> MASKLIT
%token <string> STRINGLIT

%token AND                                 "and"
%token ARRAY                               "array"
%token AS                                  "as"
%token ASSERT                              "assert"
%token BANG                                "!"
%token BANG_EQ                             "!="
%token BEGIN                               "begin"
%token BITFIELD                            "bitfield"
%token CASE                                "case"
%token CATCH                               "catch"
%token COLON                               ":"
%token COLON_COLON                         "::"
%token COLON_EQ                            ":="
%token COMMA                               ","
%token CONFIG                              "config"
%token CONSTANT                            "constant"
%token DO                                  "do"
%token DOT                                 "."
%token DOT_DOT                             ".."
%token DOWNTO                              "downto"
%token ELSE                                "else"
%token ELSIF                               "elsif"
%token END                                 "end"
%token ENDCASE                             "endcase"
%token ENDFOR                              "endfor"
%token ENDIF                               "endif"
%token ENDTRY                              "endtry"
%token ENDWHILE                            "endwhile"
%token ENUMERATION                         "enumeration"
%token EOF
%token EQ                                  "="
%token EQ_EQ                               "=="
%token EQ_GT                               "=>"
%token EXCEPTION                           "Exception"
%token FOR                                 "for"
%token FUNCTION                            "function"
%token GT                                  ">"
%token GT_EQ                               ">="
%token IF                                  "if"
%token IMPLICIT                            "implicit"
%token IN                                  "in"
%token LBRACE                              "{"
%token LBRACK                              "["
%token LET                                 "let"
%token LPAREN                              "("
%token LT                                  "<"
%token LT_EQ                               "<="
%token MINUS                               "-"
%token MINUS_COLON                         "-:"
%token MINUS_GT                            "->"
%token NOT                                 "not"
%token OF                                  "of"
%token OPTIMIZE                            "optimize"
%token OR                                  "or"
%token OTHERWISE                           "otherwise"
%token PERCENT                             "%"
%token PLUS                                "+"
%token PLUS_COLON                          "+:"
%token QUERY                               "?"
%token RBRACE                              "}"
%token RBRACK                              "]"
%token RECORD                              "record"
%token REPEAT                              "repeat"
%token RETURN                              "return"
%token RPAREN                              ")"
%token SEMICOLON                           ";"
%token SLASH                               "/"
%token STAR                                "*"
%token STAR_COLON                          "*:"
%token STAR_STAR                           "**"
%token THEN                                "then"
%token THROW                               "throw"
%token TO                                  "to"
%token TRY                                 "try"
%token TYPE                                "type"
%token UNDERSCORE                          "_"
%token UNDERSCORE_UNDERSCORE_ASSERT        "__assert"
%token UNDERSCORE_UNDERSCORE_BUILTIN       "__builtin"
%token UNDERSCORE_UNDERSCORE_IN            "__in"
%token UNDERSCORE_UNDERSCORE_LET           "__let"
%token UNDERSCORE_UNDERSCORE_OPERATOR_ONE  "__operator1"
%token UNDERSCORE_UNDERSCORE_OPERATOR_TWO  "__operator2"
%token UNSPECIFIED                         "UNSPECIFIED"
%token UNTIL                               "until"
%token USE                                 "use"
%token VAR                                 "var"
%token WHEN                                "when"
%token WHERE                               "where"
%token WHILE                               "while"
%token WITH                                "with"
%token XOR                                 "xor"

%start <Isa_ast.declaration list> declarations_start
%start <Isa_ast.expr> expr_command_start
%start <Isa_ast.stmt> stmt_command_start
%start <Isa_ast.stmt list> stmts_command_start
%start <Isa_ast.config_command> config_command_start

(* deprecated tokens *)
%token AMPERSAND_AMPERSAND    "&&"
%token BAR_BAR                "||"
%token DIV                    "div"
%token DIVRM                  "divrm"
%token GETTER                 "getter"
%token GT_GT                  ">>"
%token LT_LT                  "<<"
%token MOD                    "mod"
%token MINUS_MINUS_GT         "-->"
%token PLUS_PLUS              "++"
%token QUOT                   "quot"
%token REM                    "rem"
%token SETTER                 "setter"


%%

declarations_start:
| declarations = declarations EOF { declarations }

expr_command_start:
| expr_command = expr_command EOF { expr_command }

stmt_command_start:
| stmt_command = stmt_command EOF { stmt_command }

stmts_command_start:
| stmts_command = stmts_command EOF { stmts_command }

config_command_start:
| config_command = config_command EOF { config_command }

pos:
| { $symbolstartpos }

let one_of(X, Y) :=
    | X
    | Y

ident:
| id = ID { Ident.mk_ident id }

path:
| p = separated_nonempty_list("::", ID) { Ident.mk_ident (String.concat "::" p) }

declarations:
| declaration0 = list(declaration) { declaration0 }

declaration:
| d = type_declaration { d }
| d = variable_declaration { d }
| d = function_declaration { d }
| d = function_instantiation { d }
| d = getter_declaration { d } (* deprecated *)
| d = setter_declaration { d } (* deprecated *)
| d = internal_definition { d }
| d = module_definition { d }

type_declaration:
| "__builtin" "type" v = path ";"
    { Decl_BuiltinType(v, Range($symbolstartpos, $endpos)) }
| "type" v = path ";"
    { Decl_Forward(v, Range($symbolstartpos, $endpos)) }
| "record" v = path ps = ty_params_opt "=" "{" fs = separated_nonempty_list(",", field_asl1) "}" ";"
    { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
| "Exception" v = path ";"
    { Decl_Exception(v, [], Range($symbolstartpos, $endpos)) }
| "Exception" v = path "=" "{" fs = separated_nonempty_list(",", field_asl1) "}" ";"
    { Decl_Exception(v, fs, Range($symbolstartpos, $endpos)) }
| "type" v = path ps = ty_params_opt "=" ty = ty ";"
    { Decl_Typedef(v, ps, ty, Range($symbolstartpos, $endpos)) }
| "bitfield" v = path "=" "{" fs = regfields "}" ":" path "(" wd = expr ")" ";"
    { Decl_Typedef(v, [], Type_Bits(wd, fs), Range($symbolstartpos, $endpos)) } (* todo: second path is expected to be Bits - but is ignored *)
| "enumeration" v = path "=" "{" es = separated_list(",", ident) "}" ";"
    { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }

(* deprecated *)
| "type" v = path "=" "bitfield" "(" fs = regfields ")" ":" path "(" wd = expr ")" ";"
    { Decl_Typedef(v, [], Type_Bits(wd, fs), Range($symbolstartpos, $endpos)) } (* todo: second path is expected to be Bits - but is ignored *)
| RECORD v = path ps = ty_params_opt "{" fs = nonempty_list(field) "}" ";"
    { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
| "type" v = path ps = ty_params_opt OF RECORD "{" fs = separated_nonempty_list(",", field_asl1) "}" ";"
    { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
| "type" v = path OF "Exception" ";"
    { Decl_Exception(v, [], Range($symbolstartpos, $endpos)) }
| "type" v = path OF "Exception" "{" fs = separated_nonempty_list(",", field_asl1) "}" ";"
    { Decl_Exception(v, fs, Range($symbolstartpos, $endpos)) }
| "type" v = path ps = ty_params_opt OF ty = ty ";"
    { Decl_Typedef(v, ps, ty, Range($symbolstartpos, $endpos)) }
| "enumeration" v = path "{" es = separated_list(",", path) "}" ";"
    { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }

%inline ty_params_opt:
| "(" ps = separated_nonempty_list(",", ident) ")" { ps }
| { [] }

(* deprecated *)
field:
| ident = ident ":" ty = ty ";" { (ident, ty) }

(* To provide a period of backwards compatibility, we support fields that
 * (1) end with a semicolon (deprecated),
 * (2) do not end with a semicolon but separated in a list by a colon.
 *)
field_asl1:
| ident = ident ":" ty = ty { (ident, ty) }

variable_declaration:
| "var" v = path ":" ty = ty ";"
    { Decl_Var(v, ty, Range($symbolstartpos, $endpos)) }
| "let" v = path ty = ty_opt assign e = expr ";"
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }
(* deprecated *)
| "constant" v = path ty = ty_opt assign e = expr ";"
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }

ixtype:
| "type" ident = ident { Index_Enum(ident) }
| expr = expr { Index_Int(expr) }

throws:
| "!" { AlwaysThrow }
| "?" { MayThrow }
| { NoThrow }

function_declaration:
| hdr = function_header ";"
    { Decl_FunType(fst hdr, snd hdr, Range($symbolstartpos, $endpos)) }
| hdr = function_header "begin" b = block "end"
    { Decl_FunDefn(fst hdr, snd hdr, b, Range($symbolstartpos, $endpos)) }

function_instantiation:
| "optimize" "function" f = path "with" "{" ps = separated_nonempty_list(",", parameter_value) "}" ";"
    { Decl_FunInstance(f, ps, Range($symbolstartpos, $endpos)) }

let parameter_value :=
    | p = ident ; "=>" ; v = literal_expression2 ; { (p, Some v) }
    | p = ident ; "=>" ; "_"                     ; { (p, None) }

let literal_expression2 :=
    | i = INTLIT ; {
        ( match Value.from_intLit i with
        | Some v -> v
        | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
        )
      }
    | b = BITSLIT   ; { Value.VBits b }
    | s = STRINGLIT ; { Value.from_stringLit s }

%inline builtin:
    | "__builtin" { true }
    |             { false }

function_header:
(* Read form *)
| b = builtin "function" f = path throws=throws formals = formals_opt rty=returntype_opt
    { let (ps, args, is_getter_setter, array_syntax) = formals in
      let fty = { parameters=ps; args; setter_arg=None; rty=rty; use_array_syntax=array_syntax; is_getter_setter=is_getter_setter; throws; is_builtin=b } in
      (f, fty) }
(* Write form *)
| b = builtin "function" f = path throws=throws formals = formals_opt ":=" value=ident ":" value_ty=ty
    { let (ps, args, is_getter_setter, array_syntax) = formals in
      let fty = { parameters=ps; args; setter_arg=Some(value,value_ty); rty=Isa_utils.type_unit; use_array_syntax=array_syntax; is_getter_setter=true; throws; is_builtin=b } in
      (f, fty) }

parameters_opt:
| "{" pars = parameter_list "}" { pars }
| { [] }

parameter_list:
| pars = separated_nonempty_list(",", parameter) { pars }

parameter:
| par = ident ty = ty_opt { (par, ty) }

(* note: use of parameters_opt is deprecated *)
formals_opt:
| ps = parameters_opt "(" args = formals_with_default ")" { (ps @ fst args, snd args, false, false) }
| ps = parameters_opt "[" args = formals_with_default "]" { (ps @ fst args, snd args, true, true) }
| ps = parameters_opt { (ps, [], true, false) }

returntype_opt:
| funtype ty = ty { ty }
| { Isa_utils.type_unit }

ty_opt:
| ":" ty = ty { Some ty }
| { None }

formals_with_default:
| formals = separated_list(",", formal_with_default)
  { let ps = List.filter_map (fun (x, (v,ty,_)) -> if x then Some (v, Some ty) else None) formals in
    let args = List.filter_map (fun (x, y) -> if x then None else Some y) formals in
    (ps, args)
  }

formal_with_default:
| imp = option("implicit") v = ident ":" ty = ty                 { (Option.is_some imp, (v, ty, None)) }
| imp = option("implicit") v = ident ":" ty = ty assign d = expr { (Option.is_some imp, (v, ty, Some d)) }

formal_list:
| formals = separated_list(",", formal) { formals }

formal:
| ident = ident ":" ty = ty { (ident, ty, None) }

(* deprecated *)
getter_declaration:
| "getter" f = ident throws=throws ps = parameters_opt funtype ty = ty ";"
    { let fty = { parameters=ps; args=[]; setter_arg=None; rty=ty; use_array_syntax=false; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| "getter" f = ident throws=throws ps = parameters_opt funtype ty = ty "begin" b = block "end"
    { let fty = { parameters=ps; args=[]; setter_arg=None; rty=ty; use_array_syntax=false; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }
| "getter" f = ident throws=throws ps = parameters_opt "[" args = formal_list "]" funtype ty = ty ";"
    { let fty = { parameters=ps; args; setter_arg=None; rty=ty; use_array_syntax=true; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| "getter" f = ident throws=throws ps = parameters_opt "[" args = formal_list "]" funtype ty = ty "begin" b = block "end"
    { let fty = { parameters=ps; args; setter_arg=None; rty=ty; use_array_syntax=true; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

(* deprecated *)
setter_declaration:
| "setter" f = ident throws=throws ps = parameters_opt assign v = ident ":" ty = ty ";"
    { let fty = { parameters=ps; args=[]; setter_arg=Some (v, ty); rty=Isa_utils.type_unit; use_array_syntax=false; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| "setter" f = ident throws=throws ps = parameters_opt assign v = ident ":" ty = ty "begin" b = block "end"
    { let fty = { parameters=ps; args=[]; setter_arg=Some (v, ty); rty=Isa_utils.type_unit; use_array_syntax=false; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }
| "setter" f = ident throws=throws ps = parameters_opt "[" args = formal_list "]" assign v = ident ":" ty = ty ";"
    { let fty = { parameters=ps; args; setter_arg=Some (v, ty); rty=Isa_utils.type_unit; use_array_syntax=true; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| "setter" f = ident throws=throws ps = parameters_opt "[" args = formal_list "]" assign v = ident ":" ty = ty "begin" b = block "end"
    { let fty = { parameters=ps; args; setter_arg=Some (v, ty); rty=Isa_utils.type_unit; use_array_syntax=true; is_getter_setter=true; throws; is_builtin=false; } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

internal_definition:
| "__operator1" op = unop "=" vs = separated_nonempty_list(",", path) ";"
    { Decl_Operator1(op, vs, Range($symbolstartpos, $endpos)) }
| "__operator2" op = binop "=" vs = separated_nonempty_list(",", path) ";"
    { Decl_Operator2(op, vs, Range($symbolstartpos, $endpos)) }
| "config" v = path ":" ty = ty assign e = expr ";"
    { Decl_Config(v, ty, e, Range($symbolstartpos, $endpos)) }

module_definition:
| "use" p1 = path "as" p2 = path ";" { Decl_Use(p1, p2, Range($symbolstartpos, $endpos)) }

ty:
| tc = path
    { if Ident.equal tc Builtin_idents.integer_ident
      then Type_Integer(None)
      else if Ident.equal tc Builtin_idents.old_integer_ident
      then Type_Integer(None)
      else Type_Constructor(tc, []) }
| "{" ranges = separated_nonempty_list(",", set_range) "}"
    { Type_Integer(Some(ranges)) }
| tc = path "(" es = separated_nonempty_list(",", expr) ")"
    { if Ident.equal tc Builtin_idents.bits_ident && List.length es == 1
      then Type_Bits(List.hd es, [])
      else if Ident.equal tc Builtin_idents.old_bits_ident && List.length es == 1
      then Type_Bits(List.hd es, [])
      else Type_Constructor(tc, es) }
| tc = path "(" es = separated_nonempty_list(",", expr) ")"
            "{" fs = regfields_old "}" (* deprecated *)
    { if Ident.equal tc Builtin_idents.bits_ident && List.length es == 1
      then Type_Bits(List.hd es, fs)
      else if Ident.equal tc Builtin_idents.old_bits_ident && List.length es == 1
      then Type_Bits(List.hd es, fs)
      else Type_Constructor(tc, es) (* Note: set is discarded - this would be a problem if not deprecated *)
    }
| "array" "[" ixtype = ixtype "]" OF ty = ty
    { Type_Array(ixtype, ty) }
| "(" tys = separated_list(",", ty) ")"
    { Type_Tuple(tys) }
| tc = path "{" ranges = separated_nonempty_list(",", set_range) "}" (* deprecated *)
    { if Ident.equal tc Builtin_idents.integer_ident
      then Type_Integer(Some(ranges))
      else if Ident.equal tc Builtin_idents.old_integer_ident
      then Type_Integer(Some(ranges))
      else Type_Constructor(tc, []) (* Note: set is discarded - this would be a problem if not deprecated *)
    }

set_range:
| c = expr { Set_Single(c) }
| c1 = expr ".." c2 = expr { Set_Range(Some c1, Some c2) }
| c1 = expr ".."           { Set_Range(Some c1, None) }
|           ".." c2 = expr { Set_Range(None, Some c2) }

(* deprecated *)
regfields_old:
| fs = list(regfield_old) { fs }
| f = regfield_old "," fs = regfields_old { f :: fs }

(* deprecated *)
regfield_old:
| "[" slices = separated_nonempty_list(",", slice) "]" ident = ident
    { (slices, ident) }

regfields:
| fs = list(regfield) { fs }
| f = regfield "," fs = regfields { f :: fs }

regfield:
| ident = ident "=>" "[" slices = separated_nonempty_list(",", slice) "]"
    { (slices, ident) }

stmt:
| simple_stmt = simple_stmt { simple_stmt }
| compound_stmt = compound_stmt { compound_stmt }

compound_stmt:
| conditional_stmt = conditional_stmt { conditional_stmt }
| repetitive_stmt = repetitive_stmt { repetitive_stmt }
| catch_stmt = catch_stmt { catch_stmt }
| "begin" block = block "end" opt_semi { Stmt_Block(block, Range($symbolstartpos, $endpos)) }

block:
| stmts = list(stmt) { stmts }

(* todo: this is just a stepping stone to making semicolon into a separator *)
opt_semi:
| ";" {}
| {}

assign:
| ":=" {}
| "=" {} (* deprecated *)

mapsto:
| "=>" {}
| "="  {} (* deprecated *)

%inline funtype:
| "->" {()}
| "=>" {()}(* deprecated *)

assignment_stmt:
| VAR v = ident "," vs = separated_list(",", ident) ":" ty = ty ";"
    { Stmt_VarDeclsNoInit(v :: vs, ty, Range($symbolstartpos, $endpos)) }
| VAR v = ident ":" ty = ty ";"
    { Stmt_VarDeclsNoInit([v], ty, Range($symbolstartpos, $endpos)) }
| VAR dis = decl_item assign i = expr ";"
    { Stmt_VarDecl(false, dis, i, Range($symbolstartpos, $endpos)) }
| LET dis = decl_item assign i = expr ";"
    { Stmt_VarDecl(true, dis, i, Range($symbolstartpos, $endpos)) }
| l = lexpr assign r = expr ";"
    { Stmt_Assign(l, r, Range($symbolstartpos, $endpos)) }

(* deprecated *)
| CONSTANT dis = decl_item assign i = expr ";"
    { Stmt_VarDecl(true, dis, i, Range($symbolstartpos, $endpos)) }

decl_item:
| v = ident  oty = ty_opt
    { DeclItem_Var(v, oty) }
| "(" dis = separated_nonempty_list(",", decl_item) ")"
    { DeclItem_Tuple(dis) }
| "[" dis = separated_nonempty_list(",", decl_bit) "]"
    { DeclItem_BitTuple(dis) }
| wildcard oty = ty_opt
    { DeclItem_Wildcard(oty) }

wildcard:
| "-" {} (* deprecated *)
| "_" {}

decl_bit:
| v = ident oty = ty_opt { (Some v, Option.value oty ~default:(Type_Bits (Isa_utils.one, []))) }
| wildcard  oty = ty_opt { (None,   Option.value oty ~default:(Type_Bits (Isa_utils.one, []))) }

lexpr:
| wildcard
    { LExpr_Wildcard }
| v = path
    { LExpr_Var(v) }
| e = lexpr "." f = ident
    { LExpr_Field(e, f) }
| e = lexpr "." "[" fs = separated_nonempty_list(",", ident) "]"
    { LExpr_Fields(e, fs) }
| e = lexpr "[" ss = separated_list(",", slice) "]"
    { LExpr_Slices(Isa_utils.type_unknown, e, ss) }
| "[" es = separated_nonempty2_list(",", lexpr) "]"
    { LExpr_BitTuple([], es) }
| "(" es = separated_nonempty2_list(",", lexpr) ")"
    { LExpr_Tuple(es) }
| "(" e = lexpr ")"
    { e }

simple_stmt:
| assignment_stmt = assignment_stmt
    { assignment_stmt }
| f = path throws1=throws "(" args = separated_list(",", arg) ")" throws2=throws ";"
    { Stmt_UCall(f, args, (if throws1<>NoThrow then throws1 else throws2), Range($symbolstartpos, $endpos)) }
| "return" e = expr ";"
    { Stmt_Return(e, Range($symbolstartpos, $endpos)) }
| "return" ";"
    { Stmt_Return(Expr_Tuple([]), Range($symbolstartpos, $endpos)) }
| "assert" e = expr ";"
    { Stmt_Assert(e, Range($symbolstartpos, $endpos)) }
| "throw" e = expr ";"
    { Stmt_Throw(e, Range($symbolstartpos, $endpos)) }

conditional_stmt:
| "if" c = expr "then" t = block els = list(s_elsif) f = optional_else one_of("end","endif") opt_semi
    { let loc = Range($symbolstartpos, $endpos(t)) in
      Stmt_If((c, t, loc)::els, f, Range($symbolstartpos, $endpos)) }
| "case" e = expr "of" alts = nonempty_list(alt) ob = opt_otherwise one_of("end","endcase") opt_semi
    { Stmt_Case(e, None, alts, ob, Range($symbolstartpos, $endpos)) }

s_elsif:
| "elsif" c = expr "then" b = block
    { (c, b, Range($symbolstartpos, $endpos)) }

optional_else:
| "else" b = block { (b, Range($symbolstartpos, $endpos)) }
|                  { ([], Range($symbolstartpos, $endpos)) }

alt:
| "when" ps = separated_nonempty_list(",", pattern) oalt = opt_altcond "=>" b = block
    { Alt_Alt(ps, oalt, b, Range($symbolstartpos, $endpos)) }

opt_otherwise:
| "otherwise" "=>" b = block { Some(b, Range($symbolstartpos, $endpos)) }
| { None }

opt_altcond:
| "where" e = expr { Some(e) } (* deprecated *)
| "if" e = expr { Some(e) }
| { None }

pattern:
| i = INTLIT {
    ( match Value.from_intLit i with
    | Some v -> Pat_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| "-" i = INTLIT {
    ( match Value.from_intLit (Value.negate_intLit i) with
    | Some v -> Pat_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| b = BITSLIT { Pat_Lit (Value.VBits b) }
| m = MASKLIT { Pat_Lit (Value.from_maskLit m) }
| c = path { Pat_Const(c) }
| wildcard { Pat_Wildcard }
| "(" ps = separated_nonempty2_list(",", pattern) ")" { Pat_Tuple(ps) }
| "{" aps = separated_list(",", apattern) "}" { Pat_Set(aps) }

apattern:
| p1 = expr ".." p2 = expr { Pat_Range(p1, p2) }
| p = expr { Pat_Single(p) }
| m = MASKLIT { Pat_Lit (Value.from_maskLit m) }

repetitive_stmt:
| "for" v = ident ty_opt = ty_opt assign f = expr dir = direction t = expr "do" b = block one_of("end","endfor") opt_semi
    { Stmt_For(v, Option.value ty_opt ~default:(Type_Integer(None)), f, dir, t, b, Range($symbolstartpos, $endpos)) }
| "while" c = expr "do" b = block one_of("end","endwhile") opt_semi
    { Stmt_While(c, b, Range($symbolstartpos, $endpos)) }
| "repeat" b = block "until" c = expr ";" pos = pos
    { Stmt_Repeat(b, c, pos, Range($symbolstartpos, $endpos)) }

direction:
| "to" { Direction_Up }
| "downto" { Direction_Down }

catch_stmt:
| "try" b = block "catch" pos = pos cs = list(catcher) ob = opt_otherwise one_of("end","endtry") opt_semi
    { Stmt_Try(b, pos, cs, ob, Range($symbolstartpos, $endpos)) }

catcher:
| "when" v = ident ":" tc=ident "=>" b = block
    { Catcher_Guarded(v, tc, b, Range($symbolstartpos, $endpos)) }

expr:
| ce = conditional_expression { ce }

conditional_expression:
| "if" c = cexpr "then" t = expr els = list(e_elsif) "else" e = expr
    { Expr_If((c, t)::els, e) }
| "__let" v = ident ":" ty = ty assign e = expr "__in" b = expr
    { Expr_Let(v, ty, e, b) }
| "__assert" e1 = expr "__in" e2 = expr
    { Expr_Assert(e1, e2, Range($symbolstartpos, $endpos)) }
| cexpr = cexpr { cexpr }

e_elsif:
| "elsif" c = expr "then" e = expr { (c, e) }

cexpr:
| bexpr = bexpr fs = list(factor)
    { buildExpression bexpr fs (Range($startpos(bexpr), $endpos(fs))) }

factor:
| op = binop e = bexpr { Factor_BinOp(op, e) }

binop:
| "=="         { Binop_Eq }
| "!="         { Binop_NtEq }
| ">"          { Binop_Gt }
| ">="         { Binop_GtEq }
| "<"          { Binop_Lt }
| "<="         { Binop_LtEq }
| "+"          { Binop_Plus }
| "-"          { Binop_Minus }
| "*"          { Binop_Multiply }
| "/"          { Binop_Divide }
| "%"          { Binop_Remainder }
| "**"         { Binop_Power }
| "<<"         { Binop_ShiftL }
| ">>"         { Binop_ShiftR }
| "and"        { Binop_And }
| "or"         { Binop_Or }
| "xor"        { Binop_Xor }
| "and" "then" { Binop_AndThen }
| "or" "else"  { Binop_OrElse }

(* deprecated *)
| "&&"         { Binop_AndThen }
| "||"         { Binop_OrElse }
| "++"         { Binop_Append }
| "quot"       { Binop_Quot }
| "rem"        { Binop_Rem }
| "div"        { Binop_Div_exact }
| "divrm"      { Binop_Divrm }
| "mod"        { Binop_Mod }
| "-->"        { Binop_BoolImplies }

bexpr:
| op = unop e = fexpr { Expr_Unop(op, e) }
| e = fexpr { e }

fexpr:
| e = fexpr "." f = ident
    { Expr_Field(e, f) }
| e = fexpr "." "[" fs = separated_nonempty_list(",", ident) "]"
    { Expr_Fields(e, fs) }
| e = fexpr "[" ss = separated_list(",", slice) "]"
    { Expr_Slices(Isa_utils.type_unknown, e, ss) }
| e = fexpr "in" p = pattern
    { Expr_In(e, p) }
| e = fexpr "not" "in" p = pattern
    { Expr_Unop(Unop_Not, Expr_In(e, p)) }
| e = aexpr
    { e }

aexpr:
| literal_expression = literal_expression
    { literal_expression }
| v = path
    { Expr_Var(v) }
| f = path "(" es = separated_list(",", arg) ")" throws=throws
    { Expr_UApply(Expr_Var f, false, es, throws) }
| f = path "?" "(" es = separated_list(",", arg) ")"
    { Expr_UApply(Expr_Var f, false, es, MayThrow) }
| tc = path
    "(" es = separated_list(",", arg) ")"
    "{" fas = separated_nonempty_list(",", field_assignment) "}"
    { Expr_Record(tc, es, fas) }
| tc = path "{" fas = separated_nonempty_list(",", field_assignment) "}"
    { Expr_Record(tc, [], fas) }
| "array" "(" es = separated_nonempty_list(",", expr) ")"
    { Expr_ArrayInit(Isa_utils.type_unknown, es) }
| "(" fas = separated_nonempty2_list(",", field_assignment) ")" ":" ty=simple_type
    { Expr_Record(fst ty, snd ty, fas) }
| "(" e = expr ")"
    { e }
| "(" es = separated_nonempty2_list(",", expr) ")"
    { Expr_Tuple(es) }
| "[" es = separated_nonempty2_list(",", expr) "]"
    { Expr_Concat([], es) }
| "UNSPECIFIED" ":" t = ty
    { Expr_Unknown(t) }
| e = aexpr "with" "{" changes = separated_nonempty_list(",", change) "}"
    { Expr_WithChanges(Isa_utils.type_unknown, e, changes) }
| e = aexpr AS t = ty { Expr_AsType(e, t) } (* deprecated *)

(* records currently require a type constructor after them *)
simple_type:
    tc=path "(" ps=separated_nonempty_list(",", arg) ")" { (tc, ps) }

%inline arg:
| v = ident mapsto e = expr  { (Some v, e) }
| e = expr                   { (None, e) }

field_assignment:
| ident = ident mapsto expr = expr { (ident, expr) }

change:
| f = ident mapsto rhs = expr { (Change_Field f,   rhs) }
| "[" ss = separated_nonempty_list(",", slice) "]" mapsto rhs = expr { (Change_Slices ss, rhs) }

unop:
| "-"   { Unop_Negate }
| "not" { Unop_Not }
| "!"   { Unop_Not } (* deprecated *)

slice:
| e = expr                 { Slice_Single(e) }
| hi = expr ":" lo = expr  { Slice_HiLo(hi, lo) }
| lo = expr "+:" wd = expr { Slice_LoWd(lo, wd) }
| hi = expr "-:" wd = expr { Slice_HiWd(hi, wd) }
| lo = expr "*:" wd = expr { Slice_Element(lo, wd) }

literal_expression:
| i = INTLIT {
    ( match Value.from_intLit i with
    | Some v -> Expr_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| "-" i = INTLIT {
    ( match Value.from_intLit (Value.negate_intLit i) with
    | Some v -> Expr_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| b = BITSLIT   { Expr_Lit(Value.VBits b) }
| s = STRINGLIT { Expr_Lit(Value.from_stringLit s) }

expr_command:
| expr = expr { expr }

stmt_command:
| stmt = stmt { stmt }

stmts_command:
| stmts = list(stmt) { stmts }

config_command:
| v = path "=" e = expr { CLI_Config(v, e) }

/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Paris-Rocquencourt                            */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2015 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the GNU Library General Public License, with the   */
/*  special exception on linking described in file LICENSE.               */
/*                                                                        */
/**************************************************************************/

/* nonempty2 variants of the menhir standard library lists, Peter Sewell, 2017-05 */

(* [separated_nonempty2_list(separator, X)] recognizes list of
   two or more [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public separated_nonempty2_list(separator, X):
  x1 = X; separator; x2 = X
    { [ x1; x2 ] }
| x = X; separator; xs = separated_nonempty2_list(separator, X)
    { x :: xs }

(****************************************************************
 * End
 ****************************************************************)
