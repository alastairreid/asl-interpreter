(****************************************************************
 * ISA grammar file
 *
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
%token ENDMODULE                           "endmodule"
%token ENSURES                             "ensures"
%token ENUMERATION                         "enumeration"
%token EOF
%token EQ                                  "="
%token EQ_EQ                               "=="
%token EQ_GT                               "=>"
%token EXCEPTION                           "exception"
%token EXPORT                              "export"
%token FILE                                "file"
%token FOR                                 "for"
%token FOREIGN                             "foreign"
%token FUNCTION                            "function"
%token GT                                  ">"
%token GT_GT                               ">>"
%token GT_EQ                               ">="
%token IF                                  "if"
%token IMPLICIT                            "implicit"
%token IMPORT                              "import"
%token IMPURE                              "impure"
%token IN                                  "in"
%token INTERFACE                           "interface"
%token LBRACE                              "{"
%token LBRACK                              "["
%token LET                                 "let"
%token LPAREN                              "("
%token LT                                  "<"
%token LT_LT                               "<<"
%token LT_EQ                               "<="
%token MINUS                               "-"
%token MINUS_COLON                         "-:"
%token MINUS_GT                            "->"
%token MODULE                              "module"
%token NOT                                 "not"
%token OF                                  "of"
%token OPTIMIZE                            "optimize"
%token OR                                  "or"
%token ORDERED                             "ordered"
%token OTHERWISE                           "otherwise"
%token OTHERS                              "others"
%token PERCENT                             "%"
%token PLUS                                "+"
%token PLUS_PLUS                           "++"
%token PLUS_COLON                          "+:"
%token PURE                                "pure"
%token QUERY                               "?"
%token RBRACE                              "}"
%token RBRACK                              "]"
%token RECORD                              "record"
%token REPEAT                              "repeat"
%token REQUIRES                            "requires"
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
%token UNORDERED                           "unordered"
%token UNSPECIFIED                         "UNSPECIFIED"
%token UNTIL                               "until"
%token USE                                 "use"
%token VAR                                 "var"
%token WHEN                                "when"
%token WHILE                               "while"
%token WITH                                "with"
%token XOR                                 "xor"

%start <Isa_ast.declaration list> declarations_file
%start <Isa_ast.expr> expr_command_start
%start <Isa_ast.stmt> stmt_command_start
%start <Isa_ast.stmt list> stmts_command_start

%%

let pos :=
    | { $symbolstartpos }

let one_of(X, Y) :=
    | X
    | Y

(****************************************************************
 * Identifiers and paths
 ****************************************************************)

let ident :=
    | id = ID ; { Ident.mk_ident id }

let path :=
    | p = separated_nonempty_list("::", ID) ; { Ident.mk_ident (String.concat "::" p) }

(****************************************************************
 * Expressions
 ****************************************************************)

(* expr is the top-level entry to expressions.
 * At this level, conditional expressions are allowed without parentheses
 *)
let expr :=
    | expression
    | conditional_expression
    | e = expression ; "with" ; "{" ; changes = separated_nonempty_list(",", change) ; "}" ;
      { Expr_WithChanges(Isa_utils.type_unknown, e, changes) }
    | "__let" ; v = ident ; ":" ; t = ty ; becomes ; e = expr ; "__in" ; b = expr ; { Expr_Let(v, t, e, b) }
    | "__assert" ; e1 = expr ; "__in" ; e2 = expr ; { Expr_Assert(e1, e2, Range($symbolstartpos, $endpos)) }

let expression :=
    | r = relation ; { r }
    | r = relation ; rs = expression_chain("and", relation) ; { mk_binop_chain Binop_And r rs }
    | r = relation ; rs = expression_chain("or", relation) ; { mk_binop_chain Binop_Or r rs }
    | r = relation ; rs = expression_chain("xor", relation) ; { mk_binop_chain Binop_Xor r rs }
    | r = relation ; rs = expression_chain("and"; "then", relation) ; { mk_binop_chain Binop_AndThen r rs }
    | r = relation ; rs = expression_chain("or"; "else", relation) ; { mk_binop_chain Binop_OrElse r rs }

let expression_chain(op, thing) :=
    | op ; e = thing; { [e] }
    | op ; e = thing ; es = expression_chain(op, thing) ; { e :: es }

let expression_chain2(op, thing) :=
    | o=op ; e = thing; { [(o,e)] }
    | o=op ; e = thing ; es = expression_chain2(op, thing) ; { (o,e) :: es }

let relation :=
    | e = simple_expression  ; { e }
    | e1 = simple_expression ; op = relational_operator ; e2 = simple_expression ; { Expr_Binop (e1, op, e2) }
    | e = simple_expression  ;         "in" ; p = atomic_pattern ; { Expr_In(e, p) }
    | e = simple_expression  ; "not" ; "in" ; p = atomic_pattern ; { Expr_Unop(Unop_Not, Expr_In(e, p)) }

let relational_operator :=
    | "==" ; { Binop_Eq }
    | "=" ;  { raise (Parse_error_locn (Range($symbolstartpos, $endpos), "Use '==' instead of '=' for comparisons.")) }
    | "!=" ; { Binop_NtEq }
    | ">"  ; { Binop_Gt }
    | ">=" ; { Binop_GtEq }
    | "<"  ; { Binop_Lt }
    | "<=" ; { Binop_LtEq }

let simple_expression :=
    | t = term1 ; { t }
    | t = term1 ; ts = expression_chain2(additive_op, term); { mk_binop_chain2 t ts }
    | t = term  ; ts = expression_chain("++", term); { mk_binop_chain Binop_Append t ts }
    | t1 = term ; "<<" ; t2 = primary ;              { Expr_Binop(t1, Binop_ShiftL, t2) }
    | t1 = term ; ">>" ; t2 = primary ;              { Expr_Binop(t1, Binop_ShiftR, t2) }

let additive_op :=
    | "+"; { Binop_Plus }
    | "-"; { Binop_Minus }

let term1 :=
    | "-" ; t = term ; { Expr_Unop(Unop_Negate, t) }
    | "+" ; t = term ; { t }
    |       t = term ; { t }

let term :=
    | factor
    | f = factor ; fs = expression_chain("*", factor); { mk_binop_chain Binop_Multiply f fs }
    | f = factor ; fs = expression_chain("/", factor); { mk_binop_chain Binop_Divide f fs }
    | f = factor ; fs = expression_chain("%", factor); { mk_binop_chain Binop_Remainder f fs }

let factor :=
    | primary
    | p1 = primary ; "**" ; p2 = primary ; { Expr_Binop(p1, Binop_Power, p2) }
    | "not" ; p = primary ;                { Expr_Unop(Unop_Not, p) }
    | "!"   ; p = primary ;                { Expr_Unop(Unop_Not, p) }
    | "UNSPECIFIED" ; ":" ; t = ty ;       { Expr_Unknown(t) }

let primary :=
    | p = selectable_primary ; sel = selectors ; { sel(p) }
    | p = path; "{" ; fas = separated_list(",", field_assignment); "}"; { Expr_Record(p, [], fas) }
    | aggregate
    | p = path; function_exception_mark;
      "(" ; ps = parameter_associations; ")";
      "{" ; fas = separated_list(",", field_assignment); "}";
      { Expr_Record(p, ps, fas) }

let selectable_primary ==
    | p = path; { Expr_Var(p) }
    | p = path; mark = function_exception_mark ; "("; ps = parameter_associations ; ")"; { Expr_UApply(Expr_Var(p), false, ps, mark) }
    | l = literal_expression ; { Expr_Lit l }
    | "(" ; e = expr ; ")" ; { e }

let selectors :=
    | { Fun.id }
    | sel1 = selector; sel2 = selectors; { fun e -> sel2(sel1(e)) }

let selector :=
    | "." ; f = ident ; { fun e -> Expr_Field(e, f) }
      (* note that the syntax "A[i]" is also used for array indexing and slicing *)
    | mark = function_exception_mark ; "["; ps = parameter_associations ; "]"; { fun e -> Expr_UApply(e, true, ps, mark) }

let function_exception_mark ==
    | "?"; { MayThrow }
    |      { NoThrow }

let parameter_associations :=
    | separated_list(",", parameter_association)

let parameter_association :=
    | v = ident ; "=>" ; e = slice ; { (Some v, Expr_Slice e) }
    |                    e = slice ; { (None, Expr_Slice e) }

let literal_expression :=
    | i = INTLIT ; {
        ( match Value.from_intLit i with
        | Some v -> v
        | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
        )
      }
    | b = BITSLIT   ; { Value.VBits b }
    | s = STRINGLIT ; { Value.from_stringLit s }

let aggregate :=
    (* Note that record aggregates are parsed as function calls *)
    | "(" ; es = separated_nonempty2_list(",", expr) ; ")" ; { Expr_Tuple(es) }
    | "array" ; "(" ; es = separated_nonempty_list(",", expr) ; ")" ; { Expr_ArrayInit(es) }

let field_assignment :=
    | f = ident ; "=>" ; e = expr ; { (f, e) }

let change :=
    | f = ident ;                                            "=>" ; rhs = expr ; { (Change_Field f, rhs) }
    | "[" ; ss = separated_nonempty_list(",", slice) ; "]" ; "=>" ; rhs = expr ; { (Change_Slices ss, rhs) }

let conditional_expression :=
    | "if" ; c = expr ; "then" ; t = expr ;
      els = list(e_elsif) ;
      "else" ; e = expr ; { Expr_If((c, t)::els, e) }

let e_elsif :=
    | "elsif" ; c = expr ; "then" ; e = expr ; { (c, e) }

let slice :=
    | e = expr ;                     { Slice_Single(e) }
    | hi = expr ; ":"  ; lo = expr ; { Slice_HiLo(hi, lo) }
    | lo = expr ; "+:" ; wd = expr ; { Slice_LoWd(lo, wd) }
    | hi = expr ; "-:" ; wd = expr ; { Slice_HiWd(hi, wd) }
    | lo = expr ; "*:" ; wd = expr ; { Slice_Element(lo, wd) }

(****************************************************************
 * Patterns
 ****************************************************************)

let pattern :=
    | atomic_pattern
    | "_" ;                          { Pat_Wildcard }
    | literal_pattern
    | c = path ;                     { Pat_Const(c) }

let atomic_pattern :=
    | m = MASKLIT ;                                             { Pat_Lit (Value.from_maskLit m) }
    | "{" ; ps = separated_list(",", set_pattern) ;       "}" ; { Pat_Set(ps) }
    | "(" ; ps = separated_nonempty2_list(",", pattern) ; ")" ; { Pat_Tuple(ps) }

let set_pattern :=
    | m = MASKLIT ;                  { Pat_Lit (Value.from_maskLit m) }
    | p1 = expr ; ".." ; p2 = expr ; { Pat_Range(p1, p2) }
    | p = expr ;                     { Pat_Single(p) }


let literal_pattern :=
    | b = BITSLIT ; { Pat_Lit (Value.VBits b) }
    | i = INTLIT ; {
        ( match Value.from_intLit i with
        | Some v -> Pat_Lit v
        | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
        )
      }
    | "-" ; i = INTLIT ; {
        ( match Value.from_intLit (Value.negate_intLit i) with
        | Some v -> Pat_Lit v
        | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
        )
      }

(****************************************************************
 * Types
 ****************************************************************)

let ty :=

    | "(" ; tys = separated_list(",", ty) ; ")" ; < Type_Tuple >
    | tc = path ;
        { if Ident.equal tc Builtin_idents.integer_ident
          then Type_Integer(None)
          else if Ident.equal tc Builtin_idents.old_integer_ident
          then Type_Integer(None)
          else Type_Constructor(tc, []) }
    | "{" ; ranges = separated_nonempty_list(",", set_range) ; "}" ; { Type_Integer(Some(ranges)) }
    | tc = path ; "(" ; es = separated_nonempty_list(",", expr) ; ")" ;
        { if Ident.equal tc Builtin_idents.bits_ident && List.length es == 1
          then Type_Bits(List.hd es, [])
          else if Ident.equal tc Builtin_idents.old_bits_ident && List.length es == 1
          then Type_Bits(List.hd es, [])
          else Type_Constructor(tc, es) }
    | "array" ; "[" ; ix = index_type ; "]" ; OF ; t = ty ; < Type_Array >

let set_range :=
    | e = expr ;                     { Set_Single(e) }
    | e1 = expr ; ".." ; e2 = expr ; { Set_Range(Some e1, Some e2) }
    | e1 = expr ; ".." ;             { Set_Range(Some e1, None) }
    |             ".." ; e2 = expr ; { Set_Range(None, Some e2) }

let index_type :=
    | "type" ; t = ident ; { Index_Enum(t) }
    | sz = expr ;          { Index_Int(sz) }

(****************************************************************
 * Statements
 ****************************************************************)

let block :=
    | list(statement)

let statement :=
    | simple_statement
    | compound_statement

let simple_statement :=
    | assignment_statement
    | f = path ; throws1=throws ; "(" ; args = separated_list(",", arg) ; ")" ; ";" ;
        { Stmt_UCall(f, args, throws1, Range($symbolstartpos, $endpos)) }
    | "return" ; e = expr ; ";" ; { Stmt_Return(e, Range($symbolstartpos, $endpos)) }
    | "return" ; ";" ;            { Stmt_Return(Expr_Tuple([]), Range($symbolstartpos, $endpos)) }
    | "assert" ; e = expr ; ";" ; { Stmt_Assert(e, Range($symbolstartpos, $endpos)) }
    | "throw"  ; e = expr ; ";" ; { Stmt_Throw(e, Range($symbolstartpos, $endpos)) }

let assignment_statement :=
    | "var" ; v = ident ; "," ; vs = separated_list(",", ident) ; ":" ; t = ty ; ";" ;
        { Stmt_VarDeclsNoInit(v :: vs, t, Range($symbolstartpos, $endpos)) }
    | "var" ; v = ident ; ":" ; t = ty ; ";" ;
        { Stmt_VarDeclsNoInit([v], t, Range($symbolstartpos, $endpos)) }
    | "var" ; dis = decl_item ; becomes ; i = expr ; ";" ;
        { Stmt_VarDecl(false, dis, i, Range($symbolstartpos, $endpos)) }
    | "let" ; dis = decl_item ; becomes ; i = expr ; ";" ;
        { Stmt_VarDecl(true, dis, i, Range($symbolstartpos, $endpos)) }
    | l = lexpr ; becomes ; r = expr ; ";" ;
        { Stmt_Assign(l, r, Range($symbolstartpos, $endpos)) }

(* This rule is just to give a better error message *)
let becomes :=
    | ":=" ; { () }
    | "=" ;  { raise (Parse_error_locn (Range($symbolstartpos, $endpos), "Use ':=' instead of '=' for assignment.")) }

let ty_opt :=
    | ":" ; ty = ty ; { Some ty }
    |                 { None }

let decl_item :=
    | v = ident ; oty = ty_opt ;                                  { DeclItem_Var(v, oty) }
    | "(" ; dis = separated_nonempty_list(",", decl_item) ; ")" ; { DeclItem_Tuple(dis) }
    | "_" ; oty = ty_opt ;                                        { DeclItem_Wildcard(oty) }

let lexpr :=
    | "_" ; { LExpr_Wildcard }
    | lexpr_name
    | "(" ; es = separated_nonempty2_list(",", lexpr) ; ")" ;   { LExpr_Tuple(es) }

let lexpr_name :=
    | lexpr_direct_name
    | lexpr_selected_component
    | lexpr_function_call

let lexpr_direct_name :=
    | v = path ; { LExpr_Var(v) }

let lexpr_selected_component :=
    | n = lexpr_name ; "." ; f = ident ; { LExpr_Field(n, f) }

let lexpr_function_call :=
    | e = lexpr ; "[" ; ss = separated_list(",", slice) ; "]" ; { LExpr_Slices(Isa_utils.type_unknown, e, ss) }

let arg :=
    | v = ident ; "=>" ; e = expr ; { (Some v, e) }
    | e = expr ;                    { (None,   e) }

(* This rule is just to give a better error message *)
let semi :=
    | ";" ; { () }
    | { raise (Parse_error_locn (Range($symbolstartpos, $endpos), "Missing semicolon after end/endif/...")) }

let compound_statement :=
    | conditional_statement
    | repetitive_statement
    | catch_statement
    | "begin" ; b = block ; "end" ; semi ; { Stmt_Block(b, Range($symbolstartpos, $endpos)) }

let conditional_statement :=
    | "if" ; c = expr ;
      "then" ; t = block ;
      els = s_elsif* ;
      f = optional_else ;
      one_of("end", "endif") ; semi ;
      { let loc = Range($symbolstartpos, $endpos(t)) in
        Stmt_If((c, t, loc)::els, f, Range($symbolstartpos, $endpos)) }
    | "case" ; e = expr ; "of" ;
      alts = nonempty_list(alt) ;
      ob = otherwise? ;
      one_of("end", "endcase") ; semi ;
      { Stmt_Case(e, None, alts, ob, Range($symbolstartpos, $endpos)) }

let s_elsif :=
    | "elsif" ; c = expr ; "then" ; b = block ; { (c, b, Range($symbolstartpos, $endpos)) }

let optional_else :=
    | "else" ; b = block ; { (b, Range($symbolstartpos, $endpos)) }
    |                      { ([], Range($symbolstartpos, $endpos)) }

let alt :=
    | "when" ; ps = separated_nonempty_list(",", pattern) ; cond = alt_cond? ; "=>" ; b = block ;
      { Alt_Alt(ps, cond, b, Range($symbolstartpos, $endpos)) }

let otherwise :=
    | "otherwise" ; "=>" ; b = block ; { (b, Range($symbolstartpos, $endpos)) }

let alt_cond :=
    | "if" ; e = expr ; { e }

let repetitive_statement :=
    | "for" ;
      v = ident ; vt = ty_opt ; becomes ;
      f = expr ; dir = direction ; t = expr ;
      "do" ;
      b = block ;
      one_of("end", "endfor") ; semi ;
      { Stmt_For(v, Option.value vt ~default:(Type_Integer(None)), f, dir, t, b, Range($symbolstartpos, $endpos)) }
    | "while" ; c = expr ; "do" ;
      b = block ;
      one_of("end", "endwhile") ; semi ;
      { Stmt_While(c, b, Range($symbolstartpos, $endpos)) }
    | "repeat" ; b = block ; "until" ; c = expr ; ";" ; pos = pos ;
      { Stmt_Repeat(b, c, pos, Range($symbolstartpos, $endpos)) }

let direction :=
    | "to"     ; { Direction_Up }
    | "downto" ; { Direction_Down }

let catch_statement :=
    | "try" ; b = block ;
      "catch" ; pos = pos ;
      cs = catcher* ;
      ob = otherwise? ;
      one_of("end", "endtry") ; semi ;
      { Stmt_Try(b, pos, cs, ob, Range($symbolstartpos, $endpos)) }

let catcher :=
    | "when" ; v = ident ; ":" ; tc=ident ; "=>" ; b = block ;
      { Catcher_Guarded(v, tc, b, Range($symbolstartpos, $endpos)) }

(****************************************************************
 * Declarations
 ****************************************************************)

let declarations_file :=
    | ds = declaration* ; EOF ; { ds }

let declaration :=
    | type_declaration
    | variable_declaration
    | function_declaration
    | function_instantiation
    | internal_definition
    | ffi_definition
    | module_definition

(****************************************************************
 * Type declarations
 ****************************************************************)

let type_declaration :=
    | "__builtin" ; "type" ; v = path ; ";" ;
      { Decl_BuiltinType(v, Range($symbolstartpos, $endpos)) }
    | "type" ; v = path ; ";" ;
      { Decl_Forward(v, Range($symbolstartpos, $endpos)) }
    | "type" ; v = path ; ps = ty_params_opt ; "=" ; ty = ty ; ";" ;
      { Decl_Typedef(v, ps, ty, Range($symbolstartpos, $endpos)) }
    | "record" ; v = path ; ps = ty_params_opt ; "=" ; "{" ; fs = separated_list(",", field) ; "}"; ";" ;
      { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
    | "bitfield" ; v = path ; "="; "{" ; fs = regfields ; "}"; ":" ; path ; "(" ; wd = expr ; ")" ; ";" ;
      { Decl_Typedef(v, [], Type_Bits(wd, fs), Range($symbolstartpos, $endpos)) } (* todo: second path is expected to be Bits - but is ignored *)
    | "exception" ; v = path ; ";" ;
      { Decl_Exception(v, [], Range($symbolstartpos, $endpos)) }
    | "exception" ; v = path ; "="; "{" ; fs = separated_nonempty_list(",", field) ; "}" ; ";" ;
      { Decl_Exception(v, fs, Range($symbolstartpos, $endpos)) }
    | "enumeration" ; v = path ; "="; "{" ; es = separated_list(",", path); "}"; ";" ;
      { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }

let ty_params_opt ==
    | "(" ; ps = separated_nonempty_list(",", ident) ; ")" ; { ps }
    |                                                        { [] }

let field :=
    | f = ident ; ":" ; t = ty ; { (f, t) }

let regfields :=
    | separated_list(",", regfield)

let regfield :=
    | f = ident ; "=>" ; "[" ; ss = separated_nonempty_list(",", slice) ; "]" ;
    { (ss, f) }

(****************************************************************
 * Function declarations
 ****************************************************************)

let function_declaration :=
    | hdr = function_header ; ";" ;
        { Decl_FunType(fst hdr, snd hdr, Range($symbolstartpos, $endpos)) }
    | hdr = function_header ; "begin" ; b = block ; "end" ;
        { Decl_FunDefn(fst hdr, snd hdr, b, Range($symbolstartpos, $endpos)) }

let throws :=
    | "!" ; { AlwaysThrow }
    | "?" ; { MayThrow }
    |       { NoThrow }

let builtin ==
    | "__builtin" ; { true }
    |               { false }

let function_header :=
    (* Read form *)
    | b = builtin ; "function" ; f = path ; throws=throws ;
      formals = formals_opt ; rty=returntype_opt ;
      { let (ps, args, is_getter_setter, array_syntax) = formals in
        let fty = { parameters=ps; args; setter_arg=None; rty=rty; use_array_syntax=array_syntax; is_getter_setter=is_getter_setter; throws; is_builtin=b } in
        (f, fty)
      }

    (* Write form *)
    | b = builtin ; "function" ; f = path ; throws=throws ;
      formals = formals_opt ; becomes ; value=ident ; ":" ; value_ty=ty ;
      { let (ps, args, is_getter_setter, array_syntax) = formals in
        let fty = { parameters=ps; args; setter_arg=Some(value,value_ty); rty=Isa_utils.type_unit; use_array_syntax=array_syntax; is_getter_setter=true; throws; is_builtin=b } in
        (f, fty)
      }

let formals_opt :=
    | "(" ; args = formals_with_default ; ")" ; { (fst args, snd args, false, false) }
    | "[" ; args = formals_with_default ; "]" ; { (fst args, snd args, true, true) }
    |                                           { ([], [], true, false) }

let formals_with_default :=
    | formals = separated_list(",", formal_with_default) ;
      { let ps = List.filter_map (fun (x, (v,ty,_)) -> if x then Some (v, Some ty) else None) formals in
        let args = List.filter_map (fun (x, y) -> if x then None else Some y) formals in
        (ps, args)
      }

let formal_with_default :=
    | imp = option("implicit") ; v = ident ; ":" ; ty = ty ;                      { (Option.is_some imp, (v, ty, None)) }
    | imp = option("implicit") ; v = ident ; ":" ; ty = ty ; becomes ; d = expr ; { (Option.is_some imp, (v, ty, Some d)) }

let returntype_opt :=
    | "->" ; ty = ty ; { ty }
    |                  { Isa_utils.type_unit }

let function_instantiation :=
    | "optimize"; "function" ; f = path ; "with" ; "{" ; ps = separated_nonempty_list(",", parameter_value) ; "}" ; ";" ;
        { Decl_FunInstance(f, ps, Range($symbolstartpos, $endpos)) }

let parameter_value :=
    | p = ident ; "=>" ; v = literal_expression ; { (p, Some v) }
    | p = ident ; "=>" ; "_"                    ; { (p, None) }

(****************************************************************
 * Variable declarations
 ****************************************************************)

let variable_declaration :=
    | "var" ; v = path ; ":" ; ty = ty ; ";" ;
        { Decl_Var(v, ty, Range($symbolstartpos, $endpos)) }
    | "let" ; v = path ; ty = ty_opt ; becomes ; e = expr ; ";" ;
        { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }

(****************************************************************
 * Internal declarations (only for use in stdlib)
 ****************************************************************)

let internal_definition :=
    | "__operator1" ; op = unop ; "=" ; vs = separated_nonempty_list(",", path) ; ";" ;
        { Decl_Operator1(op, vs, Range($symbolstartpos, $endpos)) }
    | "__operator2" ; op = binop ; "=" ; vs = separated_nonempty_list(",", path) ; ";" ;
        { Decl_Operator2(op, vs, Range($symbolstartpos, $endpos)) }

let unop :=
    | "-"   ; { Unop_Negate }
    | "not" ; { Unop_Not }
    | "!"   ; { Unop_Not }

let binop :=
    | "=="  ;          { Binop_Eq }
    | "!="  ;          { Binop_NtEq }
    | ">"   ;          { Binop_Gt }
    | ">="  ;          { Binop_GtEq }
    | "<"   ;          { Binop_Lt }
    | "<="  ;          { Binop_LtEq }
    | "+"   ;          { Binop_Plus }
    | "-"   ;          { Binop_Minus }
    | "*"   ;          { Binop_Multiply }
    | "/"   ;          { Binop_Divide }
    | "%"   ;          { Binop_Remainder }
    | "**"  ;          { Binop_Power }
    | "<<"  ;          { Binop_ShiftL }
    | ">>"  ;          { Binop_ShiftR }
    | "++"  ;          { Binop_Append }
    | "and" ;          { Binop_And }
    | "or"  ;          { Binop_Or }
    | "xor" ;          { Binop_Xor }
    | "and" ; "then" ; { Binop_AndThen }
    | "or"  ; "else" ; { Binop_OrElse }

(****************************************************************
 * FFI declarations
 ****************************************************************)

let ffi_definition :=
    | "foreign"; is_export=ffi_direction;
      "function"; nm = STRINGLIT; "="; f=path;
      "with"; "{"; ps = separated_list(",", ffi_parameter_value);"}"; ";";
      { Decl_FunFFI(nm, is_export, f, ps, Range($symbolstartpos, $endpos)) }
    | "foreign"; is_export=ffi_direction;
      "var"; nm=STRINGLIT; "="; v=path; ";";
      { Decl_VarFFI(nm, is_export, v, Range($symbolstartpos, $endpos)) }
    | "foreign"; is_export=ffi_direction;
      "type"; nm=STRINGLIT; "="; t=ty; ";";
      { Decl_TypeFFI(nm, is_export, t, Range($symbolstartpos, $endpos)) }

let ffi_parameter_value :=
    | p = ident ; "=>" ; v = literal_expression ; { (p, v) }

let ffi_direction :=
    | "import" ; { false }
    | "export" ; { true }

(****************************************************************
 * Module declarations
 ****************************************************************)

let module_definition :=
    | "use" ; p1 = path ; "as" ; p2 = path ; ";" ; { Decl_Use(p1, p2, Range($symbolstartpos, $endpos)) }

    (* dummy grammar rule - does not generate correct AST *)
    | "module" ; p = path ;
      export* ;
      import* ;
      module_file* ;
      "endmodule" ;
      { Decl_BuiltinType(p, Range($symbolstartpos, $endpos)) }

    (* dummy grammar rule - does not generate correct AST *)
    | "interface" ; p = path ;
      "end" ;
      { Decl_BuiltinType(p, Range($symbolstartpos, $endpos)) }

let export :=
    "export" ; path ; ":" ; path
let import :=
    "import" ; path ; ":" ; path
let module_file :=
    "file" ; STRINGLIT ; ";"

(* Additional entrypoints used by ASLi and testing framework *)

expr_command_start:
| cmd = expr EOF { cmd }

stmt_command_start:
| cmd = statement EOF { cmd }

stmts_command_start:
| cmd = list(statement) EOF { cmd }

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
