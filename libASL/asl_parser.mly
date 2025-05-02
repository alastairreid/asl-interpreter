(****************************************************************
 * ASL grammar file
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

%{
open Asl_ast
open Loc

(* The following type is used in the parser in places where
 * the typechecker is expected to insert an inferred type that is
 * not known when we are parsing the source code.
 *
 * The particular value used for type_unknown is not especially
 * important because it should never appear after the typechecker.
 * The current representation is intended to make it easier to
 * diagnose failures.
 *
 * It would also be possible to use Option to handle this
 * and use "None" instead of "type_unknown". Using Option
 * is not especially helpful because all later passes assume that
 * the type is available.
 *)
let type_unknown = Type_Constructor (Ident.mk_ident "<type_unknown>", [])
%}

%token UNDERSCORE_UNDERSCORE_ASSERT  (* __assert *)
%token UNDERSCORE_UNDERSCORE_BUILTIN  (* __builtin *)
%token UNDERSCORE_UNDERSCORE_IN   (* __in *)
%token UNDERSCORE_UNDERSCORE_LET  (* __let *)
%token UNDERSCORE_UNDERSCORE_OPERATOR_ONE  (* __operator1 *)
%token UNDERSCORE_UNDERSCORE_OPERATOR_TWO  (* __operator2 *)

%token COLON  (* : *)
%token COMMA  (* , *)
%token DOT  (* . *)
%token DOT_DOT  (* .. *)
%token EQ  (* = *)
%token LBRACE  (* { *)
%token LBRACK  (* [ *)
%token LPAREN  (* ( *)
%token RBRACE  (* } *)
%token RBRACK  (* ] *)
%token RPAREN  (* ) *)
%token SEMICOLON  (* ; *)

%token <Primops.bitvector> BITSLIT  (* metavarroot bitsLit *)
%token <int option * Primops.bigint> INTLIT  (* metavarroot intLit *)
%token <string> ID  (* metavarroot id *)
%token <string> MASKLIT  (* metavarroot maskLit *)
%token <string> REALLIT  (* metavarroot realLit *)
%token <string> STRINGLIT  (* metavarroot stringLit *)

(* storage tokens *)
%token CONSTANT  (* constant *)
%token CONFIG  (* config *)
%token EQ_GT  (* => *)
%token FUNC  (* func *)
%token GETTER  (* getter *)
%token LET  (* let *)
%token SETTER  (* setter *)
%token VAR  (* var *)

(* type tokens *)
%token ARRAY  (* array *)
%token BITS  (* bits *)
%token ENUMERATION  (* enumeration *)
%token EXCEPTION  (* exception *)
%token INTEGER  (* integer *)
%token OF  (* of *)
%token RECORD  (* record *)
%token TYPE  (* type *)
%token TYPEOF  (* typeof *)

(* slice tokens *)
%token PLUS_COLON  (* +: *)
%token MINUS_COLON (* -: *)
%token STAR_COLON  (* *: *)

(* statement tokens *)
%token ASSERT  (* assert *)
%token BEGIN  (* begin *)
%token CASE  (* case *)
%token CATCH  (* catch *)
%token DO  (* do *)
%token DOWNTO  (* downto *)
%token END  (* end *)
%token FOR  (* for *)
%token IF  (* if *)
%token OTHERWISE  (* otherwise *)
%token REPEAT  (* repeat *)
%token RETURN  (* return *)
%token THEN  (* then *)
%token THROW  (* throw *)
%token TO  (* to *)
%token TRY  (* try *)
%token UNTIL  (* until *)
%token WHEN  (* when *)
%token WHERE  (* where *)
%token WHILE  (* while *)

(* expressions tokens *)
%token AS  (* as *)
%token ELSE  (* else *)
%token ELSIF  (* elsif *)
%token IN  (* IN *)
%token UNKNOWN  (* UNKNOWN *)

(* binop tokens *)
%token AMPERSAND_AMPERSAND  (* && *)
%token AND  (* AND *)
%token BANG_EQ  (* != *)
%token BAR_BAR  (* || *)
%token CARET  (* ^ *)
%token DIV  (* DIV *)
%token DIVRM  (* DIVRM *)
%token EQ_EQ  (* == *)
%token GT  (* > *)
%token GT_EQ  (* >= *)
%token GT_GT  (* >> *)
%token LT  (* < *)
%token LT_EQ  (* <= *)
%token LT_LT  (* << *)
%token LT_MINUS_GT  (* <-> *)
%token MINUS  (* - *)
%token MINUS_MINUS_GT  (* --> *)
%token MOD  (* MOD *)
%token OR  (* OR *)
%token PLUS  (* + *)
%token PLUS_PLUS  (* ++ *)
%token QUERY (* ? *)
%token QUOT  (* QUOT *)
%token REM  (* REM *)
%token SLASH  (* / *)
%token STAR  (* * *)
%token XOR  (* XOR *)

(* unop tokens *)
%token BANG  (* ! *)
%token NOT  (* NOT *)

%token EOF

%start <Asl_ast.declaration list> declarations_start
%start <Asl_ast.expr> expr_command_start
%start <Asl_ast.stmt> stmt_command_start
%start <Asl_ast.stmt list> stmts_command_start
%start <Asl_ast.config_command> config_command_start


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

ident:
| id = ID { Ident.mk_ident id }

declarations:
| declaration0 = list(declaration) { declaration0 }

declaration:
| type_declaration = type_declaration { type_declaration }
| variable_declaration = variable_declaration { variable_declaration }
| function_declaration = function_declaration { function_declaration }
| procedure_declaration = procedure_declaration { procedure_declaration }
| getter_declaration = getter_declaration { getter_declaration }
| setter_declaration = setter_declaration { setter_declaration }
| internal_definition = internal_definition { internal_definition }

type_declaration:
| UNDERSCORE_UNDERSCORE_BUILTIN TYPE v = ident SEMICOLON
    { Decl_BuiltinType(v, Range($symbolstartpos, $endpos)) }
| TYPE v = ident SEMICOLON
    { Decl_Forward(v, Range($symbolstartpos, $endpos)) }
| RECORD v = ident LBRACE fs = nonempty_list(field) RBRACE SEMICOLON
    { Decl_Record(v, [], fs, Range($symbolstartpos, $endpos)) }
| RECORD v = ident ps = ty_params LBRACE fs = nonempty_list(field) RBRACE SEMICOLON
    { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF RECORD LBRACE fs = separated_nonempty_list(COMMA, field_asl1) RBRACE SEMICOLON
    { Decl_Record(v, [], fs, Range($symbolstartpos, $endpos)) }
| TYPE v = ident ps = ty_params OF RECORD LBRACE fs = separated_nonempty_list(COMMA, field_asl1) RBRACE SEMICOLON
    { Decl_Record(v, ps, fs, Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF EXCEPTION SEMICOLON
    { Decl_Exception(v, [], Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF EXCEPTION LBRACE fs = separated_nonempty_list(COMMA, field_asl1) RBRACE SEMICOLON
    { Decl_Exception(v, fs, Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF ty = ty SEMICOLON
    { Decl_Typedef(v, [], ty, Range($symbolstartpos, $endpos)) }
| TYPE v = ident ps = ty_params OF ty = ty SEMICOLON
    { Decl_Typedef(v, ps, ty, Range($symbolstartpos, $endpos)) }
| ENUMERATION v = ident LBRACE es = separated_list(COMMA, ident) RBRACE SEMICOLON
    { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }
| TYPE v = ident OF ENUMERATION LBRACE es = separated_list(COMMA, ident) RBRACE SEMICOLON
    { Decl_Enum(v, es, Range($symbolstartpos, $endpos)) }

ty_params:
| LPAREN ps = separated_nonempty_list(COMMA, ident) RPAREN { ps }

field:
| ident = ident COLON ty = ty SEMICOLON { (ident, ty) }

(* To provide a period of backwards compatibility, we support fields that
 * (1) end with a semicolon (deprecated),
 * (2) do not end with a semicolon but separated in a list by a colon.
 *)
field_asl1:
| ident = ident COLON ty = ty { (ident, ty) }

variable_declaration:
| VAR v = ident COLON ty = ty SEMICOLON
    { Decl_Var(v, ty, Range($symbolstartpos, $endpos)) }
| CONSTANT v = ident ty = ty_opt EQ e = expr SEMICOLON
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }
| LET v = ident ty = ty_opt EQ e = expr SEMICOLON
    { Decl_Const(v, ty, e, Range($symbolstartpos, $endpos)) }

ixtype:
| ident = ident { Index_Enum(ident) }
| expr = expr { Index_Int(expr) }

throws:
| BANG { AlwaysThrow }
| QUERY { MayThrow }
| { NoThrow }

function_declaration:
| UNDERSCORE_UNDERSCORE_BUILTIN FUNC f = ident throws=throws ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { let fty = { parameters=ps; args; setter_arg=None; rty=Some ty; use_array_syntax=false; is_getter_setter=false; throws } in
      Decl_BuiltinFunction(f, fty, Range($symbolstartpos, $endpos)) }
| FUNC f = ident throws=throws ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty SEMICOLON
    { let fty = { parameters=ps; args; setter_arg=None; rty=Some ty; use_array_syntax=false; is_getter_setter=false; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| FUNC f = ident throws=throws ps = parameters_opt LPAREN args = formal_list RPAREN EQ_GT ty = ty BEGIN b = block END
    { let fty = { parameters=ps; args; setter_arg=None; rty=Some ty; use_array_syntax=false; is_getter_setter=false; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

procedure_declaration:
| FUNC f = ident throws=throws ps = parameters_opt LPAREN args = formal_list RPAREN SEMICOLON
    { let fty = { parameters=ps; args; setter_arg=None; rty=None; use_array_syntax=false; is_getter_setter=false; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| FUNC f = ident throws=throws ps = parameters_opt LPAREN args = formal_list RPAREN BEGIN b = block END
    { let fty = { parameters=ps; args; setter_arg=None; rty=None; use_array_syntax=false; is_getter_setter=false; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

parameters_opt:
| LBRACE pars = parameter_list RBRACE { pars }
| { [] }

parameter_list:
| pars = separated_nonempty_list(COMMA, parameter) { pars }

parameter:
| par = ident ty = ty_opt { (par, ty) }

ty_opt:
| COLON ty = ty { Some ty }
| { None }

formal_list:
| formal0 = separated_list(COMMA, formal) { formal0 }

formal:
| ident = ident COLON ty = ty { (ident, ty) }

getter_declaration:
| GETTER f = ident throws=throws ps = parameters_opt EQ_GT ty = ty SEMICOLON
    { let fty = { parameters=ps; args=[]; setter_arg=None; rty=Some ty; use_array_syntax=false; is_getter_setter=true; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| GETTER f = ident throws=throws ps = parameters_opt EQ_GT ty = ty BEGIN b = block END
    { let fty = { parameters=ps; args=[]; setter_arg=None; rty=Some ty; use_array_syntax=false; is_getter_setter=true; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }
| GETTER f = ident throws=throws ps = parameters_opt LBRACK args = formal_list RBRACK EQ_GT ty = ty SEMICOLON
    { let fty = { parameters=ps; args; setter_arg=None; rty=Some ty; use_array_syntax=true; is_getter_setter=true; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| GETTER f = ident throws=throws ps = parameters_opt LBRACK args = formal_list RBRACK EQ_GT ty = ty BEGIN b = block END
    { let fty = { parameters=ps; args; setter_arg=None; rty=Some ty; use_array_syntax=true; is_getter_setter=true; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

setter_declaration:
| SETTER f = ident throws=throws ps = parameters_opt EQ v = ident COLON ty = ty SEMICOLON
    { let fty = { parameters=ps; args=[]; setter_arg=Some (v, ty); rty=None; use_array_syntax=false; is_getter_setter=true; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| SETTER f = ident throws=throws ps = parameters_opt EQ v = ident COLON ty = ty BEGIN b = block END
    { let fty = { parameters=ps; args=[]; setter_arg=Some (v, ty); rty=None; use_array_syntax=false; is_getter_setter=true; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }
| SETTER f = ident throws=throws ps = parameters_opt LBRACK args = formal_list RBRACK EQ v = ident COLON ty = ty SEMICOLON
    { let fty = { parameters=ps; args; setter_arg=Some (v, ty); rty=None; use_array_syntax=true; is_getter_setter=true; throws } in
      Decl_FunType(f, fty, Range($symbolstartpos, $endpos)) }
| SETTER f = ident throws=throws ps = parameters_opt LBRACK args = formal_list RBRACK EQ v = ident COLON ty = ty BEGIN b = block END
    { let fty = { parameters=ps; args; setter_arg=Some (v, ty); rty=None; use_array_syntax=true; is_getter_setter=true; throws } in
      Decl_FunDefn(f, fty, b, Range($symbolstartpos, $endpos)) }

internal_definition:
| UNDERSCORE_UNDERSCORE_OPERATOR_ONE op = unop EQ vs = separated_nonempty_list(COMMA, ident) SEMICOLON
    { Decl_Operator1(op, vs, Range($symbolstartpos, $endpos)) }
| UNDERSCORE_UNDERSCORE_OPERATOR_TWO op = binop EQ vs = separated_nonempty_list(COMMA, ident) SEMICOLON
    { Decl_Operator2(op, vs, Range($symbolstartpos, $endpos)) }
| CONFIG v = ident COLON ty = ty EQ e = expr SEMICOLON
    { Decl_Config(v, ty, e, Range($symbolstartpos, $endpos)) }

ty:
| ident = ident
    { Type_Constructor(ident, []) }
| INTEGER ocrs = constraint_opt
    { Type_Integer(ocrs) }
| BITS LPAREN n = expr RPAREN
    { Type_Bits(n, []) }
| tc = ident LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN
    { Type_Constructor(tc, es) }
| TYPEOF LPAREN e = expr RPAREN
    { Type_OfExpr(e) }
| BITS LPAREN wd = expr RPAREN LBRACE fs = regfields RBRACE
    { Type_Bits(wd, fs) }
| ARRAY LBRACK ixtype = ixtype RBRACK OF ty = ty
    { Type_Array(ixtype, ty) }
| LPAREN tys = separated_list(COMMA, ty) RPAREN
    { Type_Tuple(tys) }

constraint_opt:
| crs = constraints { Some crs }
| { None }

constraints:
| LBRACE crs = separated_nonempty_list(COMMA, constraint_range) RBRACE
    { crs }

constraint_range:
| c = expr { Constraint_Single(c) }
| c1 = expr DOT_DOT c2 = expr { Constraint_Range(c1, c2) }

regfields:
| fs = list(regfield) { fs }
| f = regfield COMMA fs = regfields { f :: fs }

regfield:
| LBRACK slices = separated_nonempty_list(COMMA, slice) RBRACK ident = ident
    { (slices, ident) }

stmt:
| simple_stmt = simple_stmt { simple_stmt }
| compound_stmt = compound_stmt { compound_stmt }

compound_stmt:
| conditional_stmt = conditional_stmt { conditional_stmt }
| repetitive_stmt = repetitive_stmt { repetitive_stmt }
| catch_stmt = catch_stmt { catch_stmt }
| BEGIN block = block END { Stmt_Block(block, Range($symbolstartpos, $endpos)) }

block:
| stmts = list(stmt) { stmts }

assignment_stmt:
| VAR v = ident COMMA vs = separated_list(COMMA, ident) COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit(v :: vs, ty, Range($symbolstartpos, $endpos)) }
| VAR v = ident COLON ty = ty SEMICOLON
    { Stmt_VarDeclsNoInit([v], ty, Range($symbolstartpos, $endpos)) }
| VAR dis = decl_item EQ i = expr SEMICOLON
    { Stmt_VarDecl(dis, i, Range($symbolstartpos, $endpos)) }
| LET dis = decl_item EQ i = expr SEMICOLON
    { Stmt_ConstDecl(dis, i, Range($symbolstartpos, $endpos)) }
| CONSTANT dis = decl_item EQ i = expr SEMICOLON
    { Stmt_ConstDecl(dis, i, Range($symbolstartpos, $endpos)) }
| l = lexpr EQ r = expr SEMICOLON
    { Stmt_Assign(l, r, Range($symbolstartpos, $endpos)) }

decl_item:
| v = ident  oty = ty_opt
    { DeclItem_Var(v, oty) }
| LPAREN dis = separated_nonempty_list(COMMA, decl_item) RPAREN
    { DeclItem_Tuple(dis) }
| LBRACK dis = separated_nonempty_list(COMMA, decl_bit) RBRACK
    { DeclItem_BitTuple(dis) }
| MINUS oty = ty_opt
    { DeclItem_Wildcard(oty) }

decl_bit:
| v = ident oty = ty_opt { (Some v, Option.value oty ~default:(Type_Bits (Asl_utils.one, []))) }
| MINUS     oty = ty_opt { (None,   Option.value oty ~default:(Type_Bits (Asl_utils.one, []))) }

lexpr:
| MINUS
    { LExpr_Wildcard }
| v = ident
    { LExpr_Var(v) }
| e = lexpr DOT f = ident
    { LExpr_Field(e, f) }
| e = lexpr DOT LBRACK fs = separated_nonempty_list(COMMA, ident) RBRACK
    { LExpr_Fields(e, fs) }
| e = lexpr LBRACK ss = separated_list(COMMA, slice) RBRACK
    { LExpr_Slices(type_unknown, e, ss) }
| LBRACK es = separated_nonempty2_list(COMMA, lexpr) RBRACK
    { LExpr_BitTuple([], es) }
| LPAREN es = separated_nonempty2_list(COMMA, lexpr) RPAREN
    { LExpr_Tuple(es) }
| LPAREN e = lexpr RPAREN
    { e }

simple_stmt:
| assignment_stmt = assignment_stmt
    { assignment_stmt }
| f = ident throws1=throws LPAREN args = separated_list(COMMA, expr) RPAREN throws2=throws SEMICOLON
    { Stmt_TCall(f, [], args, (if throws1<>NoThrow then throws1 else throws2), Range($symbolstartpos, $endpos)) }
| RETURN e = expr SEMICOLON
    { Stmt_FunReturn(e, Range($symbolstartpos, $endpos)) }
| RETURN SEMICOLON
    { Stmt_ProcReturn(Range($symbolstartpos, $endpos)) }
| ASSERT e = expr SEMICOLON
    { Stmt_Assert(e, Range($symbolstartpos, $endpos)) }
| THROW e = expr SEMICOLON
    { Stmt_Throw(e, Range($symbolstartpos, $endpos)) }

conditional_stmt:
| IF c = expr THEN t = block els = list(s_elsif) f = optional_else END
    { Stmt_If(c, t, els, f, Range($symbolstartpos, $endpos)) }
| CASE e = expr OF alts = nonempty_list(alt) ob = opt_otherwise END
    { Stmt_Case(e, None, alts, ob, Range($symbolstartpos, $endpos)) }

s_elsif:
| ELSIF c = expr THEN b = block
    { S_Elsif_Cond(c, b, Range($symbolstartpos, $endpos)) }

optional_else:
| ELSE b = block { (b, Range($symbolstartpos, $endpos)) }
| { ([], Range($symbolstartpos, $endpos)) }

alt:
| WHEN ps = separated_nonempty_list(COMMA, pattern) oalt = opt_altcond EQ_GT b = block
    { Alt_Alt(ps, oalt, b, Range($symbolstartpos, $endpos)) }

opt_otherwise:
| OTHERWISE EQ_GT b = block { Some(b, Range($symbolstartpos, $endpos)) }
| { None }

opt_altcond:
| WHERE e = expr { Some(e) }
| { None }

pattern:
| i = INTLIT {
    ( match Value.from_intLit i with
    | Some v -> Pat_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| MINUS i = INTLIT {
    ( match Value.from_intLit (Value.negate_intLit i) with
    | Some v -> Pat_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| b = BITSLIT { Pat_Lit (Value.VBits b) }
| m = MASKLIT { Pat_Lit (Value.from_maskLit m) }
| c = ident { Pat_Const(c) }
| MINUS { Pat_Wildcard }
| LPAREN ps = separated_nonempty2_list(COMMA, pattern) RPAREN { Pat_Tuple(ps) }
| LBRACE aps = separated_list(COMMA, apattern) RBRACE { Pat_Set(aps) }

apattern:
| p1 = expr DOT_DOT p2 = expr { Pat_Range(p1, p2) }
| p = expr { Pat_Single(p) }

repetitive_stmt:
| FOR v = ident ty_opt = ty_opt EQ f = expr dir = direction t = expr DO b = block END
    { Stmt_For(v, Option.value ty_opt ~default:(Type_Integer(None)), f, dir, t, b, Range($symbolstartpos, $endpos)) }
| WHILE c = expr DO b = block END
    { Stmt_While(c, b, Range($symbolstartpos, $endpos)) }
| REPEAT b = block UNTIL c = expr SEMICOLON pos = pos
    { Stmt_Repeat(b, c, pos, Range($symbolstartpos, $endpos)) }

direction:
| TO { Direction_Up }
| DOWNTO { Direction_Down }

catch_stmt:
| TRY b = block CATCH pos = pos cs = list(catcher) ob = opt_otherwise END
    { Stmt_Try(b, pos, cs, ob, Range($symbolstartpos, $endpos)) }

catcher:
| WHEN v = ident COLON tc=ident EQ_GT b = block
    { Catcher_Guarded(v, tc, b, Range($symbolstartpos, $endpos)) }

expr:
| ce = conditional_expression { ce }

conditional_expression:
| IF c = cexpr THEN t = expr els = list(e_elsif) ELSE e = expr
    { Expr_If(c, t, els, e) }
| UNDERSCORE_UNDERSCORE_LET v = ident COLON ty = ty EQ e = expr UNDERSCORE_UNDERSCORE_IN b = expr
    { Expr_Let(v, ty, e, b) }
| UNDERSCORE_UNDERSCORE_ASSERT e1 = expr UNDERSCORE_UNDERSCORE_IN e2 = expr
    { Expr_Assert(e1, e2, Range($symbolstartpos, $endpos)) }
| cexpr = cexpr { cexpr }

e_elsif:
| ELSIF c = expr THEN e = expr { E_Elsif_Cond(c, e) }

cexpr:
| bexpr = bexpr fs = list(factor)
    { buildExpression bexpr fs (Range($startpos(bexpr), $endpos(fs))) }

factor:
| op = binop e = bexpr { Factor_BinOp(op, e) }

binop:
| EQ_EQ { Binop_Eq }
| BANG_EQ { Binop_NtEq }
| GT { Binop_Gt }
| GT_EQ { Binop_GtEq }
| LT { Binop_Lt }
| LT_EQ { Binop_LtEq }
| PLUS { Binop_Plus }
| MINUS { Binop_Minus }
| STAR { Binop_Multiply }
| SLASH { Binop_Divide }
| CARET { Binop_Power }
| QUOT { Binop_Quot }
| REM { Binop_Rem }
| DIV { Binop_Div_exact }
| DIVRM { Binop_Divrm }
| MOD { Binop_Mod }
| LT_LT { Binop_ShiftL }
| GT_GT { Binop_ShiftR }
| AMPERSAND_AMPERSAND { Binop_BoolAnd }
| BAR_BAR { Binop_BoolOr }
| LT_MINUS_GT { Binop_BoolIff }
| MINUS_MINUS_GT { Binop_BoolImplies }
| OR { Binop_BitOr }
| XOR { Binop_BitXor }
| AND { Binop_BitAnd }
| PLUS_PLUS { Binop_Append }

bexpr:
| op = unop e = fexpr { Expr_Unop(op, e) }
| e = fexpr { e }

fexpr:
| e = fexpr DOT f = ident
    { Expr_Field(e, f) }
| e = fexpr DOT LBRACK fs = separated_nonempty_list(COMMA, ident) RBRACK
    { Expr_Fields(e, fs) }
| e = fexpr LBRACK ss = separated_list(COMMA, slice) RBRACK
    { Expr_Slices(type_unknown, e, ss) }
| e = fexpr IN p = pattern
    { Expr_In(e, p) }
| e = aexpr
    { e }

aexpr:
| literal_expression = literal_expression
    { literal_expression }
| v = ident
    { Expr_Var(v) }
| f = ident LPAREN es = separated_list(COMMA, expr) RPAREN throws=throws
    { Expr_TApply(f, [], es, throws) }
| f = ident QUERY LPAREN es = separated_list(COMMA, expr) RPAREN
    { Expr_TApply(f, [], es, MayThrow) }
| tc = ident
    LPAREN es = separated_list(COMMA, expr) RPAREN
    LBRACE fas = separated_nonempty_list(COMMA, field_assignment) RBRACE
    { Expr_RecordInit(tc, es, fas) }
| tc = ident LBRACE fas = separated_nonempty_list(COMMA, field_assignment) RBRACE
    { Expr_RecordInit(tc, [], fas) }
| ARRAY LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN
    { Expr_ArrayInit(es) }
| LPAREN e = expr RPAREN
    { e }
| LPAREN es = separated_nonempty2_list(COMMA, expr) RPAREN
    { Expr_Tuple(es) }
| LBRACK es = separated_nonempty2_list(COMMA, expr) RBRACK
    { Expr_Concat([], es) }
| UNKNOWN COLON t = ty
    { Expr_Unknown(t) }
| e = aexpr AS c = constraints
    { Expr_AsConstraint(e, c) }
| e = aexpr AS t = ty
    { Expr_AsType(e, t) }

field_assignment:
| ident = ident EQ expr = expr { (ident, expr) }

unop:
| MINUS { Unop_Negate }
| BANG { Unop_BoolNot }
| NOT { Unop_BitsNot }

slice:
| e = expr  { Slice_Single(e) }
| hi = expr COLON lo = expr { Slice_HiLo(hi, lo) }
| lo = expr PLUS_COLON wd = expr { Slice_LoWd(lo, wd) }
| hi = expr MINUS_COLON wd = expr { Slice_HiWd(hi, wd) }
| lo = expr STAR_COLON wd = expr { Slice_Element(lo, wd) }

literal_expression:
| i = INTLIT {
    ( match Value.from_intLit i with
    | Some v -> Expr_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| MINUS i = INTLIT {
    ( match Value.from_intLit (Value.negate_intLit i) with
    | Some v -> Expr_Lit v
    | None -> raise (Parse_error_locn (Range($symbolstartpos, $endpos), "integer is too big for bounds"))
    )
  }
| r = REALLIT { Expr_Lit(Value.from_realLit r) }
| b = BITSLIT { Expr_Lit(Value.VBits b) }
| m = MASKLIT { Expr_Lit(Value.from_maskLit m) }
| s = STRINGLIT { Expr_Lit(Value.from_stringLit s) }

expr_command:
| expr = expr { expr }

stmt_command:
| stmt = stmt { stmt }

stmts_command:
| stmts = list(stmt) { stmts }

config_command:
| v = ident EQ e = expr { CLI_Config(v, e) }

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
