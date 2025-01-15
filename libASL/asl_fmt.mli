(****************************************************************
 * ASL pretty printer
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Format

(** Optionally show type parameters when printing ASL code *)
val show_type_params : bool ref

(** The typechecker desugars infix syntax to make it absolutely explicit
 *  what it means.  This is good for tools but bad for humans.
 *
 *  This flag causes expressions to be displayed with infix syntax.
 *)
val resugar_operators : bool ref

val add_binop : AST.binop -> Ident.t -> unit
val add_unop : AST.unop -> Ident.t -> unit

type comment = Lexing.position * Lexing.position * string
val comment_list : comment list ref

val loc : formatter -> Loc.t -> unit
val tycon : formatter -> Ident.t -> unit
val varname : formatter -> Ident.t -> unit
val funname : formatter -> Ident.t -> unit
val fieldname : formatter -> Ident.t -> unit
val varnames : formatter -> Ident.t list -> unit
val funnames : formatter -> Ident.t list -> unit
val unop : formatter -> AST.unop -> unit
val binop : formatter -> AST.binop -> unit
val intLit : formatter -> AST.intLit -> unit
val bitsLit : formatter -> AST.intLit -> unit
val maskLit : formatter -> AST.intLit -> unit
val realLit : formatter -> AST.intLit -> unit
val strLit : formatter -> AST.intLit -> unit
val throws : formatter -> AST.can_throw -> unit
val ty : formatter -> AST.ty -> unit
val types : formatter -> AST.ty list -> unit
val constraint_range : formatter -> AST.constraint_range -> unit
val constraints : formatter -> AST.constraint_range list -> unit
val slice : formatter -> AST.slice -> unit
val slices : formatter -> AST.slice list -> unit
val ixtype : formatter -> AST.ixtype -> unit
val expr : formatter -> AST.expr -> unit
val exprs : formatter -> AST.expr list -> unit
val pattern : formatter -> AST.pattern -> unit
val patterns : formatter -> AST.pattern list -> unit
val lexpr : formatter -> AST.lexpr -> unit
val lexprs : formatter -> AST.lexpr list -> unit
val varty : formatter -> Ident.t -> AST.ty -> unit
val decl_item : formatter -> AST.decl_item -> unit
val stmt : formatter -> AST.stmt -> unit
val indented_block : formatter -> AST.stmt list -> unit
val parameters : formatter -> (Ident.t * AST.ty option) list -> unit
val formals : formatter -> (Ident.t * AST.ty) list -> unit
val function_type : formatter -> AST.function_type -> unit
val declaration : formatter -> AST.declaration -> unit
val declarations : formatter -> AST.declaration list -> unit
val delimiter : formatter -> string -> unit
val keyword : formatter -> string -> unit
val amp : formatter -> unit
val amp_amp : formatter -> unit
val bang : formatter -> unit
val bang_eq : formatter -> unit
val bar_bar : formatter -> unit
val caret : formatter -> unit
val colon : formatter -> unit
val dot : formatter -> unit
val dot_dot : formatter -> unit
val eq : formatter -> unit
val eq_eq : formatter -> unit
val eq_gt : formatter -> unit
val gt : formatter -> unit
val gt_eq : formatter -> unit
val gt_gt : formatter -> unit
val lbrace_lbrace : formatter -> unit
val lt : formatter -> unit
val lt_eq : formatter -> unit
val lt_lt : formatter -> unit
val minus : formatter -> unit
val plus : formatter -> unit
val plus_colon : formatter -> unit
val star_colon : formatter -> unit
val plus_plus : formatter -> unit
val rbrace_rbrace : formatter -> unit
val semicolon : formatter -> unit
val slash : formatter -> unit
val star : formatter -> unit
val kw_and : formatter -> unit
val kw_array : formatter -> unit
val kw_as : formatter -> unit
val kw_assert : formatter -> unit
val kw_begin : formatter -> unit
val kw_bits : formatter -> unit
val kw_case : formatter -> unit
val kw_catch : formatter -> unit
val kw_config : formatter -> unit
val kw_constant : formatter -> unit
val kw_div_exact : formatter -> unit
val kw_divrm : formatter -> unit
val kw_do : formatter -> unit
val kw_downto : formatter -> unit
val kw_else : formatter -> unit
val kw_elsif : formatter -> unit
val kw_enumeration : formatter -> unit
val kw_end : formatter -> unit
val kw_for : formatter -> unit
val kw_func : formatter -> unit
val kw_getter : formatter -> unit
val kw_if : formatter -> unit
val kw_in : formatter -> unit
val kw_let : formatter -> unit
val kw_mod : formatter -> unit
val kw_not : formatter -> unit
val kw_of : formatter -> unit
val kw_or : formatter -> unit
val kw_otherwise : formatter -> unit
val kw_quot : formatter -> unit
val kw_record : formatter -> unit
val kw_rem : formatter -> unit
val kw_repeat : formatter -> unit
val kw_return : formatter -> unit
val kw_see : formatter -> unit
val kw_setter : formatter -> unit
val kw_then : formatter -> unit
val kw_throw : formatter -> unit
val kw_to : formatter -> unit
val kw_try : formatter -> unit
val kw_type : formatter -> unit
val kw_typeof : formatter -> unit
val kw_underscore_array : formatter -> unit
val kw_underscore_builtin : formatter -> unit
val kw_underscore_operator1 : formatter -> unit
val kw_underscore_operator2 : formatter -> unit
val kw_underscore_readwrite : formatter -> unit
val kw_underscore_write : formatter -> unit
val kw_unknown : formatter -> unit
val kw_until : formatter -> unit
val kw_var : formatter -> unit
val kw_when : formatter -> unit
val kw_while : formatter -> unit
val kw_xor : formatter -> unit

(*****************************************
 * End
 *****************************************)
