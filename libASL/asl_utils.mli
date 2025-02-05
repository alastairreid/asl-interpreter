(****************************************************************
 * ASL utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL utility functions *)

module AST = Asl_ast
module FMT = Asl_fmt
module FMTUtils = Format_utils
open Identset

(****************************************************************)
(** {2 Name supply}                                             *)
(****************************************************************)

class nameSupply : string ->
  object
    method fresh : Ident.t
    method reset : unit
  end

(****************************************************************)
(** {2 Equivalence classes}                                     *)
(****************************************************************)

type tree = { mutable parent : tree; data : Ident.t }
(** Equivalence classes are represented by trees.

    The root of the tree is the canonical member of the class.
    Traversing the parent node takes you closer to the canonical member.
    The root is its own parent.
 *)

(** Equivalence class support (to support unification, and similar)

    The implementation is based on
    {{:https://en.wikipedia.org/wiki/Disjoint-set_data_structure}Wikipedia: Union-Find}.
    I have not implemented all the optimizations they suggest
    because I expect sets to be quite small in practice.
 *)

class equivalences :
  object
    (* Find the canonical member of the set containing 'x' *)
    method canonicalize : Ident.t -> Ident.t

    (* Merge the sets containing 'x' and 'y' *)
    method merge : Ident.t -> Ident.t -> unit

    (* Return mapping from identifiers to the canonical representation of their
     * equivalence class
     *)
    method mapping : Ident.t Bindings.t

    (* Construct equivalence classes for each canonical member of a class.
     *
     * The implementation of this could be made more efficient by adding
     * pointers to trees so that we can map each canonical member to a
     * tree containing all the nodes that point to it.
     * But this implementation just does a linear scan over all the members
     * of the forest.
     *)
    method classes : IdentSet.t Bindings.t

    (* Print equivalence classes adding a prefix at the start of every line of
     * output.
     *)
    method pp : Format.formatter -> string -> unit
  end

(****************************************************************)
(** {1 AST Transformation Utilities}                            *)
(****************************************************************)

(****************************************************************)
(** {2 Calculating free variables of expressions and types}     *)
(****************************************************************)

val fv_expr : AST.expr -> IdentSet.t
val fv_type : AST.ty -> IdentSet.t
val fv_args : (Ident.t * AST.ty) list -> IdentSet.t
val fv_stmts : AST.stmt list -> IdentSet.t
val fv_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculating assigned variables in statements}            *)
(****************************************************************)

val assigned_vars_of_stmts : AST.stmt list -> IdentSet.t
val assigned_vars_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculating subexpressions}                              *)
(****************************************************************)

val subexprs_of_expr : AST.expr -> AST.expr list

(****************************************************************)
(** {2 Calculate types used in expressions and statements}      *)
(****************************************************************)

val types_of_expr : AST.expr -> IdentSet.t
val types_of_stmts : AST.stmt list -> IdentSet.t
val types_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Calculate functions and procedures called in statements} *)
(****************************************************************)

val calls_of_expr : AST.expr -> IdentSet.t
val calls_of_stmts : AST.stmt list -> IdentSet.t
val calls_of_decl : AST.declaration -> IdentSet.t

(****************************************************************)
(** {2 Extract location info from AST nodes}                    *)
(****************************************************************)

val stmt_loc : AST.stmt -> Loc.t
val decl_loc : AST.declaration -> Loc.t

(****************************************************************)
(** {2 Keep definitions reachable from roots}                   *)
(****************************************************************)

val decl_name : AST.declaration -> Ident.t option

val monomorphizable_decl_to_ident_and_decl : AST.declaration -> (Ident.t * AST.declaration) option

(** Create mapping from identifiers to all declarations with that name.
 *
 * There can be multiple definitions with the same name so construct a map
 * from names to a list of definitionsw
 *)
val decls_map_of : AST.declaration list -> (AST.declaration list) Bindings.t

(* construct map of Union { x -> f x | for x in xs } *)
val memoize : Ident.t list -> (Ident.t -> IdentSet.t) -> IdentSet.t Bindings.t

(* construct map of Union { f x -> x | for x in xs } *)
val rev_memoize : Ident.t list -> (Ident.t -> IdentSet.t) -> IdentSet.t Bindings.t

(* Generate list of objects reachable from roots
 *
 * If the graph is acyclic, the resulting list will be
 * sorted in topological order such that 'x' occurs before 'y'
 * in the result if 'y' is transitively reachable from 'x'.
 * (Note that this is the reverse of the order that is
 * needed for code generation.)
 *
 * If the graph is cyclic, there are no ordering guarantees.
 *)
val reach : (Ident.t -> IdentSet.t) -> Ident.t list -> Ident.t list

(* Convert bindings to a function (using a default value for absent values *)
val bindings_to_function : 'a Bindings.t -> 'a -> Ident.t -> 'a

(* Generate list of declarations reachable from roots
 *
 * If the graph is acyclic, the resulting list will be
 * sorted in topological order such that 'x' occurs before 'y'
 * in the result if 'y' is transitively reachable from 'x'.
 * (Note that this is the reverse of the order that is
 * needed for code generation.)
 *
 * If the graph is cyclic, there are no ordering guarantees.
 *)
val reachable_decls : Ident.t list -> AST.declaration list -> AST.declaration list

(* Topological sort of declarations
 * The declarations should be acyclic
 *)
val topological_sort : AST.declaration list -> AST.declaration list

val callers : Ident.t list -> AST.declaration list -> IdentSet.t

(****************************************************************)
(** {2 Side effect detection}                                   *)
(****************************************************************)

val side_effects_of_decl  : AST.declaration -> (IdentSet.t * IdentSet.t * IdentSet.t * bool)
val side_effects_of_expr  : AST.expr        -> (IdentSet.t * IdentSet.t * IdentSet.t * bool)
val side_effects_of_lexpr : AST.lexpr       -> (IdentSet.t * IdentSet.t * IdentSet.t * bool)

(* `identify_impure_funs is_const isImpurePrim ds` returns the set of impure functions.
 * That is, functions that
 * - read or write non-constant globals
 * - or call impure primops
 * - or call an impure function.
 *)
val identify_impure_funs : (Ident.t -> bool) -> (Ident.t -> bool) -> AST.declaration list -> IdentSet.t

(****************************************************************)
(** {2 Substitutions}                                           *)
(****************************************************************)

(** Performing variable substitutions in expressions and types

    Note that it does not replace type constructors, global constants
    or enumerations in patterns, array indexes and types so this is
    limited to replacing local variables.
    It also does not replace variables used as l-expressions though
    that it easily changed if we think it should.               *)

val subst_expr : AST.expr Bindings.t -> AST.expr -> AST.expr
val subst_lexpr : AST.expr Bindings.t -> AST.lexpr -> AST.lexpr
val subst_var : AST.expr Bindings.t -> Asl_visitor.access_kind -> Ident.t -> Ident.t
val subst_slice : AST.expr Bindings.t -> AST.slice -> AST.slice
val subst_type : AST.expr Bindings.t -> AST.ty -> AST.ty
val subst_decl_item : AST.expr Bindings.t -> AST.decl_item -> AST.decl_item

(** More flexible substitution class - takes a function instead
    of a binding set.
 *)
class substFunClass : (Ident.t -> AST.expr option) -> Asl_visitor.aslVisitor

val subst_fun_expr : (Ident.t -> AST.expr option) -> AST.expr -> AST.expr
val subst_fun_lexpr : (Ident.t -> AST.expr option) -> AST.lexpr -> AST.lexpr
val subst_fun_slice : (Ident.t -> AST.expr option) -> AST.slice -> AST.slice
val subst_fun_type : (Ident.t -> AST.expr option) -> AST.ty -> AST.ty

(****************************************************************)
(** {2 Expression transformation}                               *)
(****************************************************************)

(** Expression transformation class

    Applies replace function to any subexpression.
    (Especially useful for expressions in types)                *)
class replaceExprClass : (AST.expr -> AST.expr option) -> Asl_visitor.aslVisitor

(****************************************************************)
(** {2 Pretty printing wrappers}                                *)
(****************************************************************)

val pp_unop : AST.unop -> string
val pp_binop : AST.binop -> string
val pp_type : AST.ty -> string
val pp_ixtype : AST.ixtype -> string
val pp_expr : AST.expr -> string
val pp_lexpr : AST.lexpr -> string
val pp_stmt : AST.stmt -> string

(****************************************************************)
(** {2 AST expression constructors}                             *)
(****************************************************************)

val type_unit : AST.ty
val type_integer : AST.ty
val type_bool : AST.ty
val type_real : AST.ty
val type_string : AST.ty
val type_bits : AST.expr -> AST.ty
val type_sintN : AST.expr -> AST.ty

val asl_false : AST.expr
val asl_true : AST.expr

val mk_litint : int -> AST.expr
val mk_litbigint : Z.t -> AST.expr
val mk_litstr : string -> AST.expr

val zero : AST.expr
val one : AST.expr
val two : AST.expr
val empty_bits : AST.expr

(** Construct "!x" *)
val mk_not : AST.expr -> AST.expr

(** Construct "x && y" *)
val mk_and : AST.expr -> AST.expr -> AST.expr

(** Construct "x || y" *)
val mk_or : AST.expr -> AST.expr -> AST.expr

(** Construct "x --> y" *)
val mk_implies : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_enum(x, y)" *)
val mk_eq_enum : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_int(x, y)" *)
val mk_eq_int : AST.expr -> AST.expr -> AST.expr

(** Construct "le_int(x, y)" *)
val mk_le_int : AST.expr -> AST.expr -> AST.expr

(** Construct "add_int(x, y)" *)
val mk_add_int : AST.expr -> AST.expr -> AST.expr

(** Construct "sub_int(x, y)" *)
val mk_sub_int : AST.expr -> AST.expr -> AST.expr

(** Construct "neg_int(x, y)" *)
val mk_neg_int : AST.expr -> AST.expr

(** Construct "mul_int(x, y)" *)
val mk_mul_int : AST.expr -> AST.expr -> AST.expr

(** Construct "pow_int_int(x, y)" *)
val mk_pow_int_int : AST.expr -> AST.expr -> AST.expr

(** Construct "exact_div_int(x, y)" *)
val mk_exact_div_int : AST.expr -> AST.expr -> AST.expr

(** Construct "Max(x, y)" *)
val mk_max_int : AST.expr -> AST.expr -> AST.expr

(** Construct "Min(x, y)" *)
val mk_min_int : AST.expr -> AST.expr -> AST.expr

(** Construct "eq_bits\{w\}(x, y)" *)
val mk_eq_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "in_mask\{w\}(x, y)" *)
val mk_in_mask : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "add_bits\{N\}(x, y)" *)
val mk_add_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "sub_bits\{N\}(x, y)" *)
val mk_sub_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "mul_bits\{N\}(x, y)" *)
val mk_mul_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "and_bits\{N\}(x, y)" *)
val mk_and_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "or_bits\{N\}(x, y)" *)
val mk_or_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "lsr_bits\{N\}(x, y)" *)
val mk_lsr_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "lsl_bits\{N\}(x, y)" *)
val mk_lsl_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "zero_bits\{N\}(N)" *)
val mk_zero_bits : AST.expr -> AST.expr

(** Construct "ones_bits\{N\}(N)" *)
val mk_ones_bits : AST.expr -> AST.expr

(** Construct "asl_extract_bits\{w,n\}(x, lo, w)" *)
val mk_bits_select : AST.expr -> AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "zero_extend_bits\{w, n\}(x, w)" *)
val mk_zero_extend_bits : AST.expr -> AST.expr -> AST.expr -> AST.expr

(** Construct "not_bits\{m\}(n)" *)
val mk_not_bits : AST.expr -> AST.expr -> AST.expr

(** Construct "mk_mask\{n\}()" which is equivalent to "zero_extend_bits\{w, n\}(Ones(w), n)" *)
val mk_mask : AST.expr -> AST.expr -> AST.expr

(** Construct "(0 + x1) + ... + xn" *)
val mk_add_ints : AST.expr list -> AST.expr

(** Construct "(z + x1) + ... + xn" *)
val mk_mul_ints : AST.expr -> AST.expr list -> AST.expr

(** Construct "(TRUE && x1) && ... && xn" *)
val mk_ands : AST.expr list -> AST.expr

(** Construct "(FALSE || x1) || ... || xn" *)
val mk_ors : AST.expr list -> AST.expr

(** Construct "cvt_int_bits\{n\}(x, n)" *)
val mk_cvt_int_bits : AST.expr -> AST.expr -> AST.expr

(****************************************************************)
(** {2 Let expressions}                                         *)
(****************************************************************)

type binding = (Ident.t * AST.ty * AST.expr)

(** Construct nested let-expressions from a list of bindings
 *
 *     mk_let_exprs [(x, tx, ex); (y, ty, ey)] e
 *   =
 *     let x:tx = ex in (let y:ty = ey in e)
 *)
val mk_let_exprs : binding list -> AST.expr -> AST.expr

(** Construct assignments from a list of bindings
 *
 *     mk_assigns loc [(x, tx, ex); (y, ty, ey)]
 *   =
 *     let x : tx = ex;
 *     let y : ty = ey;
 *)
val mk_assigns : Loc.t -> binding list -> AST.stmt list

(****************************************************************)
(** {2 Assert expressions and statements}                       *)
(****************************************************************)

type check = (AST.expr * Loc.t)

(* Construct nested assert-expressions from a list of checks
 *
 *     mk_assert [(x, locx); (y, locy)] e
 *   =
 *     __assert x __in (__assert y in e)
 *)
val mk_assert_exprs : check list -> AST.expr -> AST.expr

(* Construct assertion statements from a list of checks
 *
 *     mk_assert [(x, locx); (y, locy)] e
 *   =
 *     assert x;
 *     assert y;
 *)
val mk_assert_stmts : (AST.expr * Loc.t) list -> AST.stmt list

(****************************************************************)
(** {2 Safe expressions}                                        *)
(****************************************************************)

(** Make an expression safe to replicate by introducing a let-expression
 *  if needed.
 *
 *  An expression is safe to replicate if it has no side effects, doesn't
 *  throw exceptions and is cheap.
 *
 *  This function takes a continuation 'f' that is passed the resulting
 *  safe expression.
 *  - If the expression is already safe to replicate, then return '<f e>'.
 *  - If the expression is not safe to replicate, then return
 *    '__let tmp : ty = e __in <f tmp>'
 *
 *  The nameSupply is used to generate a fresh variable.
 *)
val mk_expr_safe_to_replicate : nameSupply -> AST.expr -> AST.ty -> (AST.expr -> AST.expr) -> AST.expr

val is_safe_to_replicate : AST.expr -> bool

(****************************************************************)
(** {2 Misc}                                                    *)
(****************************************************************)

(** Length of bitstring or mask literal.

    ASL bit and mask literals allow spaces to be included - these
    do not count towards the length of the literal.
 *)
val masklength : string -> int

val masklength_expr : string -> AST.expr

(** Is an expression a literal constant? *)
val is_literal_constant : AST.expr -> bool

(** Test whether a function returns a tuple (of 2 or more elements). *)
val isTupleType : AST.ty -> bool

(** Deconstruct a tuple type (return `[t]` if not a tuple type *)
val tupleTypes : AST.ty -> AST.ty list

(** Bitwidth of type (which is expected to be a bitvector) *)
val width_of_type : AST.ty -> AST.expr option

(** Convert an L-expression to an expression *)
val lexpr_to_expr : AST.lexpr -> AST.expr option

(** Get base type of index type *)
val ixtype_basetype : AST.ixtype -> AST.ty

(** Move function definitions to the end of the list. This is to allow
    calling the functions before they are defined. *)
val hoist_prototypes : AST.declaration list -> AST.declaration list

(****************************************************************
 * End
 ****************************************************************)
