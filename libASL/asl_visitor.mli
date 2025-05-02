(****************************************************************
 * ASL visitor class
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 *
 * This code follows the pattern used in the cilVisitor class in
 * George Necula's excellent CIL (https://people.eecs.berkeley.edu/~necula/cil/)
 * and makes use of the generic Visitor module that is copied from CIL.
 ****************************************************************)

(** ASL visitor class *)

open Asl_ast
open Visitor

type access_kind
  = Definition
  | Call
  | Read
  | Type
  | Field

class type aslVisitor =
  object
    method vvar : access_kind -> Ident.t -> Ident.t visitAction
    method ve_elsif : e_elsif -> e_elsif visitAction
    method vslice : slice -> slice visitAction
    method vchange : change -> change visitAction
    method vpattern : pattern -> pattern visitAction
    method vexpr : expr -> expr visitAction
    method vconstraint : constraint_range -> constraint_range visitAction
    method vtype : ty -> ty visitAction
    method vlvar : Ident.t -> Ident.t visitAction
    method vlexpr : lexpr -> lexpr visitAction
    method vdeclitem : decl_item -> decl_item visitAction
    method vstmt : stmt -> stmt list visitAction
    method vs_elsif : s_elsif -> s_elsif visitAction
    method valt : alt -> alt visitAction
    method vcatcher : catcher -> catcher visitAction
    method vdecl : declaration -> declaration visitAction
    method enter_scope : Ident.t list -> unit
    method leave_scope : Ident.t list -> unit
  end

val visit_alt : aslVisitor -> alt -> alt
val visit_arg : aslVisitor -> Ident.t * ty -> Ident.t * ty
val visit_args : aslVisitor -> (Ident.t * ty) list -> (Ident.t * ty) list
val visit_catcher : aslVisitor -> catcher -> catcher
val visit_change : aslVisitor -> change -> change
val visit_constraint_range : aslVisitor -> constraint_range -> constraint_range

val visit_constraints :
  aslVisitor -> constraint_range list -> constraint_range list

val visit_decl_item : aslVisitor -> decl_item -> decl_item
val visit_decl : aslVisitor -> declaration -> declaration
val visit_e_elsif : aslVisitor -> e_elsif -> e_elsif
val visit_expr : aslVisitor -> expr -> expr
val visit_exprs : aslVisitor -> expr list -> expr list
val visit_lexpr : aslVisitor -> lexpr -> lexpr
val visit_lexprs : aslVisitor -> lexpr list -> lexpr list
val visit_lvar : aslVisitor -> Ident.t -> Ident.t
val visit_parameter : aslVisitor -> Ident.t * ty option -> Ident.t * ty option

val visit_parameters :
  aslVisitor -> (Ident.t * ty option) list -> (Ident.t * ty option) list

val get_locals : function_type -> Ident.t list
val visit_funtype : aslVisitor -> Ident.t list -> function_type -> function_type
val visit_pattern : aslVisitor -> pattern -> pattern
val visit_patterns : aslVisitor -> pattern list -> pattern list
val visit_s_elsif : aslVisitor -> s_elsif -> s_elsif
val visit_slice : aslVisitor -> slice -> slice
val visit_stmt : aslVisitor -> stmt -> stmt list
val visit_stmts : aslVisitor -> stmt list -> stmt list
val visit_type : aslVisitor -> ty -> ty
val visit_types : aslVisitor -> ty list -> ty list
val visit_var : aslVisitor -> access_kind -> Ident.t -> Ident.t

class nopAslVisitor : aslVisitor
