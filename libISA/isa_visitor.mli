(****************************************************************
 * ISA visitor class
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * This code follows the pattern used in the cilVisitor class in
 * George Necula's excellent CIL (https://people.eecs.berkeley.edu/~necula/cil/)
 * and makes use of the generic Visitor module that is copied from CIL.
 ****************************************************************)

(** ISA visitor class *)

open Isa_ast
open Visitor

type access_kind
  = Definition
  | Call
  | Read
  | Type
  | Field

class type isaVisitor =
  object
    method vvar : access_kind -> Ident.t -> Ident.t visitAction
    method ve_elsif : e_elsif -> e_elsif visitAction
    method vslice : slice -> slice visitAction
    method vchange : change -> change visitAction
    method vpattern : pattern -> pattern visitAction
    method vexpr : expr -> expr visitAction
    method vset : set_range -> set_range visitAction
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

val visit_alt : isaVisitor -> alt -> alt
val visit_arg_no_default : isaVisitor -> Ident.t * ty -> Ident.t * ty
val visit_args_no_default : isaVisitor -> (Ident.t * ty) list -> (Ident.t * ty) list
val visit_arg : isaVisitor -> (Ident.t * ty * expr option) -> (Ident.t * ty * expr option)
val visit_args : isaVisitor -> (Ident.t * ty * expr option) list -> (Ident.t * ty * expr option) list
val visit_catcher : isaVisitor -> catcher -> catcher
val visit_change : isaVisitor -> change -> change
val visit_set_range : isaVisitor -> set_range -> set_range

val visit_sets :
  isaVisitor -> set_range list -> set_range list

val visit_decl_item : isaVisitor -> decl_item -> decl_item
val visit_decl : isaVisitor -> declaration -> declaration
val visit_e_elsif : isaVisitor -> e_elsif -> e_elsif
val visit_expr : isaVisitor -> expr -> expr
val visit_exprs : isaVisitor -> expr list -> expr list
val visit_lexpr : isaVisitor -> lexpr -> lexpr
val visit_lexprs : isaVisitor -> lexpr list -> lexpr list
val visit_lvar : isaVisitor -> Ident.t -> Ident.t
val visit_parameter : isaVisitor -> Ident.t * ty option -> Ident.t * ty option

val visit_parameters :
  isaVisitor -> (Ident.t * ty option) list -> (Ident.t * ty option) list

val get_locals : function_type -> Ident.t list
val visit_funtype : isaVisitor -> Ident.t list -> function_type -> function_type
val visit_pattern : isaVisitor -> pattern -> pattern
val visit_patterns : isaVisitor -> pattern list -> pattern list
val visit_s_elsif : isaVisitor -> s_elsif -> s_elsif
val visit_slice : isaVisitor -> slice -> slice
val visit_stmt : isaVisitor -> stmt -> stmt list
val visit_stmts : isaVisitor -> stmt list -> stmt list
val visit_type : isaVisitor -> ty -> ty
val visit_types : isaVisitor -> ty list -> ty list
val visit_var : isaVisitor -> access_kind -> Ident.t -> Ident.t

class nopIsaVisitor : isaVisitor
