(****************************************************************
 * Expression simplifier
 *
 * This simplifies expressions such as
 *
 *     x + 1 - x   ==>   1
 *     2 * x - x   ==>   x
 *
 * which makes it easier for constant propagation, etc.
 * to eliminate expressions.
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

(** Simplify an expression by simplifying integer +,-,*,c,v *)
val simplify : AST.expr -> AST.expr

(** Add ints and simplify *)
val mk_add_int : AST.expr -> AST.expr -> AST.expr

(** Add ints and simplify *)
val mk_add_ints : AST.expr list -> AST.expr

(****************************************************************)
(* End                                                          *)
(****************************************************************)
