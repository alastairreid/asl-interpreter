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
open Builtin_idents

(****************************************************************)
(** {2 Simplify expressions}                                    *)
(****************************************************************)

(* This simplifies expressions such as
 *
 *     x + 1 - x   ==>   1
 *     2 * x - x   ==>   x
 *
 * which makes it easier for constant propagation, etc.
 * to eliminate expressions.
 *
 * We take care not to reorder any function calls unless they
 * are known to be pure.
 * (And, in this version, we are very pessimistic about
 * whether a function is pure or not.)
 *
 * This operates by
 *
 * 1) Converting combinations of integer additions, subtractions,
 *    negations, multiplications, variables and constants
 *    to a polynomial.
 *
 *    Any subexpressions that do not fit this pattern are
 *    treated as uninterpreted terms in the polynomial.
 *
 * 2) As we construct the polynomial, multiple occurences
 *    of the same monomial (such as "x" or "x * y") are
 *    collected together.
 *
 * 3) Converting the result back to an ASL expression.
 *)

(* A monomial is composed of
 *
 * - either an integer coefficient and a list of integer variables (if this is a pure monomial)
 *
 *   `Right (c, [x, y, z])` represents "c * x * y * z"
 *
 *   (The list of variables is sorted to make comparison simpler)
 *
 * - or an expression (if the expression could not be converted to a polynomial)
 *
 *   `Left e` represents "e"
 *
 * The 'Left' case is used for any subexpressions that
 * cannot be expressed as monomials. That is, expressions
 * that contain something other than +,-,*,c,v.
 *
 * A 'pure' monomial (the Right case) has type 'integer'
 *)
type monomial = (AST.expr, (Z.t * Ident.t list)) Either.t

(** Is a monomonial a pure monomonial or an expression? *)
let is_pure_mono (m : monomial) : bool = Either.is_right m

(** Convert a monomial back to an expression *)
let monomial_to_expr (x : monomial) : AST.expr =
  ( match x with
  | Left e ->
    e
  | Right (c, vs) ->
    List.map (fun v -> AST.Expr_Var v) vs
      |> Asl_utils.mk_mul_ints (Asl_utils.mk_litbigint c)
  )

(** Format a monomonial (for debugging) *)
let fmt_mono (fmt : Format.formatter) (x : monomial) : unit =
  ( match x with
  | Left e ->
    Format.fprintf fmt "'%a'" Asl_fmt.expr e
  | Right (c, vs) ->
    Format.fprintf fmt "%s * [%a]" (Z.to_string c) Asl_fmt.varnames vs
  )

(** Negate a monomial *)
let neg_mono (x : monomial) : monomial =
  ( match x with
  | Left e ->
    Either.Left (Asl_utils.mk_neg_int e)
  | Right (c, vs) ->
    Either.Right (Z.neg c, vs)
  )

(** Multiply two monomials *)
let mul_mono (x1 : monomial) (x2 : monomial) : monomial =
  ( match (x1, x2) with
  | (Right (c1, vs1), Right (c2, vs2)) -> Right (Z.mul c1 c2, List.sort Ident.compare (vs1 @ vs2))
  | (Left e1, _) -> Left (Asl_utils.mk_mul_int e1 (monomial_to_expr x2))
  | (_, Left e2) -> Left (Asl_utils.mk_mul_int (monomial_to_expr x1) e2)
  )

(* A polynomial is a list of monomials
 *
 * A 'pure polynomial' (i.e., consisting only of pure monomials)
 * has type integer.
 *)
type polynomial = monomial list

(* Does a polynomial only contain monomonials *)
let is_pure_poly (p : polynomial) : bool = List.for_all is_pure_mono p

let polynomial_to_expr (p : polynomial) : AST.expr =
  List.map monomial_to_expr p
    |> Asl_utils.mk_add_ints

(** Format a monomonial (for debugging) *)
let fmt_poly (fmt : Format.formatter) (x : polynomial) : unit =
  Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt " + ") fmt_mono fmt x

let mk_poly (c : Z.t) (vs : Ident.t list) : polynomial = [Either.right (c, List.sort Ident.compare vs)]

let mk_poly_from_expr (x : AST.expr) = [Either.left x]

(** add a monomial to a polynomial *)
(* assumes that p is pure *)
let add_poly1 (p : polynomial) (m : monomial) : polynomial =
  ( match m with
  | Left e ->
    p @ [m]
  | Right (c, vs) ->
      let rec insert (ns : monomial list) : monomial list =
        ( match ns with
        | []  -> [m]
        | (Right (k, ws) :: ns') when vs = ws ->
          let k' = Z.add c k in
          if k' = Z.zero then
            ns'
          else
            Right (Z.add c k, vs) :: ns'
        | (n :: ns') ->
          n :: insert ns'
        )
      in
      insert p
  )

(* assumes that p and m are pure *)
let mul_poly1 (p : polynomial) (m : monomial) : polynomial =
  List.map (fun n -> mul_mono n m) p

(** negate a polynomial *)
let neg_poly (p : polynomial) : polynomial =
  List.map neg_mono p

(** add two polynomials *)
let add_poly (p : polynomial) (q : polynomial) : polynomial =
  if is_pure_poly p && is_pure_poly q then begin
    List.fold_left add_poly1 p q
  end else begin
    p @ q
  end

(** subtract two polynomials *)
let sub_poly (p : polynomial) (q : polynomial) : polynomial =
  add_poly p (neg_poly q)

(** multiply two polynomials *)
let mul_poly (p : polynomial) (q : polynomial) : polynomial =
  if is_pure_poly p && is_pure_poly q then begin
    List.fold_left add_poly [] (List.map (mul_poly1 p) q)
  end else begin
    (* To avoid reordering or duplicating expressions, convert back to an expression.
     * This is pessimistic but safe.
     *)
    let e1 = polynomial_to_expr p in
    let e2 = polynomial_to_expr q in
    mk_poly_from_expr (Asl_utils.mk_mul_int e1 e2)
  end

(** Convert an expression to a polynomial *)
let rec to_poly (x : AST.expr) : polynomial =
  ( match x with
  | Expr_Lit (VInt k) -> mk_poly k []
  | Expr_Var v -> mk_poly Z.one [v]
  | Expr_TApply (i, [], [x; y], _) when Ident.equal i add_int ->
    add_poly (to_poly x) (to_poly y)
  | Expr_TApply (i, [], [x; y], _) when Ident.equal i sub_int ->
    sub_poly (to_poly x) (to_poly y)
  | Expr_TApply (i, [], [x], _) when Ident.equal i neg_int ->
    neg_poly (to_poly x)
  | Expr_TApply (i, [], [x; y], _) when Ident.equal i mul_int ->
    mul_poly (to_poly x) (to_poly y)
  | _ -> mk_poly_from_expr x
  )

let verbose = false

(** Simplify an expression by simplifying integer +,-,*,c,v *)
let simplify (x : AST.expr) : AST.expr =
  let p = to_poly x in
  if verbose then Format.fprintf Format.std_formatter "simplified to %a\n" fmt_poly p;
  polynomial_to_expr p

(* Add ints and simplify - this pattern is surprisingly common *)
let mk_add_int (x : AST.expr) (y : AST.expr) : AST.expr =
  simplify (Asl_utils.mk_add_int x y)

(* Add ints and simplify - this pattern is surprisingly common *)
let mk_add_ints (ws : AST.expr list) : AST.expr =
  simplify (Asl_utils.mk_add_ints ws)

(****************************************************************)
(* End                                                          *)
(****************************************************************)
