(****************************************************************
 * ASL bitslice transform
 *
 * 1) Simplifies bitslice expressions where the width is not
 *    a literal constant.
 *
 * 2) Transforms bitvector concatenation {w1, .. wn}[ e1, .. en ]
 *    to e1' OR .. en', where for each index i in [1, .. n]
 *    wi' = sum of [wi .. wn]
 *    ei' == zero_extend_bits(ei, total_width) << wi'
 *
 * 3) Transforms some standard library calls
 *
 *    IsZero(e[lo +: wd]) ==> ((e >> lo) AND mask) == Zeros()
 *    IsOnes(e[lo +: wd]) ==> ((NOT e >> lo) AND mask) == Zeros()
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Builtin_idents
open Utils

let is_constant (x : AST.expr) : bool =
  ( match x with
  | Expr_Lit _ -> true
  | _ -> false
  )

let transform_slices : bool ref = ref true

let transform_non_slices (n : AST.expr) (w : AST.expr) (i : AST.expr)
    (x : AST.expr) : AST.expr =
  match x with
  | Expr_TApply (f, _, _, _) when Ident.equal f ones  ->
      mk_lsl_bits n (Asl_utils.mk_mask w n) i
  | Expr_TApply (f, _, _, _) when Ident.equal f zeros -> mk_zero_bits n
  | _ -> mk_lsl_bits n (mk_zero_extend_bits w n x) i

(** Transform expression 'x' of width 'w' to an expression of width 'n'
 * that is equivalent to 'zero_extend_bits(x, n) << i'.
 *
 * This transformation consists of a number of special cases with the
 * aim of avoiding creating intermediate values of width 'm'.
 *
 * (This is useful for transforming concatenations of expressions
 * where 'n' is the overall width of the concatenation and 'i'
 * is the bit-position that 'x' should be placed at.)
 *
 * This function will be extended with additional special cases in
 * the future.
 *)
let transform (loc : Loc.t) (n : AST.expr) (w : AST.expr) (i : AST.expr)
    (x : AST.expr) : AST.expr =
  ( match x with
  | Expr_Slices (_, _, [Slice_HiLo _]) ->
    raise (InternalError
      (loc, "Slice_HiLo not expected", (fun fmt -> Asl_fmt.expr fmt x), __LOC__))
  | Expr_Slices (_, _, [Slice_Single _]) ->
    raise (InternalError
      (loc, "Slice_Single not expected", (fun fmt -> Asl_fmt.expr fmt x), __LOC__))
  | Expr_Slices (Type_Bits (we, _), e, [Slice_LoWd (lo, wd)]) when not (is_constant wd) ->
    (* generate "zero_extend_bits((e >> lo) AND mk_mask(wd, we), n) << i" *)
    let e1 = mk_lsr_bits we e lo in
    let e2 = mk_and_bits we e1 (Asl_utils.mk_mask wd we) in
    let e3 = mk_zero_extend_bits we n e2 in
    mk_lsl_bits n e3 i
  | _ -> transform_non_slices n w i x
  )

  (** Transform assignment
    *   le[shift +: slice_width] = rhs;
    * to
    *   le = (e AND (NOT slice_mask) OR (rhs AND slice_mask)
    * where le : bits(width)
    *       e = le (converted to an expression)
    *       l = location
    *       slice_mask = mask(slice_width) << shift
    *)
let transform_assignment
    (le : AST.lexpr)
    (e : AST.expr)
    (width : AST.expr)
    (slice_width : AST.expr)
    (shift : AST.expr)
    (rhs : AST.expr)
    (l : Loc.t) =
  (* Generate masks for clearing affected bits in slice *)
  let slice_mask = mk_lsl_bits width (Asl_utils.mk_mask slice_width width) shift in
  let slice_not_mask = mk_not_bits width slice_mask in

  (* Transform the rhs. The transformed rhs should already be correctly shifted
   * and masked *)
  let rhs' = transform_non_slices width slice_width shift rhs in

  (* lhs = (lhs AND (NOT slice_mask) OR rhs' *)
  let or_op1 = mk_and_bits width e slice_not_mask in
  let rhs'' = mk_or_bits width or_op1 rhs' in

  Visitor.ChangeDoChildrenPost ([AST.Stmt_Assign (le, rhs'', l)], Fun.id)

let lexpr_to_expr_safe_to_replicate_opt (le : AST.lexpr) : AST.expr option =
  let e_opt = lexpr_to_expr le in
  Option.bind e_opt (fun e -> if is_safe_to_replicate e then Some e else None)

class bitsliceClass =
  object
    inherit Asl_visitor.nopAslVisitor
    val mutable loc = Loc.Unknown

    method! vexpr x =
      ( match x with
      | Expr_Concat (ws, es) ->
        let total_width = Xform_simplify_expr.mk_add_ints ws in
        (* Transform "{w1, .. wn}[ e1, .. en ]" to "e1' OR .. en'"
         *   where, for each index i in [1, .. n]
         *     wi' = sum of [wi .. wn]
         *     ei' == zero_extend_bits(ei, total_width) << wi'
         *)
        let (_, x') = List.fold_right2 (fun w e (i, e0) ->
            let e' = transform loc total_width w i e in
            let i' = Xform_simplify_expr.mk_add_int w i in
            let e0' = mk_or_bits total_width e' e0 in
            (i', e0')
          )
          ws es (zero, mk_zero_bits total_width)
        in
        ChangeDoChildrenPost (x', Fun.id)

      | Expr_TApply (f, [w; n], [e; _], _) when Ident.equal f zero_extend ->
        ChangeDoChildrenPost (transform loc n w zero e, Fun.id)

      (* IsZero(e[lo +: wd]) ==> ((e >> lo) AND mask) == Zeros() *)
      | Expr_TApply (f, [w], [Expr_Slices (ty, e, [Slice_LoWd (lo, wd)])], _)
        when Ident.equal f is_zero && not (is_literal_constant wd)
        ->
          ( match width_of_type ty with
          | Some n ->
              let mask = Asl_utils.mk_mask wd n in
              let e' = mk_and_bits n (mk_lsr_bits n e lo) mask in
              let e'' = mk_eq_bits n e' (mk_zero_bits n) in
              ChangeDoChildrenPost (e'', Fun.id)
          | _ ->
              DoChildren
          )

      (* IsOnes(e[lo +: wd]) ==> ((NOT e >> lo) AND mask) == Zeros() *)
      | Expr_TApply (f, [w], [Expr_Slices (ty, e, [Slice_LoWd (lo, wd)])], _)
        when Ident.equal f is_ones && not (is_literal_constant wd)
        ->
          ( match width_of_type ty with
          | Some n ->
              let mask = Asl_utils.mk_mask wd n in
              let e' = mk_and_bits n (mk_lsr_bits n (mk_not_bits n e) lo) mask in
              let e'' = mk_eq_bits n e' (mk_zero_bits n) in
              ChangeDoChildrenPost (e'', Fun.id)
          | _ ->
              DoChildren
          )

      | _ -> DoChildren
      )

    method! vstmt s =
      loc <- stmt_loc s;

      match s with
      | Stmt_Assign (
          LExpr_Slices (
            _,
            _,
            [Slice_HiLo _]),
          _,
          _) ->
        raise (InternalError
          (loc, "Slice_HiLo not expected", (fun fmt -> Asl_fmt.stmt fmt s), __LOC__))
      | Stmt_Assign (
          LExpr_Slices (
            Type_Bits (Expr_Lit _ as w, _),
            le,
            [Slice_LoWd (lo, sw)]),
          rhs,
          _) when !transform_slices ->
        Option.fold (lexpr_to_expr_safe_to_replicate_opt le)
          ~some:(fun e -> transform_assignment le e w sw lo rhs loc)
          ~none:Visitor.DoChildren
      | _ -> DoChildren

    method! vdecl d =
      loc <- decl_loc d;
      DoChildren
  end

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new bitsliceClass in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new bitsliceClass in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) ss

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new bitsliceClass in
  List.map (Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_bitslices
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  let options =
    Arg.align
      [
        ("--transform",   Arg.Set   transform_slices, " Transform bitslice operations to mask & or operations");
        ("--notransform", Arg.Clear transform_slices, " Do not transform bitslice operations");
      ]
  in
  Commands.registerCommand "xform_bitslices" options [] [] "Transform bitslice operations" cmd

(****************************************************************
 * End
 ****************************************************************)
