(****************************************************************
 * ISA bitslice transform
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
 *    Is_Zero(e[lo +: wd]) ==> ((e >> lo) AND mask) == Zeros()
 *    Is_Ones(e[lo +: wd]) ==> ((NOT e >> lo) AND mask) == Zeros()
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
open Isa_utils
open Builtin_idents
open Utils

let transform_slices : bool ref = ref true

let rec transform_non_slices (loc : Loc.t) (n : AST.expr) (w : AST.expr) (i : AST.expr) (x : AST.expr) : AST.expr =
  ( match x with
  | Expr_TApply (f, _, _, _) when Ident.equal f ones_bits ->
      mk_lsl_bits n (Isa_utils.mk_mask w n) i
  | Expr_TApply (f, _, _, _) when Ident.equal f zeros_bits ->
      mk_zero_bits n
  | Expr_TApply (f, [ we; _ ], [ e; _ ], _)
    when Ident.equal f sign_extend_bits && not (is_literal_constant w) ->
      let e1 = mk_sign_extend_bits we n e in
      let e2 = mk_and_bits n e1 (Isa_utils.mk_mask w n) in
      mk_lsl_bits n e2 i
  | Expr_TApply (f, [ Expr_Lit (VInt we); _ ], [ e; cnt ], _)
    when Ident.equal f replicate_bits && we = Z.one && not (is_literal_constant cnt) ->
      let e1 = mk_replicate_bits one e n in
      let e2 = mk_and_bits n e1 (Isa_utils.mk_mask w n) in
      mk_lsl_bits n e2 i
  | Expr_Concat (ws, es) ->
      mk_lsl_bits n (transform_concat loc n ws es) i
  | _ ->
      mk_lsl_bits n (mk_zero_extend_bits w n x) i
  )

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
and transform (loc : Loc.t) (n : AST.expr) (w : AST.expr) (i : AST.expr)
    (x : AST.expr) : AST.expr =
  ( match x with
  | Expr_Slices (_, _, [Slice_HiLo _]) ->
    raise (InternalError
      (loc, "Slice_HiLo not expected", (fun fmt -> Isa_fmt.expr fmt x), __LOC__))
  | Expr_Slices (_, _, [Slice_Single _]) ->
    raise (InternalError
      (loc, "Slice_Single not expected", (fun fmt -> Isa_fmt.expr fmt x), __LOC__))
  | Expr_Slices (Type_Bits (we, _), e, [Slice_LoWd (lo, wd)]) when not (is_literal_constant wd) ->
    (* generate "zero_extend_bits((e >> lo) AND mk_mask(wd, we), n) << i" *)
    let e1 = mk_lsr_bits we e lo in
    let e2 = mk_and_bits we e1 (Isa_utils.mk_mask wd we) in
    let e3 = mk_zero_extend_bits we n e2 in
    mk_lsl_bits n e3 i
  | _ -> transform_non_slices loc n w i x
  )

and transform_concat (loc : Loc.t) (final_width : AST.expr) (ws : AST.expr list) (es : AST.expr list) : AST.expr =
  (* Transform "{w1, .. wn}[ e1, .. en ]" to "e1' OR .. en'"
   *   where, for each index i in [1, .. n]
   *     wi' = sum of [wi .. wn]
   *     ei' == zero_extend_bits(ei, final_width) << wi'
   *)
  let (_, r) = List.fold_right2 (fun w e (i, e0) ->
      let e' = transform loc final_width w i e in
      let i' = Xform_simplify_expr.mk_add_int w i in
      let e0' = mk_or_bits final_width e' e0 in
      (i', e0')
    )
    ws es (zero, mk_zero_bits final_width)
  in
  r

  (** Transform assignment
    *   le[shift +: slice_width] = rhs;
    * to
    *   le = (e AND (NOT slice_mask) OR (rhs AND slice_mask)
    * where le : bits(width)
    *       e = le (converted to an expression)
    *       slice_mask = mask(slice_width) << shift
    *)
let transform_assignment
    (le : AST.lexpr)
    (e : AST.expr)
    (width : AST.expr)
    (slice_width : AST.expr)
    (shift : AST.expr)
    (rhs : AST.expr)
    (loc : Loc.t) =
  (* Generate masks for clearing affected bits in slice *)
  let slice_mask = mk_lsl_bits width (Isa_utils.mk_mask slice_width width) shift in
  let slice_not_mask = mk_not_bits width slice_mask in

  (* Transform the rhs. The transformed rhs should already be correctly shifted
   * and masked *)
  let rhs' = transform_non_slices loc width slice_width shift rhs in

  (* lhs = (lhs AND (NOT slice_mask) OR rhs' *)
  let or_op1 = mk_and_bits width e slice_not_mask in
  let rhs'' = mk_or_bits width or_op1 rhs' in

  Visitor.ChangeDoChildrenPost ([AST.Stmt_Assign (le, rhs'', loc)], Fun.id)

let lexpr_to_expr_safe_to_replicate_opt (le : AST.lexpr) : AST.expr option =
  let e_opt = lexpr_to_expr le in
  Option.bind e_opt (fun e -> if is_safe_to_replicate e then Some e else None)

class bitsliceClass =
  object
    inherit Isa_visitor.nopIsaVisitor
    val mutable loc = Loc.Unknown

    method! vexpr x =
      ( match x with
      | Expr_Concat (ws, es) ->
        let total_width = Xform_simplify_expr.mk_add_ints ws in
        let x' = transform_concat loc total_width ws es in
        ChangeDoChildrenPost (x', Fun.id)

      | Expr_TApply (f, [w; n], [e; _], _) when Ident.equal f zero_extend_bits ->
        ChangeDoChildrenPost (transform loc n w zero e, Fun.id)

      (* Is_Zero(e[lo +: wd]) ==> ((e >> lo) AND mask) == Zeros() *)
      | Expr_TApply (f, [w], [Expr_Slices (ty, e, [Slice_LoWd (lo, wd)])], _)
        when Ident.equal f is_zero && not (is_literal_constant wd)
        ->
          ( match width_of_type ty with
          | Some n ->
              let mask = Isa_utils.mk_mask wd n in
              let e' = mk_and_bits n (mk_lsr_bits n e lo) mask in
              let e'' = mk_eq_bits n e' (mk_zero_bits n) in
              ChangeDoChildrenPost (e'', Fun.id)
          | _ ->
              DoChildren
          )

      (* Is_Ones(e[lo +: wd]) ==> ((NOT e >> lo) AND mask) == Zeros() *)
      | Expr_TApply (f, [w], [Expr_Slices (ty, e, [Slice_LoWd (lo, wd)])], _)
        when Ident.equal f is_ones && not (is_literal_constant wd)
        ->
          ( match width_of_type ty with
          | Some n ->
              let mask = Isa_utils.mk_mask wd n in
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
          (loc, "Slice_HiLo not expected", (fun fmt -> Isa_fmt.stmt fmt s), __LOC__))
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
  Isa_visitor.visit_expr (simplify :> Isa_visitor.isaVisitor) x

let xform_stmts (ss : AST.stmt list) : AST.stmt list =
  let simplify = new bitsliceClass in
  Isa_visitor.visit_stmts (simplify :> Isa_visitor.isaVisitor) ss

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let simplify = new bitsliceClass in
  List.map (Isa_visitor.visit_decl (simplify :> Isa_visitor.isaVisitor)) ds

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
