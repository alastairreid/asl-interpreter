(****************************************************************
 * ASL integer bounds transform
 *
 * Copyright (C) 2025-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 *
 * Transforms any bounded integer expression to use a bounded
 * representation (i.e., the '__sint(N)' type).
 * For example, for "{0..31}" we use "__sint(6)" which can represent
 * {-32 .. 31}.
 *
 * There are two main challenges in implementing this transformation
 * correctly:
 *
 * The main challenge in writing transforms of this kind is that it is
 * fairly easy to generate code that is not type-correct.
 * e.g., to use an expression of type "__sint(5)" as if it had type
 * "integer" or (harder to spot) "__sint(6)".
 *
 * The main trick to avoiding this problem is to make most of the
 * transformations local by preserving the type of most expressions.
 * by wrapping integer arguments in calls to "cvt_int_sintN" and the
 * wrapping an integer result in a call to "cvt_sintN_int".
 * For example, if e1 and e2 are bounded integer expressions, then
 * we transform "add_int(e1, e2)" to
 *
 *     cvt_sintN_int{8}(add_sintN{8}(cvt_int_sintN(e1,8), cvt_int_sintN(e2, 8)))
 *
 * As long as the bitwidth of '8' is large enough, this is obviously
 * a correct transformation.
 *
 * The resulting transformed code will contain many subexpressions of
 * the form
 *
 *     cvt_int_sintN{N}(cvt_sintN_int{M}(x))
 *
 * which can easily be optimized to
 *
 *     resize_sintN{M,N}(x)
 *
 * or just "x" if "M==N".
 *
 * These optimizations are implemented in the constructor functions
 * "mk_resize_sintN", "mk_cvt_int_sintN" and "mk_cvt_sintN_int".
 *
 *
 * The second challenge is that we cannot use this trick everywhere
 * because we want to change the type of variables, constants,
 * function arguments, function results and record fields.
 *
 * For example, when a function argument with a bounded type like
 * "integer {0..31}" is changed to have type "__sint(6)", then we need
 * to consistently change every reference to the function argument in
 * the body of the function and we need to consistently change every
 * call to the function.
 *
 * To ensure that we handle these consistently, when we decide to change
 * the representation, information about the new representation is
 * stored in an environment and every AST node that could reference the
 * transformed variable/constant/... is transformed to use the new
 * representation.
 *
 *
 * Note that this transformation does not introduce any bitwidth
 * polymorphism so it can be run after the monomorphization transform.
 ****************************************************************)

[@@@warning "-26-32-39"]

module AST = Asl_ast
open Asl_utils
open Identset
open Builtin_idents
open Primops
open Utils

(****************************************************************
 * Generating conversion functions
 * todo: move to asl_utils
 *
 * Note that these perform some minor local optimizations to
 * combine adjacent conversions together:
 *
 * - resize(cvt_int_sintN(x,M),N) = cvt_int_sintN(x,N)
 * - resize(resize(x,M),N) = resize(x,N)
 * - cvt_int_sintN(cvt_sintN_int(x), M) = resize(x, M)
 * - cvt_sintN_int(cvt_int_sintN(x, M)) = x
 * - cvt_sintN_int(resize(x, M)) = cvt_sintN_int(x)
 * - todo: optimize resize(5'd8, 16) = resize(16'd8)
 *
 * These are important because the bounded integer transformation
 * generates a lot of these combinations.
 ****************************************************************)

let expr_of_int (x : int) : AST.expr = Expr_Lit (VInt (Z.of_int x))

let expr_of_sintN (n : int) (v : Z.t) : AST.expr = AST.Expr_Lit (VIntN (Primops.mksintN n v))

let mk_resize_sintN (m : int) (n : int) (x : AST.expr) : AST.expr =
  if m = n then (
    x
  ) else (
    (* note: there is no requirement that m <= n *)
    let m' = expr_of_int m in
    let n' = expr_of_int n in
    ( match x with
    | Expr_TApply (f, _, [y; _], _) when Ident.equal f cvt_int_sintN ->
      Expr_TApply (cvt_int_sintN, [n'], [y; n'], NoThrow)
    | Expr_TApply (f, [from;_], [y; _], _) when Ident.equal f resize_sintN ->
      if from = n' then y else Expr_TApply (resize_sintN, [from; n'], [y; n'], NoThrow)
    | _ ->
      Expr_TApply (resize_sintN, [m'; n'], [x; n'], NoThrow)
    )
  )

let mk_cvt_int_sintN (n : int) (x : AST.expr) : AST.expr =
  let n' = expr_of_int n in
  ( match x with
  | Expr_TApply (f, [from], [y], _) when Ident.equal f cvt_sintN_int ->
    if from = n' then y else Expr_TApply (resize_sintN, [from; n'], [y; n'], NoThrow)
  | _ ->
    Expr_TApply (cvt_int_sintN, [n'], [x; n'], NoThrow)
  )

let mk_cvt_sintN_int (n : int) (x : AST.expr) : AST.expr =
  let n' = expr_of_int n in
  ( match x with
  | Expr_TApply (f, _, [y; _], _) when Ident.equal f cvt_int_sintN ->
    y
  | Expr_TApply (f, [from;_], [y; _], _) when Ident.equal f resize_sintN ->
    Expr_TApply (cvt_sintN_int, [from], [y], NoThrow)
  | _ ->
    Expr_TApply (cvt_sintN_int, [n'], [x], NoThrow)
  )

(****************************************************************
 * Integer ranges
 *
 * Machinery to convert bounded types like "integer {0..31}" to
 * ranges like "Some (0, 31)" and to combine ranges like
 * "Some(0, 31)" and "Some(-5, 5)" to "Some(-5, 31)".
 ****************************************************************)

type bounds = Z.t * Z.t

let union_bounds (b1 : bounds) (b2 : bounds) : bounds =
  let (lo1, hi1) = b1 in
  let (lo2, hi2) = b2 in
  (Z.min lo1 lo2, Z.max hi1 hi2)

(* In a for loop "for i = 0 to 10 do ... end", the iterator
 * can have values in the range 0..11 so we need to extend
 * the bounds slightly. This is different for up and down loops
 *)
let extend_bounds (dir : AST.direction) (b : bounds) : bounds =
  let (lo, hi) = b in
  ( match dir with
  | Direction_Up -> (lo, Z.add hi Z.one)
  | Direction_Down -> (Z.sub lo Z.one, hi)
  )

type range = bounds option

let union_range (r1 : range) (r2 : range) : range =
  Option.bind r1 (fun b1 ->
  Option.bind r2 (fun b2 ->
  Some (union_bounds b1 b2)))

(* union of multiple ranges *)
let union_ranges (rs : range list) : range =
  ( match rs with
  | [] -> None
  | (r :: rs) -> List.fold_left union_range r rs
  )

let range_of_sintN (x : int) : range =
  let t = Z.shift_left Z.one (x-1) in
  Some (Z.neg t, Z.sub t Z.one)

let range_of_value (v : Value.value) : range =
  ( match v with
  | VInt i -> Some (i, i)
  | VIntN i -> range_of_sintN i.n
  | _ -> None
  )

let range_of_expr (x : AST.expr) : range =
  ( match x with
  | Expr_Lit v -> range_of_value v
  | Expr_TApply (f, [Expr_Lit (VInt n)], _, _) when Ident.equal f cvt_int_sintN -> range_of_sintN (Z.to_int n)
  | Expr_TApply (f, [Expr_Lit (VInt n)], _, _) when Ident.equal f cvt_sintN_int -> range_of_sintN (Z.to_int n)
  | Expr_TApply (f, [_; Expr_Lit (VInt n)], _, _) when Ident.equal f resize_sintN -> range_of_sintN (Z.to_int n)
  | _ -> None
  )

let range_of_constraint (x : AST.constraint_range) : range =
  ( match x with
  | Constraint_Single y -> range_of_expr y
  | Constraint_Range (lo, hi) -> union_range (range_of_expr lo) (range_of_expr hi)
  )

let range_of_constraints (x : AST.constraint_range list) : range =
  union_ranges (List.map range_of_constraint x)

let range_of_type (x : AST.ty) : range =
  ( match x with
  | Type_Integer (Some crs) -> range_of_constraints crs
  | _ -> None
  )

(****************************************************************
 * Machinery to convert ranges back into types.
 *
 * For example, to convert "Some(0, 31)" to "__sint(6)"
 ****************************************************************)

(* ceiling (log2 x) *)
let size_of_uint (x : Z.t) : int =
  let r = ref 0 in
  let b = ref Z.one in
  (* invariant: b = 2^r *)
  while Z.gt x !b do
    r := !r + 1;
    b := Z.add !b !b
  done;
  (* 2^(r-1) < x <= 2^r *)
  !r

let size_of_sint (x : Z.t) : int =
  if Z.lt x Z.zero then
    1 + size_of_uint (Z.neg x)
  else
    1 + size_of_uint (Z.add x Z.one)

let int_of_bounds (b : bounds) : int =
  let (lo, hi) = b in
  let lo_n = size_of_sint lo in
  let hi_n = size_of_sint hi in
  Int.max lo_n hi_n

let type_of_bounds (b : bounds) : AST.ty = type_sintN (expr_of_int (int_of_bounds b))

let type_of_range (r : range) : AST.ty =
  ( match r with
  | None -> type_integer
  | Some b -> type_of_bounds b
  )

(****************************************************************
 *
 ****************************************************************)

(* If x is a constrained integer, return suitable __sint(N).
 * Otherwise, return x
 *)
let xform_type (x : AST.ty) : AST.ty =
  ( match range_of_type x with
  | None -> x
  | Some b -> type_of_bounds b
  )

let xform_function_type (x : AST.function_type) : AST.function_type =
  assert (Option.is_none x.setter_arg);
  assert (not x.use_array_syntax);
  assert (not x.is_getter_setter);
  {
      parameters = x.parameters;
      args = List.map (fun (v, ty, od) -> (v, xform_type ty, od)) x.args;
      setter_arg = None;
      rty = xform_type x.rty;
      use_array_syntax = x.use_array_syntax;
      is_getter_setter = x.is_getter_setter;
      throws = x.throws;
  }

(* Transform a possibly bounded expression to integer *)
let pack (r : range) (x : AST.expr) : AST.expr =
  ( match r with
  | Some b -> mk_cvt_sintN_int (int_of_bounds b) x
  | None -> x
  )

let unpack (x : AST.expr) (ty : AST.ty) : AST.expr =
  ( match range_of_type ty with
  | Some b -> mk_cvt_int_sintN (int_of_bounds b) x
  | None -> x
  )

(* Convert a possibly bounded expression to required type *)
let cvt_to_range (x : (AST.expr * range)) (rr : range) : AST.expr =
  let (e, rx) = x in
  ( match (rx, rr) with
  | (None, None) -> e
  | (Some b, None) -> mk_cvt_sintN_int (int_of_bounds b) e
  | (None, Some b) -> mk_cvt_int_sintN (int_of_bounds b) e
  | (Some bx, Some br) -> mk_resize_sintN (int_of_bounds bx) (int_of_bounds br) e
  )

(* Convert a possibly bounded expression to required type *)
let cvt_to_type (x : (AST.expr * range)) (ty : AST.ty) : AST.expr = cvt_to_range x (range_of_type ty)

(****************************************************************
 * Analysis and transformation of primitive operations
 *
 * e.g., if x has range {0..5} and y has range {0..10}, then
 * "add_int(x, y)" has range {0..15}.
 ****************************************************************)

(* Bounds of a 1-input contravariant function *)
let bounds_of_cofun1 (f : Z.t -> Z.t) (r1 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  (f hi1, f lo1)

(* Bounds of a 2-input function that is contravariant in 2nd argument *)
let bounds_of_cofun2 (f : Z.t -> Z.t -> Z.t) (r1 : bounds) (r2 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  let (lo2, hi2) = r2 in
  (f lo1 hi2, f hi1 lo2)

(* Bounds of a 2-input covariant function *)
let bounds_of_fun1 (f : Z.t -> Z.t) (r1 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  (f lo1, f hi1)

(* Bounds of a 2-input covariant function *)
let bounds_of_fun2 (f : Z.t -> Z.t -> Z.t) (r1 : bounds) (r2 : bounds) : bounds =
  let (lo1, hi1) = r1 in
  let (lo2, hi2) = r2 in
  (f lo1 lo2, f hi1 hi2)

let range_of_cofun1 (f : Z.t -> Z.t) (r1 : range) : range =
    Option.map (bounds_of_cofun1 f) r1
let range_of_cofun2 (f : Z.t -> Z.t -> Z.t) (r1 : range) (r2 : range) : range =
    map2_option (bounds_of_cofun2 f) r1 r2
let range_of_fun1 (f : Z.t -> Z.t) (r1 : range) : range =
    Option.map (bounds_of_fun1 f) r1
let range_of_fun2 (f : Z.t -> Z.t -> Z.t) (r1 : range) (r2 : range) : range =
    map2_option (bounds_of_fun2 f) r1 r2

let range_of_mul (r1 : range) (r2 : range) : range =
    (* complicated by negative numbers *)
    map2_option
      (fun b1 b2 ->
        let (lo1, hi1) = b1 in
        let (lo2, hi2) = b2 in
        let lo = Z.min (Z.min (Z.mul lo1 lo2) (Z.mul lo1 hi2))
                       (Z.min (Z.mul hi1 lo2) (Z.mul hi1 hi2))
        in
        let hi = Z.max (Z.max (Z.mul lo1 lo2) (Z.mul lo1 hi2))
                       (Z.max (Z.mul hi1 lo2) (Z.mul hi1 hi2))
        in
        (lo, hi))
      r1
      r2

let mk_unop (op : range -> range) (f : Ident.t) (e1 : AST.expr) : AST.expr option =
  let r1 = range_of_expr e1 in
  let rr = op r1 in
  let r = union_range r1 rr in
  Option.map (fun b ->
    let n = int_of_bounds b in
    mk_cvt_sintN_int n (AST.Expr_TApply (f, [expr_of_int n], [mk_cvt_int_sintN n e1], NoThrow))
  ) r

let mk_binop (op : range -> range -> range) (f : Ident.t) (e1 : AST.expr) (e2 : AST.expr) : AST.expr option =
  let r1 = range_of_expr e1 in
  let r2 = range_of_expr e2 in
  let rr = op r1 r2 in
  let r = union_range (union_range r1 r2) rr in
  Option.map (fun b ->
    let n = int_of_bounds b in
    mk_cvt_sintN_int n (AST.Expr_TApply (f, [expr_of_int n], [mk_cvt_int_sintN n e1; mk_cvt_int_sintN n e2], NoThrow))
  ) r

let mk_cmp (f : Ident.t) (e1 : AST.expr) (e2 : AST.expr) : AST.expr option =
  let r1 = range_of_expr e1 in
  let r2 = range_of_expr e2 in
  let r = union_range r1 r2 in
  Option.map (fun b ->
    let n = int_of_bounds b in
    AST.Expr_TApply (f, [expr_of_int n], [mk_cvt_int_sintN n e1; mk_cvt_int_sintN n e2], NoThrow)
  ) r

let primop (f : Ident.t) (ftype : AST.function_type) (ps : AST.expr list) (args : AST.expr list) : AST.expr option =
  ( match (ps, args) with
  | ([],  [x1; x2]) when Ident.equal f add_int -> mk_binop (range_of_fun2 prim_add_int) add_sintN x1 x2
  | ([],  [x1])     when Ident.equal f neg_int -> mk_unop (range_of_cofun1 prim_neg_int) neg_sintN x1
  | ([],  [x1; x2]) when Ident.equal f sub_int -> mk_binop (range_of_cofun2 prim_sub_int) sub_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f mul_int -> mk_binop range_of_mul mul_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f eq_int  -> mk_cmp eq_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f ne_int  -> mk_cmp ne_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f lt_int  -> mk_cmp lt_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f le_int  -> mk_cmp le_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f ge_int  -> mk_cmp ge_sintN x1 x2
  | ([],  [x1; x2]) when Ident.equal f gt_int  -> mk_cmp gt_sintN x1 x2
  | ([Expr_Lit (VInt n') as n], [x])      when Ident.equal f cvt_bits_uint -> Some (mk_cvt_sintN_int (Z.to_int n') (AST.Expr_TApply (cvt_bits_usintN, [n], [x], NoThrow)))
  | ([Expr_Lit (VInt n') as n], [x])      when Ident.equal f cvt_bits_sint -> Some (mk_cvt_sintN_int (Z.to_int n') (AST.Expr_TApply (cvt_bits_ssintN, [n], [x], NoThrow)))
  | _ -> None
  )

(****************************************************************
 * 
 ****************************************************************)

type range_env = bounds Bindings.t

let empty_range_env = Bindings.empty

(****************************************************************
 * Visitor class to transform code to use the sized integer types and
 * operations wherever possible.
 ****************************************************************)

class boundedClass = object (self)
  inherit Asl_visitor.nopAslVisitor

  (* track the *original* function type for each function *)
  val mutable fun_env : AST.function_type Bindings.t = Bindings.empty;

  method add_funtype (f : Ident.t) (fty : AST.function_type) : unit =
    fun_env <- Bindings.add f fty fun_env

  method get_funtype (f : Ident.t) : AST.function_type option =
    Bindings.find_opt f fun_env

  (* global environment *)
  val mutable genv : range_env = empty_range_env;

  method add_grange (v : Ident.t) (r : range) : unit =
    Option.iter (fun b -> genv <- Bindings.add v b genv) r

  (* local environment *)
  val mutable lenv : range_env = empty_range_env;

  method add_lrange (v : Ident.t) (r : range) : unit =
    ( match r with
    | Some b -> lenv <- Bindings.add v b lenv
    | None -> ()
    )

  method sub_lrange (v : Ident.t) : unit =
    lenv <- Bindings.remove v lenv

  method get_range (v : Ident.t) : range =
    orelse_option (Bindings.find_opt v lenv) (fun _ -> Bindings.find_opt v genv)

  method! vexpr x =
    ( match x with
    | Expr_Lit (VInt c)
    -> let n = size_of_sint c in
       ChangeTo (mk_cvt_sintN_int n (expr_of_sintN n c))
    | Expr_Var v
    -> ChangeTo (pack (self#get_range v) x)
    | Expr_Let (v, ty, e1, e2) ->
        ( match range_of_type ty with
        | Some b ->
            let n = int_of_bounds b in
            let ty' = type_sintN (expr_of_int n) in
            let e1' = mk_cvt_int_sintN n e1 in
            self#add_lrange v (Some b);
            let e2' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e2 in
            self#sub_lrange v;
            ChangeTo (Expr_Let (v, ty', e1', e2'))
        | None ->
            DoChildren
        )

    | Expr_TApply (f, ps, args, can_throw) ->
        ( match self#get_funtype f with
        | Some fty ->
            let fty' = xform_function_type fty in
            let args' = List.map (Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor)) args in
            ( match primop f fty' ps args' with
            | Some e -> ChangeTo e
            | None ->
                let arg_tys = List.map (fun (nm, ty, od) -> ty) fty.args in
                let args'' = List.map2 unpack args' arg_tys in
                let x' = AST.Expr_TApply (f, ps, args'', can_throw) in
                ChangeTo (pack (range_of_type fty.rty) x')
            )
        | None ->
            DoChildren
        )

    | Expr_AsConstraint (e, c) ->
        (* Note: we drop the constraint check because the actual check was
         * inserted in the typechecker so there is nothing to do by now
         *)
        let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
        ( match range_of_constraints c with
        | Some b ->
            let n = int_of_bounds b in
            ChangeTo (mk_cvt_sintN_int n (mk_cvt_int_sintN n e'))
        | None ->
            ChangeTo e'
        )

    | Expr_AsType (e, ty) ->
        (* Note: we drop the constraint check because the actual check was
         * inserted in the typechecker so there is nothing to do by now
         *)
        let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
        ( match range_of_type ty with
        | Some b ->
            let n = int_of_bounds b in
            ChangeTo (mk_cvt_sintN_int n (mk_cvt_int_sintN n e'))
        | None ->
            ChangeTo e'
        )

    | _
    -> DoChildren
    )

  method range_of_lexpr (x : AST.lexpr) : range =
    ( match x with
    | LExpr_Var v -> self#get_range v
    | _ -> None
    )

  val mutable return_type : AST.ty = Asl_utils.type_unit

  method! vstmt x =
    ( match x with
    | Stmt_ConstDecl (DeclItem_Var (v, Some ty), e, loc) ->
        ( match range_of_type ty with
        | Some b ->
            let n = int_of_bounds b in
            let ty' = type_sintN (expr_of_int n) in
            let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
            let e'' = mk_cvt_int_sintN n e' in
            self#add_lrange v (Some b);
            ChangeTo [Stmt_ConstDecl (DeclItem_Var (v, Some ty'), e'', loc)]
        | None ->
            DoChildren
        )

    | Stmt_VarDecl (DeclItem_Var (v, Some ty), e, loc) ->
        ( match range_of_type ty with
        | Some b ->
            let n = int_of_bounds b in
            let ty' = xform_type ty in
            let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
            let e'' = mk_cvt_int_sintN n e' in
            self#add_lrange v (Some b);
            ChangeTo [Stmt_VarDecl (DeclItem_Var (v, Some ty'), e'', loc)]
        | None ->
            DoChildren
        )

    | Stmt_VarDeclsNoInit (vs, ty, loc) ->
        let r = range_of_type ty in
        let ty' = xform_type ty in
        List.iter (fun v -> self#add_lrange v r) vs;
        ChangeTo [Stmt_VarDeclsNoInit (vs, ty', loc)]

    | Stmt_Assign (l, r, loc) ->
        ( match self#range_of_lexpr l with
        | Some b ->
            let n = int_of_bounds b in
            let r' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) r in
            let r'' = mk_cvt_int_sintN n r' in
            let l' = Asl_visitor.visit_lexpr (self :> Asl_visitor.aslVisitor) l in
            ChangeTo [Stmt_Assign (l', r'', loc)]
        | None ->
            DoChildren
        )

    | Stmt_For (v, ty, f, dir, t, b, loc) ->
        ( match range_of_type ty with
        | Some bnd ->
            let f' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) f in
            let t' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) t in
            let bnd' = extend_bounds dir bnd in
            let ty' = type_of_bounds bnd' in
            let n = int_of_bounds bnd' in
            let f'' = mk_cvt_int_sintN n f' in
            let t'' = mk_cvt_int_sintN n t' in
            self#add_lrange v (Some bnd);
            let b' = Asl_visitor.visit_stmts (self :> Asl_visitor.aslVisitor) b in
            ChangeTo [Stmt_For (v, ty', f'', dir, t'', b', loc)]
        | None ->
            DoChildren
        )

    | Stmt_TCall (f, ps, args, can_throw, loc) ->
        ( match self#get_funtype f with
        | Some fty ->
            let fty' = xform_function_type fty in
            let args' = List.map (Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor)) args in
            let arg_tys = List.map (fun (nm, ty, od) -> ty) fty.args in
            let args'' = List.map2 unpack args' arg_tys in
            let x' = AST.Stmt_TCall (f, ps, args'', can_throw, loc) in
            ChangeTo [x']
        | None ->
            DoChildren
        )

    | Stmt_Return (e, loc) ->
        let e' = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) e in
        let e'' = unpack e' return_type in
        ChangeTo [Stmt_Return (e'', loc)]

    | _ ->
      DoChildren
    )

  method! vdecl (d : AST.declaration) : AST.declaration Visitor.visitAction =
    let xform_var (v : Ident.t) (ty : AST.ty) (e : AST.expr) : (AST.expr * AST.ty) =
      ( match Bindings.find_opt v genv with
      | Some b ->
          let n = int_of_bounds b in
          let ty' = type_sintN (expr_of_int n) in
          let e' = mk_cvt_int_sintN n e in
          (e', ty')
      | None ->
          (e, ty)
      )
    in

    ( match d with
    | Decl_Const (v, Some ty, e, loc) ->
        let (e', ty') = xform_var v ty e in
        ChangeTo (AST.Decl_Const (v, Some ty', e', loc))
    | Decl_Config (v, ty, e, loc) ->
        let (e', ty') = xform_var v ty e in
        ChangeTo (AST.Decl_Config (v, ty', e', loc))
        (*
    | Decl_Var (v, ty, loc) ->
        let r = Bindings.find_opt v genv in
        let ty' = Option.fold ~none:ty ~some:type_of_bounds r in
        ChangeTo (AST.Decl_Var (v, ty', loc))
    *)

    | Decl_FunType (f, fty, loc) ->
        ( match self#get_funtype f with
        | Some fty' ->
            let fty'' = xform_function_type fty' in
            ChangeTo (Decl_FunType (f, fty'', loc))
        | None ->
            DoChildren
        )

    | Decl_FunDefn (f, fty, b, loc) ->
        ( match self#get_funtype f with
        | Some fty' ->
          let fty'' = xform_function_type fty' in
          lenv <- empty_range_env;
          List.iter (fun (arg, ty, od) -> self#add_lrange arg (range_of_type ty)) fty'.args;
          return_type <- fty'.rty;
          let b' = Asl_visitor.visit_stmts (self :> Asl_visitor.aslVisitor) b in
          lenv <- empty_range_env;
          ChangeTo (Decl_FunDefn (f, fty'', b', loc))
        | None ->
            DoChildren
        )

    | _ -> DoChildren
    )


  method vdecls (ds : AST.declaration list) : AST.declaration list =
    let decls : AST.declaration list ref = ref [] in
    let add (x : AST.declaration) : unit = decls := x :: !decls in

    (* first add transformed types of globals to environment *)
    List.iter (fun d ->
      ( match d with
      | AST.Decl_Const (v, Some ty, _, _) -> self#add_grange v (range_of_type ty)
      | Decl_Config (v, ty, e, loc) -> self#add_grange v (range_of_type ty)
      | Decl_Var (v, ty, loc) -> self#add_grange v (range_of_type ty)
      | Decl_BuiltinFunction (f, fty, loc) -> self#add_funtype f fty
      | Decl_FunType (f, fty, loc) -> self#add_funtype f fty
      | Decl_FunDefn (f, fty, _, loc) -> self#add_funtype f fty
      | _ -> ()
      ))
      ds;

    (* second, tranform each declaration *)
    Visitor.mapNoCopy (Asl_visitor.visit_decl (self :> Asl_visitor.aslVisitor)) ds
end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let xform = new boundedClass in
  xform#vdecls ds

(****************************************************************
 * Command: :xform_bounded
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  let options = [] in
  Commands.registerCommand "xform_bounded" options [] [] "Transform bitslice operations" cmd

(****************************************************************
 * End
 ****************************************************************)
