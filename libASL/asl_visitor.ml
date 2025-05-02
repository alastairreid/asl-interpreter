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

(****************************************************************)
(** {2 variable access kind}                                    *)
(****************************************************************)

(** This type is used to distinguish between different kinds
 *  of access in the `vvar` method.
 *)
type access_kind
  = Definition (* definition of a variable/constant/function/etc *)
  | Call (* call to subprogram *)
  | Read (* read of local/global constant/variable *)
  | Type (* reference to a type constructor *)
  | Field (* read of a field of a record *)

(****************************************************************)
(** {2 ASL visitor class}                                       *)
(****************************************************************)

(** For each datatype in the ASL AST, a visitor defines what actions
    it wants to perform on values of that type.
 *)

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

(****************************************************************)
(** {2 ASL visitor functions}                                   *)
(****************************************************************)

(** The following set of recursive functions are the ASL specific
    part of the visitor class.
    For each data constructor of each datatype, they invoke visitors
    on each field of the data constructor and then reconstruct
    the corresponding data constructor.

    These functions implement the space-saving optimisation of
    only reconstructing the constructor if the sub-values are
    different.
 *)

let rec visit_exprs (vis : aslVisitor) (xs : expr list) : expr list =
  mapNoCopy (visit_expr vis) xs

and visit_var (vis : aslVisitor) (kind : access_kind) (x : Ident.t) : Ident.t =
  let aux (_ : aslVisitor) (x : Ident.t) : Ident.t = x in
  doVisit vis (vis#vvar kind x) aux x

and visit_lvar (vis : aslVisitor) (x : Ident.t) : Ident.t =
  let aux (_ : aslVisitor) (x : Ident.t) : Ident.t = x in
  doVisit vis (vis#vlvar x) aux x

and visit_e_elsif (vis : aslVisitor) (x : e_elsif) : e_elsif =
  let aux (vis : aslVisitor) (x : e_elsif) : e_elsif =
    match x with
    | E_Elsif_Cond (c, e) ->
        let c' = visit_expr vis c in
        let e' = visit_expr vis e in
        if c == c' && e == e' then x else E_Elsif_Cond (c', e')
  in
  doVisit vis (vis#ve_elsif x) aux x

and visit_slice (vis : aslVisitor) (x : slice) : slice =
  let aux (vis : aslVisitor) (x : slice) : slice =
    match x with
    | Slice_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Slice_Single e'
    | Slice_HiLo (hi, lo) ->
        let hi' = visit_expr vis hi in
        let lo' = visit_expr vis lo in
        if hi == hi' && lo == lo' then x else Slice_HiLo (hi', lo')
    | Slice_LoWd (lo, wd) ->
        let lo' = visit_expr vis lo in
        let wd' = visit_expr vis wd in
        if lo == lo' && wd == wd' then x else Slice_LoWd (lo', wd')
    | Slice_HiWd (hi, wd) ->
        let hi' = visit_expr vis hi in
        let wd' = visit_expr vis wd in
        if hi == hi' && wd == wd' then x else Slice_HiWd (hi', wd')
    | Slice_Element (lo, wd) ->
        let lo' = visit_expr vis lo in
        let wd' = visit_expr vis wd in
        if lo == lo' && wd == wd' then x else Slice_Element (lo', wd')
  in
  doVisit vis (vis#vslice x) aux x

and visit_change_part (vis : aslVisitor) (x : change * expr) : change * expr =
  let (c, e) = x in
  let c' = visit_change vis c in
  let e' = visit_expr vis e in
  if c == c' && e == e' then x else (c', e')

and visit_change (vis : aslVisitor) (x : change) : change =
  let aux (vis : aslVisitor) (x : change) : change =
    ( match x with
    | Change_Field f -> x
    | Change_Slices ss ->
        let ss' = mapNoCopy (visit_slice vis) ss in
        if ss == ss' then x else Change_Slices ss'
    )
  in
  doVisit vis (vis#vchange x) aux x

and visit_patterns (vis : aslVisitor) (xs : pattern list) : pattern list =
  mapNoCopy (visit_pattern vis) xs

and visit_pattern (vis : aslVisitor) (x : pattern) : pattern =
  let aux (vis : aslVisitor) (x : pattern) : pattern =
    match x with
    | Pat_Lit _ -> x
    | Pat_Const c ->
        let c' = visit_var vis Read c in
        if c == c' then x else Pat_Const c'
    | Pat_Wildcard -> x
    | Pat_Tuple ps ->
        let ps' = visit_patterns vis ps in
        if ps == ps' then x else Pat_Tuple ps'
    | Pat_Set ps ->
        let ps' = visit_patterns vis ps in
        if ps == ps' then x else Pat_Set ps'
    | Pat_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Pat_Single e'
    | Pat_Range (lo, hi) ->
        let lo' = visit_expr vis lo in
        let hi' = visit_expr vis hi in
        if lo == lo' && hi == hi' then x else Pat_Range (lo', hi')
  in
  doVisit vis (vis#vpattern x) aux x

and visit_expr (vis : aslVisitor) (x : expr) : expr =
  let aux (vis : aslVisitor) (x : expr) : expr =
    match x with
    | Expr_If (c, t, els, e) ->
        let c' = visit_expr vis c in
        let t' = visit_expr vis t in
        let els' = mapNoCopy (visit_e_elsif vis) els in
        let e' = visit_expr vis e in
        if c == c' && t == t' && els == els' && e == e' then x
        else Expr_If (c', t', els', e')
    | Expr_Let (v, t, e, b) ->
        let v' = visit_var vis Definition v in
        let t' = visit_type vis t in
        let e' = visit_expr vis e in
        let b' = visit_expr vis b in
        if v == v' && t == t' && e == e' && b == b' then x
        else Expr_Let (v', t', e', b')
    | Expr_Assert (e1, e2, loc) ->
        let e1' = visit_expr vis e1 in
        let e2' = visit_expr vis e2 in
        if e1 == e1' && e2 == e2' then x
        else Expr_Assert (e1', e2', loc)
    | Expr_Binop (a, op, b) ->
        let a' = visit_expr vis a in
        let b' = visit_expr vis b in
        if a == a' && b == b' then x else Expr_Binop (a', op, b')
    | Expr_Field (e, f) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Field (e', f)
    | Expr_Fields (e, fs) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Fields (e', fs)
    | Expr_Slices (t, e, ss) ->
        let t' = visit_type vis t in
        let e' = visit_expr vis e in
        let ss' = mapNoCopy (visit_slice vis) ss in
        if t == t' && e == e' && ss == ss' then x else Expr_Slices (t', e', ss')
    | Expr_WithChanges (t, e, cs) ->
        let t' = visit_type vis t in
        let e' = visit_expr vis e in
        let cs' = mapNoCopy (visit_change_part vis) cs in
        if t == t' && e == e' && cs == cs' then x else Expr_WithChanges (t', e', cs')
    | Expr_RecordInit (tc, tes, fas) ->
        let tc' = visit_var vis Type tc in
        let tes' = visit_exprs vis tes in
        let fas' = mapNoCopy (visit_fieldassignment vis) fas in
        if tc == tc' && tes == tes' && fas == fas' then x else Expr_RecordInit (tc', tes', fas')
    | Expr_ArrayInit es ->
        let es' = visit_exprs vis es in
        if es == es' then x else Expr_ArrayInit es'
    | Expr_In (e, p) ->
        let e' = visit_expr vis e in
        let p' = visit_pattern vis p in
        if e == e' && p == p' then x else Expr_In (e', p')
    | Expr_Var v ->
        let v' = visit_var vis Read v in
        if v == v' then x else Expr_Var v'
    | Expr_TApply (f, tes, es, throws) ->
        let f' = visit_var vis Call f in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && tes == tes' && es == es' then x
        else Expr_TApply (f', tes', es', throws)
    | Expr_Tuple es ->
        let es' = visit_exprs vis es in
        if es == es' then x else Expr_Tuple es'
    | Expr_Concat (ws, es) ->
        let ws' = visit_exprs vis ws in
        let es' = visit_exprs vis es in
        if ws == ws' && es == es' then x else Expr_Concat (ws', es')
    | Expr_Unop (op, e) ->
        let e' = visit_expr vis e in
        if e == e' then x else Expr_Unop (op, e')
    | Expr_Unknown t ->
        let t' = visit_type vis t in
        if t == t' then x else Expr_Unknown t'
    | Expr_Array (a, e) ->
        let a' = visit_expr vis a in
        let e' = visit_expr vis e in
        if a == a' && e == e' then x else Expr_Array (a', e')
    | Expr_Lit _ -> x
    | Expr_AsConstraint (e, c) ->
        let e' = visit_expr vis e in
        let c' = visit_constraints vis c in
        if e == e' && c == c' then x else Expr_AsConstraint (e', c')
    | Expr_AsType (e, t) ->
        let e' = visit_expr vis e in
        let t' = visit_type vis t in
        if e == e' && t == t' then x else Expr_AsType (e', t')
  in
  doVisit vis (vis#vexpr x) aux x

and visit_fieldassignment (vis : aslVisitor) (x : Ident.t * expr) : Ident.t * expr =
  match x with
  | f, e ->
      let e' = visit_expr vis e in
      if e == e' then x else (f, e')

and visit_constraint_range (vis : aslVisitor) (x : constraint_range) :
    constraint_range =
  let aux (vis : aslVisitor) (x : constraint_range) : constraint_range =
    match x with
    | Constraint_Single e ->
        let e' = visit_expr vis e in
        if e == e' then x else Constraint_Single e'
    | Constraint_Range (lo, hi) ->
        let lo' = visit_expr vis lo in
        let hi' = visit_expr vis hi in
        if lo == lo' && hi == hi' then x else Constraint_Range (lo', hi')
  in
  doVisit vis (vis#vconstraint x) aux x

and visit_constraints (vis : aslVisitor) (x : constraint_range list) :
    constraint_range list =
  mapNoCopy (visit_constraint_range vis) x

and visit_types (vis : aslVisitor) (xs : ty list) : ty list =
  mapNoCopy (visit_type vis) xs

and visit_type (vis : aslVisitor) (x : ty) : ty =
  let aux (vis : aslVisitor) (x : ty) : ty =
    match x with
    | Type_Integer ocrs ->
        let ocrs' = mapOptionNoCopy (visit_constraints vis) ocrs in
        if ocrs == ocrs' then x else Type_Integer ocrs'
    | Type_Bits (n, fs) ->
        let n' = visit_expr vis n in
        let fs' =
          mapNoCopy
            (fun ((ss, f) as r) ->
              let ss' = mapNoCopy (visit_slice vis) ss in
              if ss == ss' then r else (ss', f))
            fs
        in
        if n == n' && fs == fs' then x else Type_Bits (n', fs')
    | Type_Constructor (tc, es) ->
        let tc' = visit_var vis Type tc in
        let es' = visit_exprs vis es in
        if tc == tc' && es == es' then x else Type_Constructor (tc', es')
    | Type_OfExpr e ->
        let e' = visit_expr vis e in
        if e == e' then x else Type_OfExpr e'
    | Type_Array (Index_Enum tc, ety) ->
        let tc' = visit_var vis Type tc in
        let ety' = visit_type vis ety in
        if tc == tc' && ety == ety' then x else Type_Array (Index_Enum tc', ety')
    | Type_Array (Index_Int sz, ety) ->
        let sz' = visit_expr vis sz in
        let ety' = visit_type vis ety in
        if sz == sz' && ety == ety' then x else Type_Array (Index_Int sz', ety')
    | Type_Tuple tys ->
        let tys' = visit_types vis tys in
        if tys == tys' then x else Type_Tuple tys'
  in
  doVisit vis (vis#vtype x) aux x

let rec visit_lexprs (vis : aslVisitor) (xs : lexpr list) : lexpr list =
  mapNoCopy (visit_lexpr vis) xs

and visit_lexpr (vis : aslVisitor) (x : lexpr) : lexpr =
  let aux (vis : aslVisitor) (x : lexpr) : lexpr =
    match x with
    | LExpr_Wildcard -> x
    | LExpr_Var v ->
        let v' = visit_lvar vis v in
        if v == v' then x else LExpr_Var v'
    | LExpr_Field (e, f) ->
        let e' = visit_lexpr vis e in
        let f' = visit_var vis Field f in
        if e == e' && f == f' then x else LExpr_Field (e', f')
    | LExpr_Fields (e, fs) ->
        let e' = visit_lexpr vis e in
        let fs' = mapNoCopy (visit_var vis Field) fs in
        if e == e' && fs == fs' then x else LExpr_Fields (e', fs')
    | LExpr_Slices (t, e, ss) ->
        let t' = visit_type vis t in
        let e' = visit_lexpr vis e in
        let ss' = mapNoCopy (visit_slice vis) ss in
        if t == t' && e == e' && ss == ss' then x else LExpr_Slices (t', e', ss')
    | LExpr_BitTuple (ws, es) ->
        let ws' = visit_exprs vis ws in
        let es' = mapNoCopy (visit_lexpr vis) es in
        if ws == ws' && es == es' then x else LExpr_BitTuple (ws', es')
    | LExpr_Tuple es ->
        let es' = mapNoCopy (visit_lexpr vis) es in
        if es == es' then x else LExpr_Tuple es'
    | LExpr_Array (a, e) ->
        let a' = visit_lexpr vis a in
        let e' = visit_expr vis e in
        if a == a' && e == e' then x else LExpr_Array (a', e')
    | LExpr_Write (f, tes, es, throws) ->
        let f' = visit_var vis Call f in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && tes == tes' && es == es' then x
        else LExpr_Write (f', tes', es', throws)
    | LExpr_ReadWrite (f, g, tes, es, throws) ->
        let f' = visit_var vis Call f in
        let g' = visit_var vis Call g in
        let tes' = visit_exprs vis tes in
        let es' = visit_exprs vis es in
        if f == f' && g == g' && tes == tes' && es == es' then x
        else LExpr_ReadWrite (f', g', tes', es', throws)
  in
  doVisit vis (vis#vlexpr x) aux x

let with_locals (vis : aslVisitor) (ls : Ident.t list) (f : 'a -> 'b) (x: 'a) : 'b =
  vis#enter_scope ls;
  let result = f x in
  vis#leave_scope ls;
  result

let rec locals_of_declitem (x : decl_item) : Ident.t list =
  match x with
  | DeclItem_Var (v, _) -> [ v ]
  | DeclItem_Tuple dis -> List.concat_map locals_of_declitem dis
  | DeclItem_BitTuple dbs -> List.filter_map (fun (ov, ty) -> ov) dbs
  | DeclItem_Wildcard _ -> []

let locals_of_stmt (x : stmt) : Ident.t list =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) -> vs
  | Stmt_VarDecl (dis, i, loc)
  | Stmt_ConstDecl (dis, i, loc) -> locals_of_declitem dis
  | _ -> []

let visit_decl_bit (vis : aslVisitor) (x : (Ident.t option * ty)) : (Ident.t option * ty) =
  ( match x with
  | (ov, ty) ->
      let ov' = mapOptionNoCopy (visit_lvar vis) ov in
      let ty' = visit_type vis ty in
      if ov == ov' && ty == ty' then x else (ov', ty')
  )

let rec visit_decl_item (vis : aslVisitor) (x : decl_item) : decl_item =
  match x with
  | DeclItem_Var (v, oty) ->
      let v' = visit_lvar vis v in
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      if v == v' && oty == oty' then x else DeclItem_Var (v', oty')
  | DeclItem_Tuple dis ->
      let dis' = mapNoCopy (visit_decl_item vis) dis in
      if dis == dis' then x else DeclItem_Tuple dis'
  | DeclItem_BitTuple dbs ->
      let dbs' = mapNoCopy (visit_decl_bit vis) dbs in
      if dbs == dbs' then x else DeclItem_BitTuple dbs'
  | DeclItem_Wildcard oty ->
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      if oty == oty' then x else DeclItem_Wildcard oty'

(* todo: should probably make this more like cil visitor and allow
 * visit_stmt to generate a list of statements and provide a mechanism to emit
 * statements to be inserted before/after the statement being transformed
 *)
let rec visit_stmts (vis : aslVisitor) (xs : stmt list) : stmt list =
  with_locals vis (List.concat_map locals_of_stmt xs) (mapNoCopyList (visit_stmt vis)) xs

and visit_stmt (vis : aslVisitor) (x : stmt) : stmt list =
  let aux (vis : aslVisitor) (x : stmt) : stmt =
    match x with
    | Stmt_VarDeclsNoInit (vs, ty, loc) ->
        let ty' = visit_type vis ty in
        let vs' = mapNoCopy (visit_lvar vis) vs in
        if ty == ty' && vs == vs' then x else Stmt_VarDeclsNoInit (vs', ty', loc)
    | Stmt_VarDecl (di, i, loc) ->
        let di' = visit_decl_item vis di in
        let i' = visit_expr vis i in
        if di == di' && i == i' then x else Stmt_VarDecl (di', i', loc)
    | Stmt_ConstDecl (di, i, loc) ->
        let di' = visit_decl_item vis di in
        let i' = visit_expr vis i in
        if di == di' && i == i' then x else Stmt_ConstDecl (di', i', loc)
    | Stmt_Assign (l, r, loc) ->
        let l' = visit_lexpr vis l in
        let r' = visit_expr vis r in
        if l == l' && r == r' then x else Stmt_Assign (l', r', loc)
    | Stmt_TCall (f, tes, args, throws, loc) ->
        let f' = visit_var vis Call f in
        let tes' = visit_exprs vis tes in
        let args' = visit_exprs vis args in
        if f == f' && tes == tes' && args == args' then x
        else Stmt_TCall (f', tes', args', throws, loc)
    | Stmt_FunReturn (e, loc) ->
        let e' = visit_expr vis e in
        if e == e' then x else Stmt_FunReturn (e', loc)
    | Stmt_ProcReturn _ -> x
    | Stmt_Assert (e, loc) ->
        let e' = visit_expr vis e in
        if e == e' then x else Stmt_Assert (e', loc)
    | Stmt_Throw (e, loc) ->
        let e' = visit_expr vis e in
        if e == e' then x else Stmt_Throw (e', loc)
    | Stmt_Block (b, loc) ->
        let b' = visit_stmts vis b in
        if b == b' then x else Stmt_Block (b', loc)
    | Stmt_If (c, t, els, (e, el), loc) ->
        let c' = visit_expr vis c in
        let t' = visit_stmts vis t in
        let els' = mapNoCopy (visit_s_elsif vis) els in
        let e' = visit_stmts vis e in
        if c == c' && t == t' && els == els' && e == e' then x
        else Stmt_If (c', t', els', (e', el), loc)
    | Stmt_Case (e, oty, alts, ob, loc) ->
        let e' = visit_expr vis e in
        let oty' = mapOptionNoCopy (visit_type vis) oty in
        let alts' = mapNoCopy (visit_alt vis) alts in
        let ob' = mapOptionNoCopy (fun (b, bl) -> (visit_stmts vis b, bl)) ob in
        if e == e' && oty == oty' && alts == alts' && ob == ob' then x
        else Stmt_Case (e', oty', alts', ob', loc)
    | Stmt_For (v, ty, f, dir, t, b, loc) ->
        let v' = visit_lvar vis v in
        let ty' = visit_type vis ty in
        let f' = visit_expr vis f in
        let t' = visit_expr vis t in
        let b' = with_locals vis [v] (visit_stmts vis) b in
        if v == v' && ty == ty' && f == f' && t == t' && b == b' then x
        else Stmt_For (v', ty', f', dir, t', b', loc)
    | Stmt_While (c, b, loc) ->
        let c' = visit_expr vis c in
        let b' = visit_stmts vis b in
        if c == c' && b == b' then x else Stmt_While (c', b', loc)
    | Stmt_Repeat (b, c, pos, loc) ->
        let b' = visit_stmts vis b in
        let c' = visit_expr vis c in
        if b == b' && c == c' then x else Stmt_Repeat (b', c', pos, loc)
    | Stmt_Try (b, pos, cs, ob, loc) ->
        let b' = visit_stmts vis b in
        let cs' = mapNoCopy (visit_catcher vis) cs in
        let ob' = mapOptionNoCopy (fun (b, bl) -> (visit_stmts vis) b, bl) ob in
        if b == b' && cs == cs' && ob == ob' then x
        else Stmt_Try (b', pos, cs', ob', loc)
  in
  doVisitList vis (vis#vstmt x) aux x

and visit_s_elsif (vis : aslVisitor) (x : s_elsif) : s_elsif =
  let aux (vis : aslVisitor) (x : s_elsif) : s_elsif =
    match x with
    | S_Elsif_Cond (c, b, loc) ->
        let c' = visit_expr vis c in
        let b' = visit_stmts vis b in
        if c == c' && b == b' then x else S_Elsif_Cond (c', b', loc)
  in
  doVisit vis (vis#vs_elsif x) aux x

and visit_alt (vis : aslVisitor) (x : alt) : alt =
  let aux (vis : aslVisitor) (x : alt) : alt =
    match x with
    | Alt_Alt (ps, oc, b, loc) ->
        let ps' = visit_patterns vis ps in
        let oc' = mapOptionNoCopy (visit_expr vis) oc in
        let b' = visit_stmts vis b in
        if ps == ps' && oc == oc' && b == b' then x
        else Alt_Alt (ps', oc', b', loc)
  in
  doVisit vis (vis#valt x) aux x

and visit_catcher (vis : aslVisitor) (x : catcher) : catcher =
  let aux (vis : aslVisitor) (x : catcher) : catcher =
    match x with
    | Catcher_Guarded (v, tc, b, loc) ->
        let v' = visit_var vis Read v in
        let tc' = visit_var vis Type tc in
        let b' = with_locals vis [v] (visit_stmts vis) b in
        if v == v' && tc == tc' && b == b' then x else Catcher_Guarded (v', tc', b', loc)
  in
  doVisit vis (vis#vcatcher x) aux x

let visit_parameter (vis : aslVisitor) (x : Ident.t * ty option) :
    Ident.t * ty option =
  match x with
  | v, oty ->
      let oty' = mapOptionNoCopy (visit_type vis) oty in
      let v' = visit_var vis Definition v in
      if oty == oty' && v == v' then x else (v', oty')

let visit_parameters (vis : aslVisitor) (xs : (Ident.t * ty option) list) :
    (Ident.t * ty option) list =
  mapNoCopy (visit_parameter vis) xs

let visit_arg (vis : aslVisitor) (x : Ident.t * ty) : Ident.t * ty =
  match x with
  | v, ty ->
      let ty' = visit_type vis ty in
      let v' = visit_var vis Definition v in
      if ty == ty' && v == v' then x else (v', ty')

let visit_args (vis : aslVisitor) (xs : (Ident.t * ty) list) : (Ident.t * ty) list =
  mapNoCopy (visit_arg vis) xs

let get_locals (fty : function_type) : Ident.t list =
  let ps = List.map fst fty.parameters in
  let args = List.map fst fty.args in
  let sv = List.map fst (Option.to_list fty.setter_arg) in
  ps @ args @ sv

let visit_funtype (vis : aslVisitor) (locals : Ident.t list) (fty : function_type) : function_type =
  let parameters' = with_locals vis locals (visit_parameters vis) fty.parameters in
  let args' = with_locals vis locals (visit_args vis) fty.args in
  let setter_arg' = mapOptionNoCopy (visit_arg vis) fty.setter_arg in
  let rty' = mapOptionNoCopy (fun ty -> with_locals vis locals (visit_type vis) ty) fty.rty in
  if fty.parameters == parameters'
  && fty.args == args'
  && fty.setter_arg == setter_arg'
  && fty.rty == rty'
  then fty
  else {
    parameters=parameters';
    args=args';
    setter_arg=setter_arg';
    rty=rty';
    use_array_syntax=fty.use_array_syntax;
    is_getter_setter=fty.is_getter_setter;
    throws=fty.throws;
  }

let visit_decl (vis : aslVisitor) (x : declaration) : declaration =
  let aux (vis : aslVisitor) (x : declaration) : declaration =
    match x with
    | Decl_BuiltinType (v, loc) ->
        let v' = visit_var vis Definition v in
        if v == v' then x else Decl_BuiltinType (v', loc)
    | Decl_Forward (v, loc) ->
        let v' = visit_var vis Definition v in
        if v == v' then x else Decl_Forward (v', loc)
    | Decl_Record (v, ps, fs, loc) ->
        let v' = visit_var vis Definition v in
        let ps' = mapNoCopy (visit_var vis Definition) ps in
        let fs' = visit_args vis fs in
        if v == v' && ps == ps' && fs == fs' then x else Decl_Record (v', ps', fs', loc)
    | Decl_Exception (v, fs, loc) ->
        let v' = visit_var vis Definition v in
        let fs' = visit_args vis fs in
        if v == v' && fs == fs' then x else Decl_Exception (v', fs', loc)
    | Decl_Typedef (v, ps, ty, loc) ->
        let v' = visit_var vis Definition v in
        let ps' = mapNoCopy (visit_var vis Definition) ps in
        let ty' = visit_type vis ty in
        if v == v' && ps == ps' && ty == ty' then x else Decl_Typedef (v', ps', ty', loc)
    | Decl_Enum (v, es, loc) ->
        let v' = visit_var vis Definition v in
        let es' = mapNoCopy (visit_var vis Definition) es in
        if v == v' && es == es' then x else Decl_Enum (v', es', loc)
    | Decl_Var (v, ty, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis Definition v in
        if ty == ty' && v == v' then x else Decl_Var (v', ty', loc)
    | Decl_Const (v, oty, e, loc) ->
        let oty' = mapOptionNoCopy (visit_type vis) oty in
        let v' = visit_var vis Definition v in
        let e' = visit_expr vis e in
        if oty == oty' && v == v' && e == e' then x
        else Decl_Const (v', oty', e', loc)
    | Decl_BuiltinFunction (f, fty, loc) ->
        let locals = get_locals fty in
        let f' = visit_var vis Definition f in
        let fty' = visit_funtype vis locals fty in
        if f == f' && fty == fty' then x else Decl_BuiltinFunction (f', fty', loc)
    | Decl_FunType (f, fty, loc) ->
        let locals = get_locals fty in
        let f' = visit_var vis Definition f in
        let fty' = visit_funtype vis locals fty in
        if f == f' && fty == fty' then x else Decl_FunType (f', fty', loc)
    | Decl_FunDefn (f, fty, b, loc) ->
        let locals = get_locals fty in
        let f' = visit_var vis Definition f in
        let fty' = visit_funtype vis locals fty in
        let b' = with_locals vis locals (visit_stmts vis) b in
        if f == f' && fty == fty' && b == b' then x else Decl_FunDefn (f', fty', b', loc)
    | Decl_Operator1 (op, vs, loc) ->
        let vs' = mapNoCopy (visit_var vis Definition) vs in
        if vs == vs' then x else Decl_Operator1 (op, vs', loc)
    | Decl_Operator2 (op, vs, loc) ->
        let vs' = mapNoCopy (visit_var vis Definition) vs in
        if vs == vs' then x else Decl_Operator2 (op, vs', loc)
    | Decl_Config (v, ty, e, loc) ->
        let ty' = visit_type vis ty in
        let v' = visit_var vis Definition v in
        let e' = visit_expr vis e in
        if ty == ty' && v == v' && e == e' then x
        else Decl_Config (v', ty', e', loc)
  in

  doVisit vis (vis#vdecl x) aux x

(****************************************************************)
(** {2 nopAslVisitor class}                                     *)
(****************************************************************)

(** The nopAslVisitor class defines a visitor that recursively
    visits the entire tree making no change.
    In practice, all uses of the visitor framework are based on defining
    a subclass of this type.
 *)

class nopAslVisitor : aslVisitor =
  object
    method vvar (_ : access_kind) (_ : Ident.t) = DoChildren
    method ve_elsif (_ : e_elsif) = DoChildren
    method vslice (_ : slice) = DoChildren
    method vchange (_ : change) = DoChildren
    method vpattern (_ : pattern) = DoChildren
    method vexpr (_ : expr) = DoChildren
    method vconstraint (_ : constraint_range) = DoChildren
    method vtype (_ : ty) = DoChildren
    method vlvar (_ : Ident.t) = DoChildren
    method vlexpr (_ : lexpr) = DoChildren
    method vdeclitem (_ : decl_item) = DoChildren
    method vstmt (_ : stmt) = DoChildren
    method vs_elsif (_ : s_elsif) = DoChildren
    method valt (_ : alt) = DoChildren
    method vcatcher (_ : catcher) = DoChildren
    method vdecl (_ : declaration) = DoChildren
    method enter_scope _ = ()
    method leave_scope _ = ()
  end

(****************************************************************
 * End
 ****************************************************************)
