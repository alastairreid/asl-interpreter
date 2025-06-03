(****************************************************************
 * ASL complex case elimination transform
 *
 * This converts case statements to if statements if they contain
 * complex pattern matches or guards.
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Builtin_idents

let mk_if
    (branches : (AST.expr * AST.stmt list * Loc.t) list)
    (d : (AST.stmt list * Loc.t))
    (loc : Loc.t)
  : AST.stmt list =
  ( match branches with
  | [] -> fst d
  | _ -> [Stmt_If (branches, d, loc)]
  )

let rec match_pattern (loc : Loc.t) (e : AST.expr) (p : AST.pattern) : AST.expr =
  ( match (p, e) with
  | (Pat_Lit (VInt _ as v), _)  -> mk_eq_int e (Expr_Lit v)
  | (Pat_Lit (VBits b as v), _) -> mk_eq_bits (Asl_utils.mk_litint b.n) e (Expr_Lit v)
  | (Pat_Lit (VMask _), _)      -> Expr_In (e, p)
  (* This actually makes the type information invalid because mk_eq_enum will
     just use eq_enum with tag 0 which most likely corresponds to some other
     enum. *)
  | (Pat_Const c,   _) -> mk_eq_enum e (Expr_Var c)
  | (Pat_Wildcard,  _) -> asl_true
  | (Pat_Set ps,    _) -> match_any_pattern loc e ps
  | (Pat_Single e', _) -> mk_eq_int e e'
  | (Pat_Range (lo, hi), _) -> mk_and (mk_le_int lo e) (mk_le_int e hi)
  | (Pat_Tuple ps, Expr_Tuple es) -> mk_ands (List.map2 (match_pattern loc) es ps)
  | _ ->  raise (Error.Unimplemented (loc, "match_pattern", fun fmt -> Asl_fmt.pattern fmt p))
  )

and match_any_pattern (loc : Loc.t) (e : AST.expr) (ps : AST.pattern list) : AST.expr =
  mk_ors (List.map (match_pattern loc e) ps)

let alt_to_branch (e : AST.expr) (x : AST.alt) : (AST.expr * AST.stmt list * Loc.t) =
  ( match x with
  | Alt_Alt (ps, Some c, s, loc) -> (mk_and (match_any_pattern loc e ps) c, s, loc)
  | Alt_Alt (ps, None, s, loc) -> (match_any_pattern loc e ps, s, loc)
  )

(* Detect patterns that can be directly used in
 * C switch statements.
 *)
let is_simple_pattern (x : AST.pattern) : bool =
  ( match x with
  | Pat_Lit (VMask _)
  -> false
  | Pat_Lit (VInt c)
  -> Z.fits_int64 c
  | Pat_Lit (VBits c)
  -> c.n <= 64
  | Pat_Const _
  -> true
  | _
  -> false
  )

(* Detect case alternatives that can be directly
 * converted to C switch statements.
 *)
let is_simple_alt (x : AST.alt) : bool =
  ( match x with
  | Alt_Alt ([p], None, _, _) -> is_simple_pattern p
  | Alt_Alt _ -> false
  )

(* Test whether it is acceptable to duplicate evaluation of an expression.
 * That is, is it something 'cheap' like a variable or a literal constant
 * and does in not contain side effects.
 * The default assumption is false.
 *)
let rec can_duplicate (x : AST.expr) : bool =
  ( match x with
  | Expr_Var _
  | Expr_Lit _
    -> true

  | Expr_Field (e, f) -> can_duplicate e
  | Expr_Tuple es -> List.for_all can_duplicate es

  | _ -> false
  )

class simplifyCaseClass =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method! vexpr s =
      ( match s with
      | Expr_In (e, p) when can_duplicate e ->
        Visitor.ChangeTo (match_pattern Loc.Unknown e p)
      | _ -> DoChildren
      )

    method! vstmt s =
      ( match s with
      | Stmt_Case (e, oty, alts, odefault, loc)
        when can_duplicate e && not (List.for_all is_simple_alt alts) ->
        let branches = List.map (alt_to_branch e) alts in
        let default = match odefault with
          | Some d -> d
          | None ->
            let msg = Loc.to_string loc in
            let f = asl_error_unmatched_case in
            let s = AST.Stmt_TCall (f, [], [Asl_utils.mk_litstr msg], NoThrow, loc) in
            ([s], loc)
        in
        let ss = mk_if branches default loc in
        let ss' = Asl_visitor.visit_stmts (self :> Asl_visitor.aslVisitor) ss in
        Visitor.ChangeTo ss'

      | _ -> DoChildren
      )

  end

let xform_expr (x : AST.expr) : AST.expr =
  let simplify = new simplifyCaseClass in
  Asl_visitor.visit_expr (simplify :> Asl_visitor.aslVisitor) x

let xform_stmts (x : AST.stmt list) : AST.stmt list =
  let simplify = new simplifyCaseClass in
  Asl_visitor.visit_stmts (simplify :> Asl_visitor.aslVisitor) x

let xform_decl (x : AST.declaration) : AST.declaration =
  let simplify = new simplifyCaseClass in
  Asl_visitor.visit_decl (simplify :> Asl_visitor.aslVisitor) x

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  List.map xform_decl ds

(****************************************************************
 * Command: :xform_case
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_case" [] [] [] "Simplify complex case statements" cmd

(****************************************************************
 * End
 ****************************************************************)
