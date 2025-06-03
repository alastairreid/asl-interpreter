(****************************************************************
 * ASL constant propagation transform
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL constant propagation transform *)

module AST = Asl_ast
module FMTAST = Asl_fmt
open Asl_utils
open AST
open Identset
open Utils
open Builtin_idents

let unroll_loops : bool ref = ref true

module Values = Lattice.Const (struct
  type t = Value.value
  let pp = Value.pp_value
  let equal = Value.eq_value
end)

module Env = struct
  type t = {
    globalConsts : Eval.GlobalEnv.t;
    locals : Values.t ScopeStack.t;
  }

  let pp (fmt : Format.formatter) (env : t) : unit =
    Format.fprintf fmt "globals\n";
    Eval.GlobalEnv.pp fmt env.globalConsts;
    Format.fprintf fmt "locals\n";
    ScopeStack.pp Values.pp_abstract fmt env.locals

  let globals (env : t) : Eval.GlobalEnv.t = env.globalConsts

  let newEnv (genv : Eval.GlobalEnv.t) =
    {
      globalConsts = genv;
      locals = ScopeStack.empty ();
    }

  let nest (env : t) (k : t -> 'a) : 'a =
    ScopeStack.nest env.locals (fun locals' ->
        let env' = {env with locals = locals' } in
        k env')

  let fork_join (env : t) (f : t -> 'a) (g : t -> 'b) : 'a * 'b =
    let env' = {env with locals = ScopeStack.clone env.locals} in
    let a = f env in
    let b = g env' in
    ScopeStack.merge_inplace Values.glb env.locals env'.locals;
    (a, b)

  let rec fixpoint (env : t) (f : t -> 'a) : 'a =
    let env' = {env with locals = ScopeStack.clone env.locals} in
    let orig = {env with locals = ScopeStack.clone env.locals} in
    let a = f env' in
    ScopeStack.merge_inplace Values.glb env.locals env'.locals;
    if ScopeStack.equal Values.equal env.locals orig.locals then
      a
    else
      fixpoint env f

  let to_concrete (env : t) : Eval.Env.t =
    let locals = ScopeStack.filter_map Values.to_concrete env.locals in
    Eval.Env.mkEnv env.globalConsts locals

  let fun_return (env : t) (r : Values.t) : unit =
    ScopeStack.map_inplace (Fun.const Values.top) env.locals

  let throw (env : t) : unit =
    ScopeStack.map_inplace (Fun.const Values.top) env.locals

  let addLocalVar (env : t) (x : Ident.t) (v : Values.t) : unit =
    ScopeStack.add env.locals x v

  let addLocalConst (env : t) (x : Ident.t) (v : Values.t) : unit =
    (* todo: should constants be held separately from local vars? *)
    ScopeStack.add env.locals x v

  let getVar (env : t) (x : Ident.t) : Values.t =
    from_option (ScopeStack.get env.locals x) (fun _ ->
        from_option
          (Option.map Values.singleton
             (Eval.GlobalEnv.get_global_constant env.globalConsts x))
          (fun _ ->
            raise (Value.EvalError (Unknown, "getVar: " ^ Ident.to_string x))))

  let setVar (env : t) (x : Ident.t) (v : Values.t) : unit =
    ignore (ScopeStack.set env.locals x v)

  let mapLocals (env : t) (f : Values.t -> Values.t) : unit =
    ScopeStack.map_inplace f env.locals
end

let mkEnv (genv : Eval.GlobalEnv.t) (values: (Ident.t * Value.value) list) : Env.t =
  let env = Env.newEnv genv in
  List.iter (fun (x, v) -> Env.addLocalConst env x (Values.singleton v)) values;
  env

let isConstant (env : Env.t) (x : expr) : bool =
  match x with
  | Expr_Var v -> Option.is_some (Eval.GlobalEnv.get_global_constant (Env.globals env) v)
  | _ -> Asl_utils.is_literal_constant x

let value_of_constant (env : Env.t) (x : expr) : Value.value option =
  match x with
  | Expr_Lit v -> Some v
  | Expr_Var v -> Eval.GlobalEnv.get_global_constant (Env.globals env) v
  | _ -> None

let expr_value (env : Env.t) (x : AST.expr) : Values.t =
  if isConstant env x then
    let env0 = Eval.Env.newEnv (Env.globals env) in
    Values.singleton (Eval.eval_expr Unknown env0 x)
  else Values.bottom

let rec value_to_expr (x : Value.value) : expr option =
  match x with
  | VBool b -> Some (Expr_Var (if b then true_ident else false_ident))
  | VEnum (v, _) -> Some (Expr_Var v)

  | VInt _
  | VBits _
  | VReal _
  | VString _
  -> Some (Expr_Lit x)

  | VTuple vs ->
      Option.map (fun es -> Expr_Tuple es) (flatten_map_option value_to_expr vs)
  | _ -> None

let value_to_pattern (x : Value.value) : pattern option =
  match x with
  | VBool b -> Some (Pat_Const (if b then true_ident else false_ident))
  | VEnum (v, _) -> Some (Pat_Const v)
  | VInt v -> Some (Pat_Lit x)
  | VBits v -> Some (Pat_Lit x)
  | _ -> None

let algebraic_simplifications (x : expr) : expr =
  match x with
  (* [x, '', y] == [x,y] *)
  (* [] == '' *)
  | Expr_Concat (ws, xs) ->
      let (ws', xs') =
        List.combine ws xs |>
        List.filter (function (_, Expr_Lit (VBits v)) -> v.n <> 0 | _ -> true) |>
        List.split
      in
      if xs' = [] then Asl_utils.empty_bits else Expr_Concat (ws', xs')
  (* '' : x == x == x : '' *)
  | Expr_TApply (i, [ Expr_Lit (VInt v); _ ], [ _; y ], _) when
    v = Z.zero && Ident.equal i append_bits ->
      y
  | Expr_TApply (i, [ _; Expr_Lit (VInt v) ], [ x; _ ], _) when
    v = Z.zero && Ident.equal i append_bits ->
      x
  (* Replicate(x, 0) = '' *)
  | Expr_TApply (i, [ _; Expr_Lit (VInt v) ], _, _) when v = Z.zero ->
      Asl_utils.empty_bits
  (* Replicate(x, 1) = x *)
  | Expr_TApply (i, [ _; Expr_Lit (VInt v) ], [x; _], _) when v = Z.one ->
      x
  (* x + 0 == x == 0 + x *)
  | Expr_TApply (i, [], [ x; Expr_Lit (VInt v) ], _) when v = Z.zero && Ident.equal i add_int -> x
  | Expr_TApply (i, [], [ Expr_Lit (VInt v); x ], _) when v = Z.zero && Ident.equal i add_int -> x
  (* x - 0 == x *)
  | Expr_TApply (i, [], [ x; Expr_Lit (VInt v) ], _) when v = Z.zero && Ident.equal i sub_int -> x
  | _ -> x

(* impure functions: set during initialization *)
let impure_funs : IdentSet.t ref = ref IdentSet.empty

let isPure (x : expr) : bool =
  match x with
  | Expr_TApply (f, _, _, throws) -> throws = NoThrow && not (IdentSet.mem f !impure_funs)
  | _ -> true

class constEvalClass (env : Env.t) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method eval_type (x : ty) : ty = Asl_visitor.visit_type (self :> Asl_visitor.aslVisitor) x
    method eval_expr (x : expr) : expr = Asl_visitor.visit_expr (self :> Asl_visitor.aslVisitor) x
    method eval_slice (x : slice) : slice = Asl_visitor.visit_slice (self :> Asl_visitor.aslVisitor) x

    method! vexpr x =
      match x with
      | Expr_Var v -> (
          let r = try Env.getVar env v with _ -> Values.bottom in
          match Option.bind (Values.to_concrete r) value_to_expr with
          | Some r -> ChangeTo r
          | None -> SkipChildren)
      | Expr_Array (a, i) ->
          let i' = self#eval_expr i in
          ChangeTo (Expr_Array (a, i'))
      | Expr_If (els, e) ->
          let rec xform_if xs =
            ( match xs with
            | [] -> ([], self#eval_expr e)
            | (cond, b) :: xs' ->
                let cond' = self#eval_expr cond in
                if cond' = asl_false then
                  xform_if xs'
                else if cond' = asl_true then
                  ([], self#eval_expr b)
                else
                  let b' = self#eval_expr b in
                  let (els', e') = xform_if xs' in
                  ((cond', b') :: els', e')
            )
          in
          ChangeTo
            ( match xform_if els with
            | ([], e') -> e'
            | (els', e') -> Expr_If(els', e')
            )
      | Expr_Let (v, t, e, b) ->
          let t' = self#eval_type t in
          let e' = self#eval_expr e in
          Env.nest env (fun env' ->
            Env.addLocalConst env' v (expr_value env' e');
            let ce = new constEvalClass env' in
            let b' = Asl_visitor.visit_expr (ce :> Asl_visitor.aslVisitor) b in
            if isConstant env' e' && isConstant env' b' then
              (* Special case: e' can be dropped because it is not used in b'
               * (Testing for both b' and e' being constant is a quick, easy, conservative
               * approximation for e' not having any side-effects and for
               * v not being used in b'.)
               *)
              Visitor.ChangeTo b'
            else
              Visitor.ChangeTo (Expr_Let (v, t', e', b'))
            )
      | Expr_Assert (e1, e2, loc) ->
          let e1' = self#eval_expr e1 in
          let e2' = self#eval_expr e2 in
          if e1' = asl_true then
              Visitor.ChangeTo e2'
          else if e1 == e1' && e2 == e2' then SkipChildren
          else Visitor.ChangeTo (Expr_Assert (e1', e2', loc))
      | Expr_Slices (t, e, ss) ->
          let t' = self#eval_type t in
          let e' = self#eval_expr e in
          let ss' = List.map self#eval_slice ss in
          (* optimization: remove empty slices *)
          let ss'' = List.filter (function (Slice_LoWd(_, w)) -> w <> zero | _ -> true) ss' in
          let r = if Utils.is_empty ss'' then empty_bits else Expr_Slices (t', e', ss'') in
          Visitor.ChangeTo r
      | Expr_TApply (f, [n], [e], NoThrow) when Ident.equal f Builtin_idents.len && isPure e ->
          Visitor.ChangeTo (self#eval_expr n)
      | _ -> (
          try
            let eval (x : expr) : expr =
              if isConstant env x then x
              else if isPure x && List.for_all (isConstant env) (subexprs_of_expr x)
              then
                let env0 = Env.to_concrete env in
                let x' =
                    Option.value
                    (value_to_expr (Eval.eval_expr Unknown env0 x))
                    ~default:x
                in
                (*
                        let fmt = Format.std_formatter in
                        Format.pp_print_string fmt "const: "; Asl_fmt.expr fmt x; Format.pp_print_string fmt " -> "; Asl_fmt.expr fmt x'; Format.pp_print_string fmt "\n";
                        *)
                x'
              else
                (* Format.pp_print_string Format.std_formatter "\nnonconst "; Asl_fmt.expr fmt x; *)
                algebraic_simplifications x
            in
            (*
                let fmt = Format.std_formatter in
                List.iter (fun bs ->
                    List.iter (fun (k, v) ->
                        Format.pp_print_string fmt "\n"; Asl_fmt.varname fmt k
                    ) bs)
                    (ScopeStack.bindings (Env.locals env));
                Eval.Env.pp fmt (Env.to_concrete env);
                *)
            ChangeDoChildrenPost (x, eval)
          with _ -> DoChildren)
  end

let xform_expr (env : Env.t) (x : AST.expr) : AST.expr =
  (* bottom up rewrite of expression substituting constants and evaluating
     constant subexpressions *)
  let ce = new constEvalClass env in
  Asl_visitor.visit_expr (ce :> Asl_visitor.aslVisitor) x

let xform_exprs (env : Env.t) (es : AST.expr list) : AST.expr list =
  List.map (xform_expr env) es

let xform_ty (env : Env.t) (x : AST.ty) : AST.ty =
  let ce = new constEvalClass env in
  Asl_visitor.visit_type (ce :> Asl_visitor.aslVisitor) x

let xform_funtype (env : Env.t) (x : AST.function_type) : AST.function_type =
  let ce = new constEvalClass env in
  let locals = Asl_visitor.get_locals x in
  Asl_visitor.visit_funtype (ce :> Asl_visitor.aslVisitor) locals x

let xform_slice (env : Env.t) (loc : Loc.t) (x : AST.slice) : AST.slice =
  match x with
  | Slice_LoWd (lo, wd) -> Slice_LoWd (xform_expr env lo, xform_expr env wd)
  | Slice_Single _ ->
    raise (InternalError (loc, "Slice_Single not expected", (fun fmt -> Asl_fmt.slice fmt x), __LOC__))
  | Slice_HiLo _ ->
    raise (InternalError (loc, "Slice_HiLo not expected", (fun fmt -> Asl_fmt.slice fmt x), __LOC__))
  | Slice_HiWd _ ->
    raise (InternalError (loc, "Slice_HiWd not expected", (fun fmt -> Asl_fmt.slice fmt x), __LOC__))
  | Slice_Element _ ->
    raise (InternalError (loc, "Slice_Element not expected", (fun fmt -> Asl_fmt.slice fmt x), __LOC__))

(* todo: this combines abstract interpretation with transformation
 * would it be cleaner to just use a visitior to perform the transformation
 * and then this bit just updates environments?
 *)
let rec xform_lexpr (env : Env.t) (loc : Loc.t) (x : AST.lexpr) (r : Values.t) : AST.lexpr =
  match x with
  | LExpr_Wildcard -> x
  | LExpr_Var v ->
      Env.setVar env v r;
      x
  | LExpr_Field(l, f) ->
      let l' = xform_lexpr env loc l Values.bottom in
      LExpr_Field(l', f)
  (*
    | LExpr_Fields(l, fs) ->
            let rec set_fields (i: int) (fs: ident list) (prev: value): value =
                (match fs with
                | [] -> prev
                | (f::fs') ->
                        let p = get_field prev f in (* read previous value to get width *)
                        let w = Primops.prim_length_bits (Value.to_bits p) in
                        let y = extract_bits' r i w in
                        let v' = set_field prev f y in
                        set_fields (i + w) fs' v'
                )
            in
            xform_lexpr_modify env l (set_fields 0 fs)
    *)
  | LExpr_Slices (t, l, ss) ->
      let t' = xform_ty env t in
      let l' = xform_lexpr env loc l Values.bottom in
      let ss' = List.map (xform_slice env loc) ss in
      (* optimization: remove empty slices *)
      let ss'' = List.filter (function (Slice_LoWd(_, w)) -> w <> zero | _ -> true) ss' in
      if Utils.is_empty ss'' then LExpr_Wildcard else LExpr_Slices (t', l', ss'')
  | LExpr_BitTuple (ws, ls) ->
      let ws' = xform_exprs env ws in
      let ls' = List.map (fun l -> xform_lexpr env loc l Values.bottom) ls in
      LExpr_BitTuple(ws', ls')
  | LExpr_Tuple(ls) ->
      let ls' = List.map (fun l -> xform_lexpr env loc l Values.bottom) ls in
      LExpr_Tuple(ls')
  | LExpr_Array (l, i) ->
      let i' = xform_expr env i in
      let l' = xform_lexpr env loc l Values.bottom in
      LExpr_Array (l', i')
  | LExpr_Write (setter, tes, es, throws) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      LExpr_Write (setter, tes', es', throws)
  | LExpr_ReadWrite (getter, setter, tes, es, throws) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      LExpr_ReadWrite (getter, setter, tes', es', throws)
  | _ ->
      let msg = "unexpected lexpr" in
      raise
        (InternalError (loc, msg, (fun fmt -> Asl_fmt.lexpr fmt x), __LOC__))

(** Evaluate pattern match *)
let rec xform_pattern (env : Env.t) (x : AST.pattern) : AST.pattern =
  match x with
  | Pat_Const c -> (
      let r = try Env.getVar env c with _ -> Values.bottom in
      match Option.bind (Values.to_concrete r) value_to_pattern with
      | Some r -> r
      | None -> Pat_Const c
    )
  | Pat_Tuple ps -> Pat_Tuple (List.map (xform_pattern env) ps)
  | Pat_Set ps -> Pat_Set (xform_patterns env ps)
  | Pat_Single e -> Pat_Single (xform_expr env e)
  | Pat_Range (lo, hi) ->
      let lo' = xform_expr env lo in
      let hi' = xform_expr env hi in
      Pat_Range (lo', hi')
  | p -> p

and xform_patterns (env : Env.t) (ps : AST.pattern list) : AST.pattern list =
  List.map (xform_pattern env) ps

and xform_pattern_with_expr (env : Env.t) (p : AST.pattern) (e : AST.expr) : AST.pattern =
  let add_id (id : Ident.t) (v : Value.value) =
    Env.addLocalVar env id (Values.singleton v)
  in
  match (e, p) with
  | (Expr_Tuple rs, Pat_Tuple ps) ->
      Pat_Tuple (List.map2 (xform_pattern_with_expr env) ps rs)
  | (Expr_Var v, Pat_Lit v') ->
      add_id v v';
      p
  | _ -> xform_pattern env p

let rec xform_declitem (env : Env.t) (isConst : bool) (x : AST.decl_item)
    (r : AST.expr option) : AST.decl_item =
  match (x, r) with
  | DeclItem_Var (v, oty), _ ->
      let oty' = Option.map (xform_ty env) oty in
      let r' = Option.map (expr_value env) r in
      Option.iter
        (fun r'' ->
          if isConst then Env.addLocalConst env v r''
          else Env.addLocalVar env v r'')
        r';
      DeclItem_Var (v, oty')
  | DeclItem_Tuple dis, Some (Expr_Tuple rs) ->
      let dis' =
        List.map2 (fun di r -> xform_declitem env isConst di (Some r)) dis rs
      in
      DeclItem_Tuple dis'
  | DeclItem_Tuple dis, _ ->
      let dis' = List.map (fun di -> xform_declitem env isConst di None) dis in
      DeclItem_Tuple dis'
  | DeclItem_BitTuple dbs, _ ->
      let dbs' = List.map (fun (ov, ty) -> (ov, xform_ty env ty)) dbs in
      DeclItem_BitTuple dbs'
  | DeclItem_Wildcard oty, _ ->
      let oty' = Option.map (xform_ty env) oty in
      DeclItem_Wildcard oty'

let rec xform_stmts (env : Env.t) (xs : AST.stmt list) : AST.stmt list =
  Env.nest env (fun env' -> List.concat_map (fun x -> xform_stmt env' x) xs)

(** Evaluate statement *)
and xform_stmt (env : Env.t) (x : AST.stmt) : AST.stmt list =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      let ty' = xform_ty env ty in
      List.iter (fun v -> Env.addLocalVar env v Values.bottom) vs;
      [ Stmt_VarDeclsNoInit (vs, ty', loc) ]
  | Stmt_VarDecl (is_constant, di, i, loc) ->
      let i' = xform_expr env i in
      let di' = xform_declitem env is_constant di (Some i') in
      (* todo: if is_constant, we should always be able to delete this declaration *)
      [ Stmt_VarDecl (is_constant, di', i', loc) ]
  | Stmt_Assign (l, r, loc) ->
      let r' = xform_expr env r in
      let l' = xform_lexpr env loc l (expr_value env r') in
      (* todo: delete assignment if possible *)
      [ Stmt_Assign (l', r', loc) ]
  | Stmt_TCall (f, tes, es, throws, loc) ->
      let tes' = xform_exprs env tes in
      let es' = xform_exprs env es in
      [ Stmt_TCall (f, tes', es', throws, loc) ]
  | Stmt_Return (e, loc) ->
      let e' = xform_expr env e in
      Env.fun_return env (expr_value env e');
      [ Stmt_Return (e', loc) ]
  | Stmt_Assert (e, loc) ->
      let e' = xform_expr env e in
      if e' = asl_true then
          [] (* dead code elimination *)
      else
          (* todo: add 'e' to the environment *)
          [ Stmt_Assert (e', loc) ]
  | Stmt_Throw(e, loc) ->
      let e' = xform_expr env e in
      Env.throw env;
      [ Stmt_Throw (e', loc) ]
  | Stmt_Block (ss, loc) -> [ Stmt_Block (xform_stmts env ss, loc) ]
  | Stmt_If (els, (e, el), loc) ->
      let rec xform env css =
        ( match css with
        | [] -> ([], xform_stmts env e)
        | (c, s, loc) :: css' ->
            (* todo: each branch should assert c or not c *)
            let c' = xform_expr env c in
            if c' = asl_false then begin
              xform env css'
            end else if c' = asl_true then begin
              let s' = xform_stmts env s in
              ([], s')
            end else begin
              let (s', (css'', e')) =
                Env.fork_join env
                  (fun env' -> xform_stmts env' s)
                  (fun env' -> xform env' css')
              in
              ((c', s', loc) :: css'', e')
            end
        )
      in
      ( match xform env els with
      | ([], e') -> e'
      | (els', e') -> [Stmt_If (els', (e', el), loc)]
      )
  | Stmt_Case (e, oty, alts, odefault, loc) ->
      let e' = xform_expr env e in
      let rec xform env alts =
        match alts with
        | [] ->
            let odefault' =
              Option.map (fun (s, loc) -> (xform_stmts env s, loc)) odefault
            in
            ([], odefault')
        | Alt_Alt (ps, oc, s, loc) :: alts' ->
            Env.nest env (fun env' ->
              let (ps', oc', s'), (alts'', odefault') =
                Env.fork_join env'
                  (fun env ->
                    let ps' = match ps with
                    | [p] -> [ xform_pattern_with_expr env p e ]
                    | _   -> xform_patterns env ps
                    in
                    let oc' = Option.map (xform_expr env) oc in
                    let s' = xform_stmts env s in
                    (ps', oc', s'))
                  (fun env -> xform env alts')
              in
              (Alt_Alt (ps', oc', s', loc) :: alts'', odefault'))
      in
      let (alts', odefault') = xform env alts in

      (* perform dead code elimination when the discriminant is a constant *)
      ( match value_of_constant env e' with
      | Some v ->
        (* simplify an alternative based on whether it matches the constant *)
        let env0 = Env.to_concrete env in
        let simplify_alt alt =
          ( match alt with
          | Alt_Alt (ps, oc, s, loc) ->
              let matches = List.exists (Eval.eval_pattern loc env0 v) ps in
              if oc = Some asl_false || not matches then (
                None
              ) else (
                let ps' = if matches then [Pat_Wildcard] else ps in
                let oc' = if oc = Some asl_true then None else oc in
                Some (Alt_Alt (ps', oc', s, loc))
              )
          )
        in
        let alts'' = List.filter_map simplify_alt alts' in

        (* If the first match is a guaranteed match, eliminate all the other (dead) branches *)
        ( match alts'' with
        | [Alt_Alt ([Pat_Wildcard], None, s, loc)] -> s
        | _ -> [ Stmt_Case (e', oty, alts'', odefault', loc) ]
        )
      | _ -> [ Stmt_Case (e', oty, alts', odefault', loc) ]
      )
  | Stmt_For (v, ty, start, dir, stop, b, loc) -> (
      let ty' = xform_ty env ty in
      let start' = xform_expr env start in
      let stop' = xform_expr env stop in
      match (value_of_constant env start', value_of_constant env stop') with
      | Some x, Some y when !unroll_loops ->
          let rec eval (i : Value.value) =
            let c =
              match dir with
              | Direction_Up -> Value.eval_leq loc i y
              | Direction_Down -> Value.eval_leq loc y i
            in
            if c then
              let b' =
                Env.nest env (fun env' ->
                    Env.addLocalConst env' v (Values.singleton i);
                    xform_stmts env' b)
              in
              let i' =
                match dir with
                | Direction_Up -> Value.eval_inc loc i
                | Direction_Down -> Value.eval_dec loc i
              in
              Stmt_Block (b', loc) :: eval i'
            else []
          in
          eval x
      | _ ->
          let b' = Env.fixpoint env  (fun env' ->
              Env.addLocalVar env' v Values.bottom;
              xform_stmts env' b)
          in
          [ Stmt_For (v, ty', start', dir, stop', b', loc) ])
    | Stmt_While(c, b, loc) ->
          let (c', b') = Env.fixpoint env  (fun env' ->
              let c' = xform_expr env' c in
              let b' = xform_stmts env' b in
              (c', b'))
          in
          [ Stmt_While(c', b', loc) ]
    | Stmt_Repeat(b, c, pos, loc) ->
          let (c', b') = Env.fixpoint env  (fun env' ->
              let b' = xform_stmts env' b in
              let c' = xform_expr env' c in
              (c', b'))
          in
          [ Stmt_Repeat(b', c', pos, loc) ]
    | Stmt_Try(tb, pos, catchers, odefault, loc) ->
          let tb' = Env.nest env (fun env' -> xform_stmts env' tb) in

          (* If an exception was thrown, we don't know what variables have
           * been assigned to before the exception. So we forget everything. *)
          Env.mapLocals env (fun _ -> Values.bottom);
          let catchers' = List.map (function Catcher_Guarded (v, tc, b, loc) ->
              Env.nest env (fun env' ->
                Env.addLocalVar env' v Values.bottom;
                let b' = xform_stmts env' b in
                Catcher_Guarded (v, tc, b', loc)
                )
            )
            catchers
          in
          let odefault' = Option.map (fun (s, loc) ->
              Env.nest env (fun env' ->
                let s' = xform_stmts env s in
                (s', loc)
                )
            )
            odefault
          in
          (* We don't know whether an exception was thrown or which
           * catcher executed so we discard all variable values. *)
          Env.mapLocals env (fun _ -> Values.bottom);
          [ Stmt_Try(tb', pos, catchers', odefault', loc) ]
    | Stmt_UCall _
      ->
        raise
          (Error.Unimplemented (Loc.Unknown, "statement", fun fmt -> FMTAST.stmt fmt x))
    )

(** Create local environment and add parameters and arguments *)
let mk_fun_env (env : Eval.GlobalEnv.t) (loc : Loc.t) (f : Ident.t) : Env.t =
  let fun_env = Env.newEnv env in
  ( match Eval.GlobalEnv.get_function env f with
  | Some (tvs, args, _, _) ->
      List.iter (fun tv -> Env.addLocalConst fun_env tv Values.bottom) tvs;
      List.iter (fun v -> Env.addLocalConst fun_env v Values.bottom) args
  | _ ->
      let msg = "undeclared function" in
      raise
        (InternalError (loc, msg, (fun fmt -> Asl_fmt.funname fmt f), __LOC__))
  );
  fun_env

let xform_decl (genv : Eval.GlobalEnv.t) (d : AST.declaration) : AST.declaration
    =
  match d with
  | Decl_Var (v, ty, loc) ->
      let env = Env.newEnv genv in
      let ty' = xform_ty env ty in
      Decl_Var (v, ty', loc)
  | Decl_Const (v, ty, e, loc) ->
      let env = Env.newEnv genv in
      let ty' = Option.map (xform_ty env) ty in
      let e' = xform_expr env e in
      Decl_Const (v, ty', e', loc)
  | Decl_Config (v, ty, e, loc) ->
      let env = Env.newEnv genv in
      let ty' = xform_ty env ty in
      let e' = xform_expr env e in
      Decl_Config (v, ty', e', loc)
  | Decl_FunType (f, fty, loc) ->
      let env = mk_fun_env genv loc f in
      let fty' = xform_funtype env fty in
      Decl_FunType (f, fty', loc)
  | Decl_FunDefn (f, fty, body, loc) ->
      let env = mk_fun_env genv loc f in
      let fty' = xform_funtype env fty in
      let body' = xform_stmts env body in
      Decl_FunDefn (f, fty', body', loc)
  | _ -> d

let xform_decls (genv : Eval.GlobalEnv.t) (ds : AST.declaration list) :
    AST.declaration list =
  let isConstant (v : Ident.t) : bool =
    Option.is_some (Eval.GlobalEnv.get_global_constant genv v)
  in
  let isImpurePrim (v : Ident.t) : bool =
    List.exists (fun name -> Ident.matches v ~name) Value.impure_prims
  in
  impure_funs := identify_impure_funs isConstant isImpurePrim ds;
  List.map (xform_decl genv) ds

(****************************************************************
 * Command: :xform_constprop
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let genv = Eval.build_constant_environment !Commands.declarations in
    Commands.declarations := xform_decls genv !Commands.declarations;
    true
  in
  let options =
    Arg.align
      [
        ("--unroll",   Arg.Set   unroll_loops, " Unroll loops");
        ("--nounroll", Arg.Clear unroll_loops, " Do not unroll loops");
      ]
  in
  Commands.registerCommand "xform_constprop" options [] [] "Perform constant propagation" cmd

(****************************************************************
 * End
 ****************************************************************)
