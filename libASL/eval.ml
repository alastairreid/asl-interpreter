(****************************************************************
 * ASL evaluator
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL evaluator *)

module AST = Asl_ast
module FMT = Asl_fmt
open Builtin_idents
open AST
open Utils
open Asl_utils
open Identset
open Value

(****************************************************************
 * Flags to control behaviour (mostly for debugging)
 ****************************************************************)

(** It is an error to have multiple function definitions with conflicting types.
 *  But, for historical reasons, we still allow multiple definitions and later
 *  definitions override earlier definitions.
 *)
let override_conflicts = true

(****************************************************************)
(** {2 Mutable bindings}                                        *)
(****************************************************************)

(** Environment representing immutable global state of the system
 * Note that it is mutable so that we can construct the initial state
 * as each declaration is processed.
 *)
module GlobalEnv = struct
  type t = {
    mutable functions :
      (Ident.t list * Ident.t list * Loc.t * stmt list) Bindings.t;
    mutable enums : value list Bindings.t;
    mutable records : (Ident.t list * (Ident.t * AST.ty) list) Bindings.t;
    mutable typedefs : (Ident.t list * AST.ty) Bindings.t;
    constants : value Scope.t;
  }

  let pp (fmt : Format.formatter) (env : t) : unit = Scope.pp pp_value fmt env.constants

  let empty =
    {
      functions = Bindings.empty;
      enums = Bindings.empty;
      records = Bindings.empty;
      typedefs = Bindings.empty;
      constants = Scope.empty ();
    }

  let addGlobalConst (env : t) (x : Ident.t) (v : value) : unit =
    Scope.set env.constants x v

  let getGlobalConst (env : t) (x : Ident.t) : value =
    match Scope.get env.constants x with
    | Some v -> v
    | None -> failwith "getGlobalConst"

  let get_global_constant (env : t) (x : Ident.t) : value option =
    Scope.get env.constants x

  let addEnum (env : t) (x : Ident.t) (vs : value list) : unit =
    env.enums <- Bindings.add x vs env.enums

  let getEnum (env : t) (x : Ident.t) : value list option =
    Bindings.find_opt x env.enums

  let add_record (env : t) (x : Ident.t) (ps : Ident.t list) (fs : (Ident.t * AST.ty) list) : unit =
    env.records <- Bindings.add x (ps, fs) env.records

  let get_record (env : t) (x : Ident.t) : (Ident.t list * (Ident.t * AST.ty) list) option =
    Bindings.find_opt x env.records

  let addTypedef (env : t) (x : Ident.t) (ps : Ident.t list) (ty : AST.ty) : unit =
    env.typedefs <- Bindings.add x (ps, ty) env.typedefs

  let get_typedef (env : t) (x : Ident.t) : (Ident.t list * AST.ty) option =
    Bindings.find_opt x env.typedefs

  let get_function (env : t) (x : Ident.t) :
      (Ident.t list * Ident.t list * Loc.t * stmt list) option =
    Bindings.find_opt x env.functions

  let addFun (loc : Loc.t) (env : t) (x : Ident.t)
      (def : Ident.t list * Ident.t list * Loc.t * stmt list) : unit =
    if false then Printf.printf "Adding function %s\n" (Ident.to_string x);
    if Bindings.mem x env.functions then
      if true then () (* silently override *)
      else if override_conflicts then
        (* backward compatibility mode: only report a stern warning *)
        Printf.printf
          "Stern warning: %s function %s conflicts with earlier definition - \
           discarding earlier definition\n"
          (Loc.to_string loc) (Ident.to_string x)
      else raise (Error.Ambiguous (loc, "function definition", Ident.to_string x));
    env.functions <- Bindings.add x def env.functions

  let set_config (env : t) (x : Ident.t) (v : value) : unit =
    Scope.set env.constants x v
end

(** Environment representing both global and local state of the system *)
module Env = struct
  type t = {
    globalConsts : GlobalEnv.t;
    globalVars : value Scope.t;
    locals : value ScopeStack.t;
  }

  let mkEnv (genv : GlobalEnv.t) (env : value ScopeStack.t) =
    { globalConsts = genv; globalVars = Scope.empty (); locals = env }

  let newEnv (genv : GlobalEnv.t) = mkEnv genv (ScopeStack.empty ())

  let nestTop (parent : t) (k : t -> 'a) : 'a =
    let child =
      {
        globalConsts = parent.globalConsts;
        globalVars = parent.globalVars;
        locals = ScopeStack.empty () (* only change *);
      }
    in
    k child

  let nest (parent : t) (k : t -> 'a) : 'a =
    ScopeStack.nest parent.locals (fun newenv ->
        let child =
          {
            globalConsts = parent.globalConsts;
            globalVars = parent.globalVars;
            locals = newenv (* only change *);
          }
        in
        k child)

  let globals (env : t) : GlobalEnv.t = env.globalConsts

  let addLocalVar (loc : Loc.t) (env : t) (x : Ident.t) (v : value) : unit =
    let module Tracer = (val (!tracer) : Tracer) in
    Tracer.trace_var ~is_local:true ~is_read:false x v;
    ScopeStack.add env.locals x v

  let addLocalConst (loc : Loc.t) (env : t) (x : Ident.t) (v : value) : unit =
    (* todo: should constants be held separately from local vars? *)
    let module Tracer = (val (!tracer) : Tracer) in
    Tracer.trace_var ~is_local:true ~is_read:false x v;
    ScopeStack.add env.locals x v

  let addGlobalVar (env : t) (x : Ident.t) (v : value) : unit =
    Scope.set env.globalVars x v

  let getVar (loc : Loc.t) (env : t) (x : Ident.t) : value =
    match ScopeStack.get env.locals x with
    | Some r ->
        let module Tracer = (val (!tracer) : Tracer) in
        Tracer.trace_var ~is_local:true ~is_read:true x r;
        r
    | None ->
        match Scope.get env.globalVars x with
        | Some r ->
            let module Tracer = (val (!tracer) : Tracer) in
            Tracer.trace_var ~is_local:false ~is_read:true x r;
            r
        | None ->
            from_option (GlobalEnv.get_global_constant env.globalConsts x)
              (fun _ -> raise (EvalError (loc, "getVar: " ^ Ident.to_string x)))

  let setVar (loc : Loc.t) (env : t) (x : Ident.t) (v : value) : unit =
    if ScopeStack.set env.locals x v then begin
        let module Tracer = (val (!tracer) : Tracer) in
        Tracer.trace_var ~is_local:true ~is_read:false x v
    end else begin
        Scope.set env.globalVars x v;
        let module Tracer = (val (!tracer) : Tracer) in
        Tracer.trace_var ~is_local:false ~is_read:false x v
    end

  let pp (fmt : Format.formatter) (env : t) : unit = ScopeStack.pp pp_value fmt env.locals
end

(****************************************************************)
(** {2 Evaluation functions}                                    *)
(****************************************************************)

(** expand a type definition using type parameters *)
let expand_type (ps : Ident.t list) (ty : AST.ty) (es : expr list) : AST.ty =
  let bs = mk_bindings (List.combine ps es) in
  subst_type bs ty

(** Evaluate list of expressions *)
let rec eval_exprs (loc : Loc.t) (env : Env.t) (xs : AST.expr list) : value list =
  List.map (eval_expr loc env) xs

(** Create uninitialized value at given type

    - For any scalar type, this is the value VUninitialized.
    - For any composite type, all elements are set to uninitialized values

    todo: bitvectors are currently set to UNKNOWN because the bitvector
    representation currently in use cannot track uninitialized bits
 *)
and mk_uninitialized (loc : Loc.t) (env : Env.t) (x : AST.ty) : value =
  match x with
  | Type_Constructor (tc, es) ->
      ( match GlobalEnv.get_record (Env.globals env) tc with
      | Some (ps, fs) ->
          mk_record
            tc
            (List.map (fun (f, ty) -> (f, mk_uninitialized loc env (expand_type ps ty es))) fs)
      | None ->
          ( match GlobalEnv.get_typedef (Env.globals env) tc with
          | Some (ps, ty') -> mk_uninitialized loc env (expand_type ps ty' es)
          | None -> VUninitialized
          )
      )
  | Type_Array (Index_Enum tc, ety) ->
      Value.empty_array (mk_uninitialized loc env ety)
  | Type_Array (Index_Int sz, ety) ->
      Value.empty_array (mk_uninitialized loc env ety)
  | Type_Tuple tys -> VTuple (List.map (mk_uninitialized loc env) tys)
  | Type_Integer _ -> eval_unknown_integer ()
  (* bitvectors and registers should really track whether a bit is initialized
     individually *)
  | Type_Bits (n, _) -> eval_unknown_bits (to_integer loc (eval_expr loc env n))
  | _ -> VUninitialized (* should only be used for scalar types *)

(** Evaluate UNKNOWN at given type *)
and eval_unknown (loc : Loc.t) (env : Env.t) (x : AST.ty) : value =
  match x with
  | Type_Constructor (i, []) when Ident.equal i real_ident -> eval_unknown_real ()
  | Type_Constructor (i, []) when Ident.equal i string_ident ->
      eval_unknown_string ()
  | Type_Constructor (i, [ a ]) when Ident.equal i Builtin_idents.ram ->
      let a' = to_integer loc (eval_expr loc env a) in
      eval_unknown_ram a'
  | Type_Constructor (tc, es) ->
      ( match GlobalEnv.getEnum (Env.globals env) tc with
      | Some (e :: _) -> e
      | Some [] ->
          raise
            (EvalError
               (loc, "eval_unknown unknown type constructor " ^ pp_type x))
      | None -> (
          match GlobalEnv.get_record (Env.globals env) tc with
          | Some (ps, fs) ->
              mk_record
                tc
                (List.map (fun (f, ty) -> (f, eval_unknown loc env (expand_type ps ty es))) fs)
          | None ->
              ( match GlobalEnv.get_typedef (Env.globals env) tc with
              | Some (ps, ty') -> eval_unknown loc env (expand_type ps ty' es)
              | None -> raise (EvalError (loc, "eval_unknown " ^ pp_type x))
              )
          )
      )
  | Type_Integer _ -> eval_unknown_integer ()
  | Type_Bits (n, _) -> eval_unknown_bits (to_integer loc (eval_expr loc env n))
  | Type_OfExpr e -> raise (EvalError (loc, "eval_unknown typeof " ^ pp_type x))
  | Type_Array (Index_Enum tc, ety) ->
      Value.empty_array (eval_unknown loc env ety)
  | Type_Array (Index_Int sz, ety) ->
      Value.empty_array (eval_unknown loc env ety)
  | Type_Tuple tys -> VTuple (List.map (eval_unknown loc env) tys)

(** Evaluate pattern match *)
and eval_pattern (loc : Loc.t) (env : Env.t) (v : value) (x : AST.pattern) : bool =
  match x with
  | Pat_Lit (VInt _ as y) -> eval_eq_int loc v y
  | Pat_Lit (VBits _ as y) -> eval_eq_bits loc v y
  | Pat_Lit (VMask _ as y) -> eval_inmask loc v y
  | Pat_Lit _ ->
      raise (InternalError
        (loc, "eval_pattern: lit", (fun fmt -> FMT.pattern fmt x), __LOC__))
  | Pat_Const c -> eval_eq loc v (GlobalEnv.getGlobalConst (Env.globals env) c)
  | Pat_Wildcard -> true
  | Pat_Tuple ps ->
      let vs = of_tuple loc v in
      List.for_all2 (eval_pattern loc env) vs ps
  | Pat_Set ps -> List.exists (eval_pattern loc env v) ps
  | Pat_Single e ->
      let v' = eval_expr loc env e in
      eval_eq loc v v'
  | Pat_Range (lo, hi) ->
      let lo' = eval_expr loc env lo in
      let hi' = eval_expr loc env hi in
      eval_leq loc lo' v && eval_leq loc v hi'

(** Evaluate bitslice bounds *)
and eval_slice (loc : Loc.t) (env : Env.t) (x : AST.slice) : value * value =
  match x with
  | Slice_Single i ->
      let i' = eval_expr loc env i in
      (i', VInt Z.one)
  | Slice_HiLo (hi, lo) ->
      let hi' = eval_expr loc env hi in
      let lo' = eval_expr loc env lo in
      let wd' = eval_add_int loc (eval_sub_int loc hi' lo') (VInt Z.one) in
      (lo', wd')
  | Slice_LoWd (lo, wd) ->
      let lo' = eval_expr loc env lo in
      let wd' = eval_expr loc env wd in
      (lo', wd')
  | Slice_Element (lo, wd) ->
      let wd' = eval_expr loc env wd in
      let lo' = eval_mul_int loc (eval_expr loc env lo) wd' in
      (lo', wd')

(** Evaluate expression *)
and eval_expr (loc : Loc.t) (env : Env.t) (x : AST.expr) : value =
  match x with
  | Expr_If (c, t, els, e) ->
      let rec eval_if xs d =
        match xs with
        | [] -> eval_expr loc env d
        | AST.E_Elsif_Cond (cond, b) :: xs' ->
            if to_bool loc (eval_expr loc env cond) then eval_expr loc env b
            else eval_if xs' d
      in
      eval_if (E_Elsif_Cond (c, t) :: els) e
  | Expr_Let (v, t, e, b) ->
      Env.nest env (fun env' ->
        let e' = eval_expr loc env e in
        Env.addLocalConst loc env v e';
        eval_expr loc env b
      )
  | Expr_Assert (e1, e2, loc) ->
      if not (to_bool loc (eval_expr loc env e1)) then begin
        raise (EvalError (loc, "assertion failure"));
      end;
      eval_expr loc env e2
  | Expr_Binop (a, op, b) ->
      raise
        (EvalError
           ( loc,
             "binary operation should have been removed in expression "
             ^ pp_expr x ))
  | Expr_Field (e, f) -> get_field loc (eval_expr loc env e) f
  | Expr_Fields (e, fs) ->
      let v = eval_expr loc env e in
      let vs = List.map (get_field loc v) fs in
      eval_concat loc vs
  | Expr_Slices (_, e, ss) ->
      let v = eval_expr loc env e in
      let vs =
        List.map
          (fun s ->
            let i, w = eval_slice loc env s in
            extract_bits'' loc v i w)
          ss
      in
      eval_concat loc vs
  | Expr_RecordInit (tc, _, fas) ->
      mk_record tc (List.map (fun (f, e) -> (f, eval_expr loc env e)) fas)
  | Expr_ArrayInit es ->
      let inits = List.mapi (fun i e -> (i, eval_expr loc env e)) es in
      init_array inits VUninitialized
  | Expr_In (e, p) -> from_bool (eval_pattern loc env (eval_expr loc env e) p)
  | Expr_Var v -> Env.getVar loc env v
  | Expr_TApply (f, tes, es, _) ->
      (* First deal with &&, || and IMPLIES all of which only evaluate
       * their second argument if they need to
       *)
      if Ident.equal f and_bool then
        match (tes, es) with
        | [], [ x; y ] ->
            if to_bool loc (eval_expr loc env x) then eval_expr loc env y
            else from_bool false
        | _ ->
            raise
              (EvalError (loc, "malformed and_bool expression " ^ pp_expr x))
      else if Ident.equal f or_bool then
        match (tes, es) with
        | [], [ x; y ] ->
            if to_bool loc (eval_expr loc env x) then from_bool true
            else eval_expr loc env y
        | _ ->
            raise (EvalError (loc, "malformed or_bool expression " ^ pp_expr x))
      else if Ident.equal f implies_bool then
        match (tes, es) with
        | [], [ x; y ] ->
            if to_bool loc (eval_expr loc env x) then eval_expr loc env y
            else from_bool true
        | _ ->
            raise
              (EvalError (loc, "malformed implies_bool expression " ^ pp_expr x))
      else
        let tvs = eval_exprs loc env tes in
        let vs = eval_exprs loc env es in
        eval_funcall loc env f tvs vs
  | Expr_Tuple es ->
      let vs = List.map (eval_expr loc env) es in
      VTuple vs
  | Expr_Concat (_, es) ->
      let vs = List.map (eval_expr loc env) es in
      eval_concat loc vs
  | Expr_Unop (op, e) ->
      raise (EvalError (loc, "unary operation should have been removed"))
  | Expr_Unknown t -> eval_unknown loc env t
  | Expr_Array (a, i) ->
      let a' = eval_expr loc env a in
      let i' = eval_expr loc env i in
      get_array loc a' i'
  | Expr_Lit v -> v
  | Expr_AsConstraint (e, c) ->
      (* todo: dynamic constraint check *)
      eval_expr loc env e
  | Expr_AsType (e, t) ->
      (* todo: dynamic type/constraint check *)
      eval_expr loc env e

(** Evaluate L-expression in write-mode (i.e., this is not a read-modify-write) *)
and eval_lexpr (loc : Loc.t) (env : Env.t) (x : AST.lexpr) (r : value) : unit =
  match x with
  | LExpr_Wildcard -> ()
  | LExpr_Var v -> Env.setVar loc env v r
  | LExpr_Field (l, f) ->
      eval_lexpr_modify loc env l (fun prev -> set_field loc prev f r)
  | LExpr_Fields (l, fs) ->
      let rec set_fields (i : int) (fs : Ident.t list) (prev : value) : value =
        match fs with
        | [] -> prev
        | f :: fs' ->
            let p = get_field loc prev f in
            (* read previous value to get width *)
            let w = Primops.prim_length_bits (Value.to_bits loc p) in
            let y = extract_bits' loc r i w in
            let v' = set_field loc prev f y in
            set_fields (i + w) fs' v'
      in
      eval_lexpr_modify loc env l (set_fields 0 fs)
  | LExpr_Slices (_, l, ss) ->
      let rec eval (o : value) (ss' : AST.slice list) (prev : value) : value =
        match ss' with
        | [] -> prev
        | s :: ss ->
            let i, w = eval_slice loc env s in
            let v = extract_bits'' loc r o w in
            eval (eval_add_int loc o w) ss (insert_bits loc prev i w v)
      in
      eval_lexpr_modify loc env l (eval (VInt Z.zero) (List.rev ss))
  | LExpr_BitTuple (ws, ls) ->
    let _ = List.fold_right (fun ((lexpr, width_expr) : lexpr * expr)
        (idx : int) ->
      let width = eval_expr loc env width_expr |> to_int loc in
      let r' = extract_bits' loc r idx width in
      eval_lexpr loc env lexpr r';
      idx + width
    ) (List.combine ls ws) 0 in
    ()
  | LExpr_Tuple ls ->
      let rs = of_tuple loc r in
      List.iter2 (eval_lexpr loc env) ls rs
  | LExpr_Array (l, i) ->
      let i' = eval_expr loc env i in
      eval_lexpr_modify loc env l (fun prev -> set_array loc prev i' r)
  | LExpr_Write (setter, tes, es, _) ->
      let tvs = eval_exprs loc env tes in
      let vs = eval_exprs loc env es in
      eval_proccall loc env setter tvs (vs @ [ r ])
  | _ -> failwith ("eval_lexpr: " ^ pp_lexpr x)

(** Evaluate L-expression in read-modify-write mode.

    1. The old value of the L-expression is read.
    2. The function 'modify' is applied to the old value
    3. The result is written back to the L-expression.
 *)
and eval_lexpr_modify (loc : Loc.t) (env : Env.t) (x : AST.lexpr)
    (modify : value -> value) : unit =
  match x with
  | LExpr_Var v -> Env.setVar loc env v (modify (Env.getVar loc env v))
  | LExpr_Field (l, f) ->
      let modify' (prev : value) : value =
        let old = get_field loc prev f in
        set_field loc prev f (modify old)
      in
      eval_lexpr_modify loc env l modify'
  | LExpr_Array (l, i) ->
      let i' = eval_expr loc env i in
      let modify' (prev : value) : value =
        let old = get_array loc prev i' in
        set_array loc prev i' (modify old)
      in
      eval_lexpr_modify loc env l modify'
  | LExpr_ReadWrite (getter, setter, tes, es, _) ->
      let tvs = eval_exprs loc env tes in
      let vs = eval_exprs loc env es in
      let old = eval_funcall loc env getter tvs vs in
      eval_proccall loc env setter tvs (vs @ [ modify old ])
  | _ -> failwith "eval_lexpr_modify"

and add_decl_item_vars (loc : Loc.t) (env : Env.t) (is_const : bool) (x : AST.decl_item) (i : value) : unit =
  match (x, i) with
  | (DeclItem_Var (v, _), i) ->
      if is_const then
        Env.addLocalConst loc env v i
      else
        Env.addLocalVar loc env v i
  | (DeclItem_Tuple dis, VTuple is) ->
      List.iter2 (add_decl_item_vars loc env is_const) dis is
  | (DeclItem_Tuple dis, _) ->
      raise
        (EvalError
           ( loc,
             "add_decl_item_vars should be a tuple " ^ string_of_value i))
  | (DeclItem_BitTuple dbs, r) ->
      let _ = List.fold_right (fun ((ov, ty) : Ident.t option * AST.ty) (idx : int) ->
        let width_expr = Option.get (Asl_utils.width_of_type ty) in
        let width = eval_expr loc env width_expr |> to_int loc in
        Option.iter (fun v ->
            let r' = extract_bits' loc r idx width in
            Env.setVar loc env v r'
          )
          ov;
        idx + width
      ) dbs 0 in
      ()
  | (DeclItem_Wildcard _, _) ->
      ()

(** Evaluate list of statements *)
and eval_stmts (env : Env.t) (xs : AST.stmt list) : unit =
  Env.nest env (fun env' -> List.iter (eval_stmt env') xs)

(** Evaluate statement *)
and eval_stmt (env : Env.t) (x : AST.stmt) : unit =
  match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) ->
      List.iter
        (fun v -> Env.addLocalVar loc env v (mk_uninitialized loc env ty))
        vs
  | Stmt_VarDecl (di, i, loc) ->
      let i' = eval_expr loc env i in
      add_decl_item_vars loc env false di i'
  | Stmt_ConstDecl (di, i, loc) ->
      let i' = eval_expr loc env i in
      add_decl_item_vars loc env true di i'
  | Stmt_Assign (l, r, loc) ->
      let r' = eval_expr loc env r in
      eval_lexpr loc env l r'
  | Stmt_TCall (f, tes, es, _, loc) ->
      let tvs = eval_exprs loc env tes in
      let vs = eval_exprs loc env es in
      eval_proccall loc env f tvs vs
  | Stmt_FunReturn (e, loc) ->
      let v = eval_expr loc env e in
      raise (Return (Some v))
  | Stmt_ProcReturn loc -> raise (Return None)
  | Stmt_Assert (e, loc) ->
      if not (to_bool loc (eval_expr loc env e)) then
        raise (EvalError (loc, "assertion failure"))
  | Stmt_Throw (e, loc) ->
      raise (Throw (loc, eval_expr loc env e))
  | Stmt_Block (b, loc) -> eval_stmts env b
  | Stmt_If (c, t, els, (e, el), loc) ->
      let rec eval css d =
        match css with
        | [] -> eval_stmts env d
        | S_Elsif_Cond (c, s, loc) :: css' ->
            if to_bool loc (eval_expr loc env c) then eval_stmts env s
            else eval css' d
      in
      eval (S_Elsif_Cond (c, t, loc) :: els) e
  | Stmt_Case (e, oty, alts, odefault, loc) ->
      let rec eval v alts =
        match alts with
        | [] -> (
            match odefault with
            | None -> raise (EvalError (loc, "unmatched case"))
            | Some (s, _) -> eval_stmts env s)
        | Alt_Alt (ps, oc, s, loc) :: alts' ->
            if
              List.exists (eval_pattern loc env v) ps
              && Option.value
                   (Option.map (to_bool loc)
                      (Option.map (eval_expr loc env) oc))
                   ~default:true
            then eval_stmts env s
            else eval v alts'
      in
      eval (eval_expr loc env e) alts
  | Stmt_For (v, start, dir, stop, b, loc) ->
      let start' = eval_expr loc env start in
      let stop' = eval_expr loc env stop in
      let rec eval i =
        let c =
          match dir with
          | Direction_Up -> eval_leq loc i stop'
          | Direction_Down -> eval_leq loc stop' i
        in
        if c then (
          Env.nest env (fun env' ->
              Env.addLocalVar loc env' v i;
              eval_stmts env' b);
          let i' =
            match dir with
            | Direction_Up -> eval_add_int loc i (VInt Z.one)
            | Direction_Down -> eval_sub_int loc i (VInt Z.one)
          in
          eval i')
      in
      eval start'
  | Stmt_While (c, b, loc) ->
      let rec eval _ =
        if to_bool loc (eval_expr loc env c) then begin
            eval_stmts env b;
            eval ()
        end
      in
      eval ()
  | Stmt_Repeat (b, c, pos, loc) ->
      let rec eval _ =
        eval_stmts env b;
        if not (to_bool loc (eval_expr loc env c)) then eval ()
      in
      eval ()
  | Stmt_Try (tb, pos, catchers, odefault, loc) ->
      ( try eval_stmts env tb with
      | Return v -> raise (Return v)
      | Throw (l, (VRecord (etc, fs) as exc)) ->
          let rec eval cs =
            ( match cs with
            | [] ->
                ( match odefault with
                | None -> raise (Throw (loc, exc))
                | Some (s, _) -> eval_stmts env s
                )
            | Catcher_Guarded (v, tc, b, loc) :: cs' ->
                if etc = tc then
                  Env.nest env (fun env' ->
                    Env.addLocalVar loc env' v exc;
                    eval_stmts env' b
                    )
                else
                  eval cs'
            )
          in
          eval catchers
      )

(** Evaluate call to function or procedure *)
and eval_call (loc : Loc.t) (env : Env.t) (f : Ident.t) (tvs : value list)
    (vs : value list) : unit =
  let module Tracer = (val (!tracer) : Tracer) in
  match eval_prim f tvs vs with
  | Some r ->
      Tracer.trace_function ~is_prim:true ~is_return:false f tvs vs;
      Tracer.trace_function ~is_prim:true ~is_return:true f [] [r];
      raise (Return (Some r))
  | None ->
      Tracer.trace_function ~is_prim:false ~is_return:false f tvs vs;
      let targs, args, loc, b =
        Utils.from_option
          (GlobalEnv.get_function (Env.globals env) f)
          (fun _ ->
            raise (EvalError (loc, "Undeclared function " ^ Ident.to_string f)))
      in
      Env.nestTop env (fun env' ->
          List.iter2 (fun arg v -> Env.addLocalVar loc env' arg v) targs tvs;
          List.iter2 (fun arg v -> Env.addLocalVar loc env' arg v) args vs;
          eval_stmts env' b)

(** Evaluate call to function *)
and eval_funcall (loc : Loc.t) (env : Env.t) (f : Ident.t) (tvs : value list)
    (vs : value list) : value =
  try
    eval_call loc env f tvs vs;
    raise (EvalError (loc, "no return statement"))
  with
  | Return (Some v) ->
      let module Tracer = (val (!tracer) : Tracer) in
      Tracer.trace_function ~is_prim:false ~is_return:true f [] [v];
      v
  | Throw (l, exc) -> raise (Throw (l, exc))
  | ex ->
    Printf.printf "  %s: runtime exception thrown in %s\n%!" (Loc.to_string loc) (Ident.to_string f);
    raise ex

(** Evaluate call to procedure *)
and eval_proccall (loc : Loc.t) (env : Env.t) (f : Ident.t) (tvs : value list)
    (vs : value list) : unit =
  ( try eval_call loc env f tvs vs with
  | Return None -> ()
  | Return (Some (VTuple [])) -> ()
  | Throw (l, exc) -> raise (Throw (l, exc))
  | ex ->
    Printf.printf "  %s: runtime exception thrown in %s\n%!" (Loc.to_string loc) (Ident.to_string f);
    raise ex
  );
  let module Tracer = (val (!tracer) : Tracer) in
  Tracer.trace_function ~is_prim:false ~is_return:true f [] []

(****************************************************************)
(** {2 Creating environment from global declarations}           *)
(****************************************************************)

(* Uninitialized global variables are UNKNOWN by default *)
let eval_uninitialized (loc : Loc.t) (env : Env.t) (x : AST.ty) : value =
  eval_unknown loc env x

(** Construct global constant environment from global declarations *)
let build_constant_environment (ds : AST.declaration list) : GlobalEnv.t =
  if false then
    Printf.printf "Building environment from %d declarations\n" (List.length ds);
  let genv = GlobalEnv.empty in
  (* todo?: first pull out the constants/configs and evaluate all of them
   * lazily?
   *)
  List.iter
    (fun d ->
      match d with
      | Decl_Record (v, ps, fs, loc) -> GlobalEnv.add_record genv v ps fs
      | Decl_Exception (v, fs, loc) -> GlobalEnv.add_record genv v [] fs
      | Decl_Enum (qid, es, loc) ->
          let evs =
            if qid = boolean_ident then
              [
                (* optimized special case *)
                (false_ident, VBool false);
                (true_ident, VBool true);
              ]
            else List.mapi (fun i e -> (e, VEnum (e, i))) es
          in
          List.iter (fun (e, v) -> GlobalEnv.addGlobalConst genv e v) evs;
          GlobalEnv.addEnum genv qid (List.map (fun (e, v) -> v) evs)
      | Decl_Typedef (v, ps, ty, loc) -> GlobalEnv.addTypedef genv v ps ty
      | Decl_Const (v, ty, i, loc) ->
          (* todo: constants need to be lazily evaluated or need to be
           * sorted by dependencies
           *)
          let init = eval_expr loc (Env.newEnv genv) i in
          GlobalEnv.addGlobalConst genv v init
      | Decl_FunDefn (f, fty, body, loc) ->
          let tvs = List.map fst fty.parameters in
          let args = List.map fst fty.args @ List.map fst (Option.to_list fty.setter_arg) in
          GlobalEnv.addFun loc genv f (tvs, args, loc, body)
      (* The following declarations are part of the mutable global state *)
      | Decl_Config (ty, v, _, loc)
      | Decl_Var (ty, v, loc) -> ()
      (* The following declarations have no impact on execution *)
      | Decl_BuiltinType (_, _)
      | Decl_Forward (_, _)
      | Decl_BuiltinFunction (_, _, _)
      | Decl_FunType (_, _, _)
      | Decl_Operator1 (_, _, _)
      | Decl_Operator2 (_, _, _) ->
          ())
    ds;
  genv

(** Construct environment from global declarations *)
let build_evaluation_environment (ds : AST.declaration list) : Env.t =
  let genv = build_constant_environment ds in
  let env = Env.newEnv genv in
  List.iter
    (fun d ->
      match d with
      | Decl_Config (v, ty, i, loc) ->
          (* todo: config constants need to be lazily evaluated or need to be
           * sorted by dependencies
           *)
          let init = eval_expr loc (Env.newEnv genv) i in
          Env.addGlobalVar env v init
      | Decl_Var (v, ty, loc) ->
          let init = eval_uninitialized loc (Env.newEnv genv) ty in
          Env.addGlobalVar env v init
      | _ -> ())
    ds;
  env

(****************************************************************
 * End
 ****************************************************************)
