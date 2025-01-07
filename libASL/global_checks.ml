(****************************************************************
 * Global checks
 *
 * This module performs global checks that rely on analyzing
 * the call-graph. For example, to check whether a function
 * transitively accesses a global variable or transitively
 * throws an exception.
 *
 * Checks performed:
 *
 * - Does the meaning of an expression depend on evaluation order?
 * - Are calls to functions that can throw exceptions marked as rethrowing?
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module FMT = Asl_fmt
open Asl_utils
open Builtin_idents
open Identset

let get (f : Ident.t) (bs : IdentSet.t Bindings.t) : IdentSet.t =
  Option.value ~default:IdentSet.empty (Bindings.find_opt f bs)

let verbose = ref false

(****************************************************************
 * Check that function definitions have correct throw/nothrow markers
 *
 * This is an experimental extensions where function definitions
 * must be marked with whether or not they can throw exceptions
 * and function calls must have a matching marker.
 *
 * We distinguish between functions that can optionally throw an
 * exception and functions that always throw an exception.
 *
 * As a side effect of checking for functions that always throw
 * an exception, we also detect dead code and detect functions
 * that are missing return statements.
 ****************************************************************)

let check_defn_markers = ref false

(* The core of this checker is checking functions for exceptions, return or fail
 * instead of following the usual control flow.
 * Each expression or statement is summarized with the following record.
 *)
type status = {
    live : bool option; (* None => don't know; Some b => yes/no *)
    exc : bool;         (* can this statement throw an exception? *)
    ret : bool;         (* can this statement return a value? *)
}

let fmt_status (fmt : Format.formatter) (s : status) : unit =
    Format.fprintf fmt "{ live=%s exc=%b ret=%b }"
        (match s.live with None -> "?" | Some true -> "Yes" | Some false -> "No")
        s.exc
        s.ret

let ok       = { live = Some true;  exc = false; ret = false; }
let maythrow = { live = None;       exc = true;  ret = false; }
let throw    = { live = Some false; exc = true;  ret = false; }
let return   = { live = Some false; exc = false; ret = true;  }
let fail     = { live = Some false; exc = false; ret = false; }

(* At join points, we merge states in the obvious way *)
let status_merge (x : status) (y : status) : status =
    { live = if Option.equal (=) x.live y.live then x.live else None;
      exc = x.exc || y.exc;
      ret = x.ret || y.ret;
    }

(* When two actions are sequenced, the first action takes precedence
 * if it is dead.
 *)
let status_seq (x : status) (y : status) : status =
    if x.live = Some false then
        x
    else
        { live = if x.live = Some true then y.live
                 else if y.live = Some true then x.live
                 else y.live;
          exc = x.exc || y.exc;
          ret = x.ret || y.ret;
        }

let canthrow_expr (x : AST.expr) : status =
    let (_, _, _, canthrow) = side_effects_of_expr x in
    if canthrow then maythrow else ok

let canthrow_lexpr (x : AST.lexpr) : status =
    let (_, _, _, canthrow) = side_effects_of_lexpr x in
    if canthrow then maythrow else ok

let rec canthrow_stmt (x : AST.stmt) : status =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, ty, loc) -> ok
  | Stmt_VarDecl (di, i, loc) -> canthrow_expr i
  | Stmt_ConstDecl (di, i, loc) -> canthrow_expr i
  | Stmt_Assign (l, r, loc) -> status_merge (canthrow_expr r) (canthrow_lexpr l)
  | Stmt_TCall (f, tes, args, throws, loc) ->
      if String.starts_with ~prefix:"Unimplemented" (Ident.name f) then
          fail
      else
      ( match throws with
      | NoThrow -> ok
      | MayThrow -> maythrow
      | AlwaysThrow -> throw
      )
  | Stmt_FunReturn (e, loc) -> status_seq (canthrow_expr e) return
  | Stmt_ProcReturn loc -> return
  | Stmt_Assert (e, loc) -> ok
  | Stmt_Throw (e, loc) -> throw
  | Stmt_Block (b, loc) -> canthrow_stmts b
  | Stmt_If (c, t, els, (e, el), loc) ->
      let els' = AST.S_Elsif_Cond (c, t, loc) :: els in
      let rs = List.map (function AST.S_Elsif_Cond(e, b, _) -> status_seq (canthrow_expr e) (canthrow_stmts b)) els' in
      let r = canthrow_stmts e in
      List.fold_left status_merge r rs
  | Stmt_Case (e, oty, alts, ob, loc) ->
      let r = canthrow_expr e in
      let rs = List.map
          (function AST.Alt_Alt (ps, og, b, _) ->
            status_seq
              (Option.fold ~none:ok ~some:canthrow_expr og)
              (canthrow_stmts b))
          alts
      in
      let rb = Option.fold ~none:ok ~some:(fun (b, _) -> canthrow_stmts b) ob in
      status_seq r
        (List.fold_left status_merge rb rs)
  | Stmt_For (v, f, dir, t, b, loc) ->
          (status_seq (canthrow_expr f)
          (status_seq (canthrow_expr t)
                      (canthrow_stmts b)))
  | Stmt_While (c, b, loc) ->
          (status_merge (canthrow_expr c) (canthrow_stmts b))
  | Stmt_Repeat (b, c, pos, loc) ->
          (status_merge (canthrow_stmts b) (canthrow_expr c))
  | Stmt_Try (b, pos, cs, ob, loc) ->
          if Option.is_some ob then ok else maythrow
  )

and canthrow_stmts (xs : AST.stmt list) : status =
    ( match xs with
    | [] -> ok
    | x :: [] -> canthrow_stmt x
    | x :: y :: zs ->
            let r = canthrow_stmt x in
            if r = fail then (
                r
            ) else (
                if r.live = Some false then begin
                    Format.printf "Warning (%s): Dead code detected '%a'\n"
                        (Loc.to_string (Asl_utils.stmt_loc y))
                        FMT.stmt y
                end;
                let s = canthrow_stmts (y :: zs) in
                if s = fail then
                    fail
                else
                    status_seq r s
            )
    )

class check_defn_exception_class =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vdecl d =
      ( match d with
      | Decl_FunDefn (f, fty, b, loc)
      ->
          let s = canthrow_stmts b in
          if !verbose then begin
              Format.printf "%a: %a\n"
                FMT.funname f
                fmt_status s
          end;
          let should_return = Option.is_some fty.rty in
          if s <> fail then begin
              ( match fty.throws with
              | NoThrow when s.exc ->
                  let problem = if s = throw then "always throws an exception but is not marked with '!'"
                                else "can throw an exception but is not marked with '?'"
                  in
                  let msg = Format.asprintf "Function definition '%a' %s"
                      FMT.funname f
                      problem
                  in
                  raise (Error.TypeError (loc, msg))
              | MayThrow when s.live <> None && not s.exc ->
                    let msg = Format.asprintf "Function/procedure definition '%a' is marked '?' but does not conditionally throw an exception"
                        FMT.funname f
                    in
                    raise (Error.TypeError (loc, msg))
              | AlwaysThrow when s <> throw ->
                    let msg = Format.asprintf "Function/procedure definition '%a' is marked '!' but does not always throw an exception"
                        FMT.funname f
                    in
                    raise (Error.TypeError (loc, msg))
              | _ -> ()
              );
              if should_return && not s.ret then begin
                if s = throw then begin
                    Format.printf "Warning: Function definition '%a' should return a value but always throws an exception instead@,"
                        FMT.funname f
                end else begin
                    let msg = Format.asprintf "Function definition '%a' should return a value but does not"
                        FMT.funname f
                    in
                    raise (Error.TypeError (loc, msg))
                end
              end
          end;
          SkipChildren
      | _ ->
          SkipChildren
      )
  end

(****************************************************************
 * Check that function calls have correct throw/nothrow markers
 *
 * This is an experimental extensions where function definitions
 * must be marked with whether or not they can throw exceptions
 * and function calls must have a matching marker.
 ****************************************************************)

let check_call_markers = ref false

class exn_call_checks_class (markers : (AST.can_throw * Loc.t) Bindings.t) (loc : Loc.t) =
  object (self)
    inherit Asl_visitor.nopAslVisitor

    method check_marker (f : Ident.t) (call_loc : Loc.t) (call_marker : AST.can_throw) : unit =
      let (defn_marker, defn_loc) = Option.value (Bindings.find_opt f markers) ~default:(NoThrow, Loc.Unknown) in
      if call_marker <> defn_marker then begin
          let msg = Format.asprintf "Exception marker '%a' on call to '%a' does not match exception marker '%a' on definition at %a"
                    FMT.throws call_marker
                    FMT.funname f
                    FMT.throws defn_marker
                    FMT.loc defn_loc
          in
          raise (Error.TypeError (loc, msg))
      end

    method! vexpr x =
      ( match x with
      | Expr_TApply (f, _, _, throws) ->
          self#check_marker f loc throws
      | _ ->
          ()
      );
      DoChildren

    method! vstmt x =
      ( match x with
      | Stmt_TCall (f, _, _, throws, loc) ->
          self#check_marker f loc throws
      | _ ->
          ()
      );
      SkipChildren

  end

(****************************************************************
 * Infer which functions can throw exceptions and/or have side
 * effects and use that to detect expressions which are sensitive
 * to evaluation order and to detect function calls that
 * can throw exceptions but that are not marked appropriately.
 ****************************************************************)

(** Calculates the effects of all functions in a set of declarations.
 *
 * Effects include
 * - any global variables read
 * - any global variables written
 * - whether it throws an exception
 *
 *)
class effects_class
    (is_constant : Ident.t -> bool)
    (is_impure_prim : Ident.t -> bool)
    (ds : AST.declaration list) =

  object (self)
    val mutable globals : IdentSet.t = IdentSet.empty
    val mutable throws : IdentSet.t = IdentSet.empty
    val mutable callees : IdentSet.t Bindings.t = Bindings.empty
    val mutable callers : IdentSet.t Bindings.t = Bindings.empty
    val mutable reads : IdentSet.t Bindings.t = Bindings.empty
    val mutable writes : IdentSet.t Bindings.t = Bindings.empty

    method is_global (x : Ident.t) : bool = IdentSet.mem x globals

    method fun_effects (f : Ident.t) : (IdentSet.t * IdentSet.t * bool) =
      (get f reads, get f writes, IdentSet.mem f throws)

    method add_effects (f : Ident.t) (rds : IdentSet.t) (wrs : IdentSet.t) (throws_exn : bool) : bool =
      let changed = ref false in
      changed := !changed || (throws_exn && not (IdentSet.mem f throws));
      changed := !changed || not (IdentSet.subset rds (get f reads));
      changed := !changed || not (IdentSet.subset wrs (get f writes));
      if throws_exn then throws <- IdentSet.add f throws;
      reads <- unionToBindingSet f rds reads;
      writes <- unionToBindingSet f wrs writes;
      !changed

    (* Update the effects of 'f' with the effects of all functions that it calls.
     * Return 'true' if this adds anything new.
     *)
    method private update (f : Ident.t) : bool =
      let changed = ref false in
      IdentSet.iter (fun g ->
          let (rds, wrs, throws_exn) = self#fun_effects g in
          changed := self#add_effects f rds wrs throws_exn || !changed
        )
        (get f callees);
      !changed

    (* Propagate the effects of any changed functions to their callers *)
    method private fixpoint (fs : IdentSet.t) : unit =
      if not (IdentSet.is_empty fs) then begin
        let parents =
          IdentSet.elements fs
          |> List.map (fun f -> get f callers)
          |> Identset.unionSets
        in
        let next = IdentSet.filter self#update parents in
        self#fixpoint next
      end

    initializer begin
      (* start by calculating the local effects of each function *)
      List.iter (fun d ->
        decl_name d |>
        Option.iter (fun nm ->
            let (rds, wrs, calls, throws_exn) = side_effects_of_decl d in
            globals <- IdentSet.add nm globals;
            if throws_exn then throws <- IdentSet.add nm throws;
            reads <- unionToBindingSet nm rds reads;
            writes <- unionToBindingSet nm wrs writes;
            callees <- unionToBindingSet nm calls callees;
            IdentSet.iter (fun f -> callers <- addToBindingSet f nm callers) calls
        )
      ) ds;

      (* then compute the fixpoint *)
      self#fixpoint globals
    end
  end

(** Check that expression value does not depend on evaluation order
 *
 * This check is done by checking that we don't have two disjoint
 * subexpressions 'e1' and 'e2' such that
 *
 * - 'e1' writes a global variable that 'e2' reads or writes
 * - 'e1' writes a global variable and 'e2' can throw an exception
 * - 'e1' and 'e2' can both throw an exception
 *)
let check_effect_conflicts
    (loc : Loc.t)
    ((e1, fx1) : AST.expr * (IdentSet.t * IdentSet.t * bool))
    ((e2, fx2) : AST.expr * (IdentSet.t * IdentSet.t * bool))
  : unit
  =
  let (rds1, wrs1, throws1) = fx1 in
  let (rds2, wrs2, throws2) = fx2 in
  let ww_conflicts = IdentSet.inter wrs1 wrs2 in
  let wr_conflicts = IdentSet.inter wrs1 rds2 in
  let wt_conflicts = throws1 && not (IdentSet.is_empty wrs2) in
  let throw_conflicts = throws1 && throws2 in
  if not (IdentSet.is_empty ww_conflicts) then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    global variable(s) `%a` can be \
         written to by both subexpression `%a` \
         and subexpression `%a`"
        pp_identset ww_conflicts
        FMT.expr e1
        FMT.expr e2
    in
    raise (Error.TypeError (loc, msg))
  end;
  if not (IdentSet.is_empty wr_conflicts) then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,   global variable(s) `%a` can be \
         written to by subexpression `%a` \
         and read from by subexpression `%a`"
        pp_identset wr_conflicts
        FMT.expr e1
        FMT.expr e2
    in
    raise (Error.TypeError (loc, msg))
  end;
  if wt_conflicts then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    subexpression `%a` can throw an exception \
         and subexpression `%a` can write to global variable(s) `%a`"
        FMT.expr e1
        FMT.expr e2
        pp_identset wrs2
    in
    raise (Error.TypeError (loc, msg))
  end;
  if throw_conflicts then begin
    let msg = Format.asprintf
        "expression behaviour depends on evaluation order:\
         @,    subexpression `%a` \
         and subexpression `%a` can both throw exceptions"
        FMT.expr e1
        FMT.expr e2
    in
    raise (Error.TypeError (loc, msg))
  end


(** Check immediate subexpressions for expression order dependencies
 *
 * Returns the set of reads, writes and whether 'e' throws an exception
 * Reports an error on failure.
 *)
let rec check_expression_order (loc : Loc.t) (effects : effects_class) (e : AST.expr) : (IdentSet.t * IdentSet.t * bool) =
  ( match e with
  | Expr_Var v ->
      if effects#is_global v then
        (IdentSet.singleton v, IdentSet.empty, false)
      else
        (IdentSet.empty, IdentSet.empty, false)
  | _ ->
    let ordered = (* is evaluation order specified by ASL language? *)
      ( match e with
      | Expr_If _ -> true
      | Expr_TApply (i, _, _, _) when Ident.in_list i [
          and_bool;
          or_bool;
          implies_bool
        ]
        -> true
      | _
        -> false
      )
    in
    let es = subexprs_of_expr e in
    let fxs = List.map (fun e -> (e, check_expression_order loc effects e)) es in
    if not ordered then begin
      Utils.iter_pairs (check_effect_conflicts loc) fxs;
      Utils.iter_pairs (Fun.flip (check_effect_conflicts loc)) fxs
    end;
    let rds = Identset.unionSets (List.map (fun (_, (r, _, _)) -> r) fxs) in
    let wrs = Identset.unionSets (List.map (fun (_, (_, w, _)) -> w) fxs) in
    let throws = List.exists (fun (_, (_, _, t)) -> t) fxs in
    let (frds, fwrs, fthrows) =
      ( match e with
      | Expr_TApply (f, _, _, _) -> effects#fun_effects f
      | _ -> (IdentSet.empty, IdentSet.empty, false)
      )
    in
    (IdentSet.union frds rds, IdentSet.union fwrs wrs, fthrows || throws)
  )

(** Perform rethrow checks on the specification *)
class rethrow_checks_class (effects : effects_class) (loc : Loc.t) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ( match x with
      | Expr_TApply (f, _, _, throws) ->
        let (_, _, fthrows) = effects#fun_effects f in
        if throws <> NoThrow && not fthrows then begin
            let msg = Format.asprintf
                "call to function `%a` is incorrectly marked with `?` but it cannot throw an exception"
                FMT.varname f
            in
            raise (Error.TypeError (loc, msg))
        end else
        if throws = NoThrow && fthrows then begin
            let msg = Format.asprintf
                "call to function `%a` should be marked with `?` or `!` because it can throw an exception"
                FMT.varname f
            in
            raise (Error.TypeError (loc, msg))
        end
      | _ -> ()
      );
      DoChildren

    method! vstmt x =
      ( match x with
      | Stmt_TCall (f, _, _, throws, loc) ->
        let (_, _, fthrows) = effects#fun_effects f in
        if throws <> NoThrow && not fthrows then begin
            let msg = Format.asprintf
                "call to procedure `%a` is incorrectly marked with `?`  or `!`but it cannot throw an exception"
                FMT.varname f
            in
            raise (Error.TypeError (loc, msg))
        end else if throws = NoThrow && fthrows then begin
            let msg = Format.asprintf
                "call to procedure `%a` should be marked with `?` or `!` because it can throw an exception"
                FMT.varname f
            in
            raise (Error.TypeError (loc, msg))
        end
      | _ -> ()
      );
      SkipChildren
  end

(** Perform global checks on the specification *)
class global_checks_class (effects : effects_class) (markers : (AST.can_throw * Loc.t) Bindings.t) (loc : Loc.t) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vexpr x =
      ignore (check_expression_order loc effects x);
      ignore (Asl_visitor.visit_expr (new rethrow_checks_class effects loc) x);
      if !check_call_markers then begin
          ignore (Asl_visitor.visit_expr (new exn_call_checks_class markers loc :> Asl_visitor.aslVisitor) x)
      end;
      SkipChildren

    method! vstmt x =
      ignore (Asl_visitor.visit_stmt (new rethrow_checks_class effects (stmt_loc x)) x);
      if !check_call_markers then begin
          ignore (Asl_visitor.visit_stmt (new exn_call_checks_class markers loc :> Asl_visitor.aslVisitor) x)
      end;
      DoChildren
  end

(** Wrapper around global_checks_class that adds location information *)
class global_checks_class_wrapper (effects : effects_class) (markers : (AST.can_throw * Loc.t) Bindings.t) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vstmt x =
      ignore (Asl_visitor.visit_stmt (new global_checks_class effects markers (stmt_loc x)) x);
      SkipChildren
  end

let check_decls (ds : AST.declaration list) : AST.declaration list =
  let genv = Eval.build_constant_environment ds in
  let is_constant (v : Ident.t) : bool =
    Option.is_some (Eval.GlobalEnv.get_global_constant genv v)
  in
  let is_impure_prim (v : Ident.t) : bool =
    List.exists (fun name -> Ident.matches v ~name) Value.impure_prims
  in
  let effects = new effects_class is_constant is_impure_prim ds in

  let defn_marker (d : AST.declaration) : (Ident.t * (AST.can_throw * Loc.t)) option =
      ( match d with
      | Decl_FunType (f, fty, loc) -> Some (f, (fty.throws, loc))
      | Decl_FunDefn (f, fty, _, loc) -> Some (f, (fty.throws, loc))
      | _ -> None
      )
  in
  let markers = List.map defn_marker ds |> Utils.flatten_option |> Identset.mk_bindings in

  let checker = new global_checks_class_wrapper effects markers in
  let exn_defn_checker = new check_defn_exception_class in
  let error_count = ref 0 in
  let check_decl d =
    ( try
        ignore (Asl_visitor.visit_decl checker d);
        if !check_defn_markers then begin
            ignore (Asl_visitor.visit_decl exn_defn_checker d)
        end
    with
    | Error.TypeError _ as exn
    ->
      if !error_count < !Tcheck.max_errors then begin
        Error.print_exception exn;
        error_count := !error_count + 1
      end else begin
        raise exn
      end
    )
  in
  List.iter check_decl ds;
  if !error_count != 0 then begin
    raise (Error.TypeError (Loc.Unknown, "Errors detected"))
  end;
  ds

(****************************************************************
 * End
 ****************************************************************)
