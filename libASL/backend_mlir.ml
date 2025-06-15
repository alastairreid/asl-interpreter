(****************************************************************
 * ASL to MLIR backend
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(* Turn off warnings about unused (debug) functions. *)
[@@@warning "-32"]
[@@@warning "-37-39-69"] (* temporary - until new IR is in place *)

(** ASL to MLIR backend *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
module Builtins = Builtin_idents
module HLIR = Hlir
open Asl_utils
open Format_utils
open Utils

(****************************************************************
 * Pretty printing helpers
 ****************************************************************)

let commasep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    ~pp_sep:(fun fmt' _ -> PP.pp_print_string fmt' ", ")
    pp
    fmt
    xs

let cutsep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    pp
    fmt
    xs

let vbox (fmt : PP.formatter) (pp : unit -> 'a) : 'a=
  PP.pp_open_vbox fmt 0;
  let r = pp () in
  PP.pp_close_box fmt ();
  r

let indentation = 4

let indented (fmt : PP.formatter) (pp : unit -> 'a) =
  PP.pp_print_break fmt indentation indentation;
  vbox fmt pp

let ident (fmt : PP.formatter) (x : Ident.t) : unit = Ident.pp fmt x

let varident (fmt : PP.formatter) (x : Ident.t) : unit =
  if String.starts_with ~prefix:"%" (Ident.name x) then
    PP.fprintf fmt "%s" (Ident.name x)
  else
    PP.fprintf fmt "%%%s" (Ident.name x)

(****************************************************************
 * Primop support
 ****************************************************************)

let instantiate_funtype (ps : AST.expr list) (fty : AST.function_type) : AST.function_type =
  let env = Identset.mk_bindings (List.map2 (fun (p, _) ty -> (p, ty)) fty.parameters ps) in
  Asl_utils.subst_funtype env fty

(* set of all primitive operation names - these will be preceded by asl. *)
let primitive_operations = Identset.IdentSet.of_list [
  Builtin_idents.not_bool;
  Builtin_idents.strict_and_bool;
  Builtin_idents.strict_or_bool;
  Builtin_idents.neg_int;
  Builtin_idents.not_bits;
  Builtin_idents.zeros_bits;
  Builtin_idents.ones_bits;
  Builtin_idents.eq_int;
  Builtin_idents.le_int;
  Builtin_idents.add_int;
  Builtin_idents.sub_int;
  Builtin_idents.mul_int;
  Builtin_idents.eq_bits;
  Builtin_idents.add_bits;
  Builtin_idents.sub_bits;
  Builtin_idents.mul_bits;
  Builtin_idents.in_mask;
  Builtin_idents.and_bits;
  Builtin_idents.or_bits;
  Builtin_idents.lsr_bits;
  Builtin_idents.lsl_bits;
  Builtin_idents.eq_bool;
  Builtin_idents.ne_bool;
  Builtin_idents.align_int;
  Builtin_idents.exact_div_int;
  Builtin_idents.fdiv_int;
  Builtin_idents.frem_int;
  Builtin_idents.ge_int;
  Builtin_idents.gt_int;
  Builtin_idents.is_pow2_int;
  Builtin_idents.lt_int;
  Builtin_idents.mod_pow2_int;
  Builtin_idents.ne_int;
  Builtin_idents.pow2_int;
  Builtin_idents.pow_int_int;
  Builtin_idents.shl_int;
  Builtin_idents.shr_int;
  Builtin_idents.zdiv_int;
  Builtin_idents.zrem_int;
  Builtin_idents.add_real;
  Builtin_idents.cvt_int_real;
  Builtin_idents.divide_real;
  Builtin_idents.eq_real;
  Builtin_idents.ge_real;
  Builtin_idents.gt_real;
  Builtin_idents.le_real;
  Builtin_idents.lt_real;
  Builtin_idents.mul_real;
  Builtin_idents.ne_real;
  Builtin_idents.neg_real;
  Builtin_idents.pow2_real;
  Builtin_idents.round_down_real;
  Builtin_idents.round_tozero_real;
  Builtin_idents.round_up_real;
  Builtin_idents.sqrt_real;
  Builtin_idents.sub_real;
  Builtin_idents.cvt_bits_sint;
  Builtin_idents.cvt_bits_uint;
  Builtin_idents.cvt_int_bits;
  Builtin_idents.xor_bits;
  Builtin_idents.frem_bits_int;
  Builtin_idents.notin_mask;
  Builtin_idents.asr_bits;
  Builtin_idents.ne_bits;
  Builtin_idents.replicate_bits;
  Builtin_idents.append_str_str;
  Builtin_idents.cvt_bits_str;
  Builtin_idents.cvt_bool_str;
  Builtin_idents.cvt_int_decstr;
  Builtin_idents.cvt_int_hexstr;
  Builtin_idents.cvt_real_str;
  Builtin_idents.eq_str;
  Builtin_idents.ne_str;
  Builtin_idents.ram_init;
  Builtin_idents.ram_read;
  Builtin_idents.ram_write;
  Builtin_idents.add_bits_int;
  Builtin_idents.sub_bits_int;
  Builtin_idents.mul_bits_int;
  Builtin_idents.zero_extend_bits;
  Builtin_idents.sign_extend_bits;
  Builtin_idents.append_bits;
  Builtin_idents.mk_mask;
  Builtin_idents.mask_int;
  Builtin_idents.asl_extract_bits;
  Builtin_idents.asl_extract_int;
  Builtin_idents.print_bits_hex;
  Builtin_idents.print_sintN_hex;
  Builtin_idents.print_sintN_dec
]

(****************************************************************
 * 
 ****************************************************************)

let enum_size = 8 (* assume this is big enough for all enumerated types *)
let enums : int Identset.Bindings.t ref = ref Identset.Bindings.empty
let type_of_enum : AST.ty Identset.Bindings.t ref = ref Identset.Bindings.empty
let enum_types : Identset.IdentSet.t ref = ref Identset.IdentSet.empty

let vartypes : AST.ty Identset.Bindings.t ref = ref Identset.Bindings.empty
let funtypes : AST.function_type Identset.Bindings.t ref = ref Identset.Bindings.empty

(****************************************************************
 * HLIR context
 *
 * The key operations supported by the context are:
 * - Tracking information about the current region
 *   - Operations added to the current region
 *   - Source level variables assigned to in tthe current region
 * - Environment support
 *   - reading and writing the bindings of source language variables
 *     to HLIR variables
 *   - listing the variables assigned to in the current region
 * - Cloning the context (with an empty set of mutable variables and
 *   a nested scope for immutable variables).
 * - Providing access to the name supply for this function
 *   Note that the same name supply is used for the entire function.
 *
 * To make it easy to find the modified variables, we maintain two
 * mappings:
 * - the initial binding of a source language variable to a value
 *   (most variables are immutable - so they will be in this mapping)
 * - the changes to the bindings in the current region
 ****************************************************************)

(* IR code generators incrementally write code to a codegen context
 * containing all mutable state.
 *)
type context = {
  var_idents : Asl_utils.nameSupply;
  operations : HLIR.operation list ref; (* in reverse order *)
  (* note that both initial_bindings and local_rebindings are internally mutable *)
  initial_bindings : HLIR.ident ScopeStack.t; (* local variables *)
  local_rebindings : HLIR.ident Scope.t; (* local modifications *)
}

let ppContext (fmt : PP.formatter) (ctx : context) : unit =
    (*
  let pp_entry (fmt : PP.formatter) (x : hlir_env_entry) : unit =
      let (v, mut) = x in
      HLIR.ppIdent fmt v;
      if mut then PP.fprintf fmt " mutable"
  in
  PP.fprintf fmt "{ %a }" (ScopeStack.pp pp_entry) env
  *)
    ()

let fresh_context (_ : unit) : context =
  { var_idents = new nameSupply "%";
    operations = ref [];
    initial_bindings = ScopeStack.empty ();
    local_rebindings = Scope.empty ();
  }

let clone_context (ctx : context) : context =
  { var_idents = ctx.var_idents;
    operations = ref [];
    initial_bindings = ScopeStack.add_local_scope ctx.initial_bindings;
    local_rebindings = Scope.empty ()
  }

let mk_fresh (ctx : context) (t : HLIR.ty) : HLIR.ident =
  let v = ctx.var_idents#fresh in
  HLIR.Ident (v, t)

let get_region (ctx : context) (outputs : HLIR.ident list) (inputs : HLIR.ident list) : HLIR.region =
  let ops = List.rev (!(ctx.operations)) in
  ctx.operations := [];
  { inputs; operations = ops; outputs }

let emit_op (ctx : context) (x : HLIR.operation) : unit =
  ctx.operations := x :: !(ctx.operations)

let get_binding (ctx : context) (v : Ident.t) : HLIR.ident option =
  ( match Scope.get ctx.local_rebindings v with
  | Some v' -> Some v'
  | None -> ScopeStack.get ctx.initial_bindings v
  )

let add_initial_binding (ctx : context) (v : Ident.t) (v' : HLIR.ident) : unit =
  ScopeStack.add ctx.initial_bindings v v'

let rebind (ctx : context) (v : Ident.t) (v' : HLIR.ident) : unit =
  Scope.set ctx.local_rebindings v v'

let get_rebound_variables (old_ctx : context) (ctx : context) : HLIR.ident Scope.t =
  Scope.filter
    (fun v v' -> get_binding old_ctx v <> Some v')
    ctx.local_rebindings

(****************************************************************
 * HLIR utilities
 ****************************************************************)

let add_noresult_op (loc : Loc.t) (ctx : context) (op : HLIR.op) (operands : HLIR.ident list) : unit =
  emit_op ctx { results=[]; op; operands; regions=[]; loc }

let add_simple_op (loc : Loc.t) (ctx : context) (rty : HLIR.ty) (op : HLIR.op) (operands : HLIR.ident list) : HLIR.ident =
  let r = mk_fresh ctx rty in
  emit_op ctx { results=[r]; op; operands; regions=[]; loc };
  r

(****************************************************************
 * HLIR generation
 ****************************************************************)

let valueLit (loc : Loc.t) (ctx : context) (x : Value.value) : HLIR.ident =
  let ty = HLIR.mkType (type_of_value loc x) in
  add_simple_op loc ctx ty (Constant x) []

let rec expr_to_ir (loc : Loc.t) (ctx : context) (x : AST.expr) : HLIR.ident =
  ( match x with
  | Expr_Lit v -> valueLit loc ctx v
  | Expr_Var v when Ident.equal v Builtin_idents.false_ident -> valueLit loc ctx (VBool false)
  | Expr_Var v when Ident.equal v Builtin_idents.true_ident -> valueLit loc ctx (VBool true)

  | Expr_Var v when Identset.Bindings.mem v !type_of_enum ->
      let ty = Identset.Bindings.find v !type_of_enum in
      add_simple_op loc ctx (Type ty) (Symbol v) []

  | Expr_Var v ->
      ( match get_binding ctx v with
      | None -> (* global variable *)
          assert (Identset.Bindings.mem v !vartypes);
          let ty = Identset.Bindings.find v !vartypes in
          let ref = add_simple_op loc ctx (Ref ty) (MkRef v) [] in
          add_simple_op loc ctx (Type ty) Load [ref]
      | Some v' -> v' (* local variable *)
      )

  | Expr_Array (e, ix) ->
      let e'  = expr_to_ir loc ctx e in
      let ix' = expr_to_ir loc ctx ix in
      let ref_ty = HLIR.typeof e' in
      let elt_ty = ( match ref_ty with
                   | Ref (Type_Array(_, elt_ty)) -> elt_ty
                   | _ -> raise (InternalError (loc, "expr", (fun fmt -> HLIR.ppType fmt ref_ty), __LOC__))
                   )
      in
      let eref = add_simple_op loc ctx (Ref elt_ty) AddIndex [e'; ix'] in
      add_simple_op loc ctx (Type elt_ty) Load [eref]

  | Expr_In (e, Pat_Lit (VMask mask)) ->
      let e' = expr_to_ir loc ctx e in
      let ty = HLIR.typeof e' in
      let (v, m) = Primops.prim_mask_to_bits mask in
      let value  = add_simple_op loc ctx ty (Constant (VBits v)) [] in
      let mask   = add_simple_op loc ctx ty (Constant (VBits m)) [] in
      let masked = add_simple_op loc ctx ty (Builtin Builtins.and_bits) [e'; mask] in
      add_simple_op loc ctx ty (Builtin Builtins.eq_bits) [masked; value]

  | Expr_If (els, e) ->
      let rec ir_ites els ctx : HLIR.ident =
        ( match els with
        | [] -> expr_to_ir loc ctx e
        | (c,t)::els' ->
           let c' = expr_to_ir loc ctx c in
           let t' = expr_to_region loc ctx t in
           let e' = expr_to_region loc ctx (AST.Expr_If (els', e)) in
           ir_ite loc ctx c' t' e'
        )
      in ir_ites els ctx
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.lazy_and_bool ->
      let x' = expr_to_ir loc ctx x in
      let y' = expr_to_region loc ctx y in
      let ff = value_to_region loc ctx (Value.VBool false) in
      ir_ite loc ctx x' y' ff
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.lazy_or_bool ->
      let x' = expr_to_ir loc ctx x in
      let tt = value_to_region loc ctx (Value.VBool true) in
      let y' = expr_to_region loc ctx y in
      ir_ite loc ctx x' tt y'
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.implies_bool ->
      let x' = expr_to_ir loc ctx x in
      let y' = expr_to_region loc ctx y in
      let tt = value_to_region loc ctx (VBool true) in
      ir_ite loc ctx x' y' tt
  | Expr_TApply (f, ps, args, throws) ->
      let fty = Identset.Bindings.find f !funtypes in
      let fty' = instantiate_funtype ps fty in
      let args' = List.map (expr_to_ir loc ctx) args in
      if Identset.IdentSet.mem f primitive_operations then (
        (* todo: in the backend, we expect primops to be called with fixed bitwidths *)
        add_simple_op loc ctx (Type fty'.rty) (Builtin f) args'
      ) else (
        let ps' = List.map (expr_to_ir loc ctx) ps in
        add_simple_op loc ctx (Type fty'.rty) (Call f) (ps' @ args')
      )
  (* 
  | Expr_Slices (Type_Bits (Expr_Lit (VInt m), _), x, [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) ->
      let (x', _) = expr loc env fmt x in
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.get_slice %a, %a, %a : (!asl.bits<%s>, !asl.int, !asl.int) -> !asl.bits<%s>@,"
        varident t
        varident x'
        varident lo'
        varident wd'
        (Z.to_string m)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  | Expr_Slices (Type_Integer _, Expr_Lit (VInt v), [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) when lo = Asl_utils.zero ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits %s : !asl.bits<%s>@,"
        ident t
        (Z.to_string v)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  *)
  | _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

and value_to_region (loc : Loc.t) (ctx : context) (v : Value.value) : (HLIR.region * HLIR.ty) =
  let ctx' = clone_context ctx in
  let v' = add_simple_op loc ctx' (HLIR.mkType (Asl_utils.type_of_value loc v)) (Constant v) [] in
  (get_region ctx [v'] [], HLIR.typeof v')

and expr_to_region (loc : Loc.t) (ctx : context) (e : AST.expr) : (HLIR.region * HLIR.ty) =
  let ctx' = clone_context ctx in
  let e' = expr_to_ir loc ctx e in
  (get_region ctx' [e'] [], HLIR.typeof e')

and ir_ite (loc : Loc.t) (ctx : context) (c : HLIR.ident) (t : (HLIR.region * HLIR.ty)) (e : (HLIR.region * HLIR.ty)) : HLIR.ident =
  let (t', tty) = t in
  let (e', ety) = e in
  (* tty and ety should be equivalent *)
  let r = mk_fresh ctx tty in
  emit_op ctx { results=[r]; op=If; operands=[c]; regions=[t'; e']; loc };
  r

let rec stmt_to_ir (ctx : context) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDecl (is_constant, DeclItem_Var (v, _), i, loc) ->
      let i' = expr_to_ir loc ctx i in
      add_initial_binding ctx v i'
  | Stmt_VarDecl (is_constant, DeclItem_Wildcard _, i, loc) ->
      ignore (expr_to_ir loc ctx i)
  | Stmt_Assign (LExpr_Var v, rhs, loc) ->
      if Identset.Bindings.mem v !vartypes then begin (* global *)
        let ty = Identset.Bindings.find v !vartypes in
        let ref = add_simple_op loc ctx (Ref ty) (MkRef v) [] in
        let rhs' = expr_to_ir loc ctx rhs in
        add_noresult_op loc ctx Store [ref; rhs']
      end else begin
        let rhs' = expr_to_ir loc ctx rhs in
        rebind ctx v rhs'
      end
      (*
  | Stmt_Assign (LExpr_Array (LExpr_Var v, ix), rhs, loc) ->
      let ty = Identset.Bindings.find v !vartypes in
      let aref = locals#fresh in
      PP.fprintf fmt "%a = asl.address_of(@@%a) : !asl.ref<%a>@,"
        varident aref
        ident v
        (pp_type loc) ty;
      let (ix', _) = expr loc env fmt ix in
      let eref = locals#fresh in
      PP.fprintf fmt "%a = asl.array_ref(%a, %a) : !asl.ref<%a>@,"
        varident eref
        varident aref
        varident ix'
        (pp_type loc) ty;
      let (rhs', ty) = expr loc env fmt rhs in
      PP.fprintf fmt "asl.store(%a) = %a : %a@,"
        varident eref
        varident rhs'
        (pp_type loc) ty
        *)
  | Stmt_Assign (LExpr_Wildcard, rhs, loc) ->
      ignore (expr_to_ir loc ctx rhs)
  | Stmt_Block (ss, loc) ->
      let ctx' = clone_context ctx in
      List.iter (stmt_to_ir ctx') ss;
      let changes = get_rebound_variables ctx ctx' in
      List.iter (emit_op ctx) ((get_region ctx' [] []).operations);
      Scope.iter (rebind ctx) changes
  | Stmt_Return (e, loc) ->
      ( match e with
      | Expr_Tuple [] -> ()
      | _ ->
        let e' = expr_to_ir loc ctx e in
        add_noresult_op loc ctx Return [e']
      )
  | Stmt_Assert (e, loc) ->
      let e' = expr_to_ir loc ctx e in
      let msg = String.escaped (Utils.to_string2 (Fun.flip FMT.expr e)) in
      add_noresult_op loc ctx (Assert msg) [e']
  | Stmt_TCall (f, ps, args, throws, loc) ->
      let args' = List.map (expr_to_ir loc ctx) args in
      if Identset.IdentSet.mem f primitive_operations then (
        (* todo: in the backend, we expect primops to be called with fixed bitwidths *)
        add_noresult_op loc ctx (Builtin f) args'
      ) else (
        let ps' = List.map (expr_to_ir loc ctx) ps in
        add_noresult_op loc ctx (Call f) (ps' @ args')
      )
  | Stmt_If ([(c, t, loc)], (e, _), _) ->
      let c' = expr_to_ir loc ctx c in

      let t_ctx = clone_context ctx in
      List.iter (stmt_to_ir t_ctx) t;
      let t_changes = Scope.get_bindings (get_rebound_variables ctx t_ctx) in

      let e_ctx = clone_context ctx in
      List.iter (stmt_to_ir e_ctx) e;
      let e_changes = Scope.get_bindings (get_rebound_variables ctx e_ctx) in

      let changes = Identset.Bindings.merge
        (fun v ovt ove ->
            (* I believe that at least one of ovt or ove is not None and they have the same type *)
            let ty = Utils.from_option ovt (fun _ -> Option.get ove) |> HLIR.typeof in
            let vm = mk_fresh ctx ty in
            let vt' = from_option ovt (fun _ -> get_binding ctx v |> Option.get) in
            let ve' = from_option ove (fun _ -> get_binding ctx v |> Option.get) in
            Some (vm, vt', ve')
        )
        t_changes
        e_changes
      in

      let t_results = List.map (fun (v, (vm,vt,ve)) -> vt) (Identset.Bindings.bindings changes) in
      let e_results = List.map (fun (v, (vm,vt,ve)) -> ve) (Identset.Bindings.bindings changes) in

      let t_region = get_region t_ctx t_results [] in
      let e_region = get_region e_ctx e_results [] in

      let results = List.map (fun (v, (vm,vt,ve)) -> rebind ctx v vm; vm) (Identset.Bindings.bindings changes) in
      emit_op ctx { results; op=If; operands=[c']; regions=[t_region; e_region]; loc }

  | Stmt_While (cond, body, loc) ->
      let after_ctx = clone_context ctx in
      List.iter (stmt_to_ir after_ctx) body;
      let after_changes = Scope.bindings (get_rebound_variables ctx after_ctx) in

      (* TRICKY DETAIL:
       * The inputs to the after region have the same names as the bindings
       * of the variables before the loop.
       * But, they are distinct variables because they exist in the nested
       * scope of the after region.
       *
       * (Trying to allocate completely fresh names for them would require us
       * to have a list of all modified variables before starting translation
       * of the body.)
       *)
      let after_inputs = List.map (fun (v, _) -> get_binding ctx v |> Option.get) after_changes in
      let after_results = List.map snd after_changes in

      (* use the list of variables modified in the body as the initial values
       * required by the condition block
       *)
      let before_ctx = clone_context ctx in
      let cond' = expr_to_ir loc before_ctx cond in
      let before_inputs = List.map (fun (v, v') ->
             let vi = mk_fresh ctx (HLIR.typeof v') in
             rebind before_ctx v vi;
             vi
           )
           after_changes
      in
      let before_outputs = cond' :: before_inputs in

      let results = List.map (fun (v, v') ->
             let vm = mk_fresh ctx (HLIR.typeof v') in
             rebind ctx v vm;
             vm
           )
           after_changes
      in
      let before_region = get_region before_ctx before_outputs before_inputs in
      let after_region = get_region after_ctx after_results after_inputs in
      emit_op ctx { results; op=While; operands=after_inputs; regions=[before_region; after_region]; loc }
  | _ ->
      let pp fmt = FMT.stmt fmt x in
      raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

let declaration_to_ir (x : AST.declaration) : HLIR.global option =
  ( match x with
  | Decl_BuiltinType _
  | Decl_BuiltinFunction _
  | Decl_Forward _
  | Decl_Operator1 _
  | Decl_Operator2 _
  | Decl_FunType _
    -> None
  | Decl_Var (v, ty, loc) ->
      Some (HLIR.Variable(v, ty, loc))
  | Decl_FunDefn (f, fty, body, loc) ->
      let ctx = fresh_context () in
      let ps' = List.map (fun (v, ot) ->
          let t' = HLIR.mkType (Option.get ot) in
          let v' = mk_fresh ctx t' in
          add_initial_binding ctx v v';
          v'
        )
        fty.parameters
      in
      let args' = List.map (fun (v, t, _) ->
          let t' = HLIR.mkType t in
          let v' = mk_fresh ctx t' in
          add_initial_binding ctx v v';
          v'
        )
        fty.args
      in
      List.iter (stmt_to_ir ctx) body;
      let inputs = ps' @ args' in
      let outputs = [] in (* todo: I guess these come from the return type or from the return stmts? *)
      let r = get_region ctx inputs outputs in
      Some (HLIR.Function(f, r, loc))
  | _ ->
      None
  )

(****************************************************************
 * Primop support
 ****************************************************************)

let standard_functions = Identset.IdentSet.of_list [
  Builtin_idents.asl_file_open;
  Builtin_idents.asl_file_write;
  Builtin_idents.asl_file_getc;
  Builtin_idents.asl_fuzz;
  Builtin_idents.print_int_hex;
  Builtin_idents.print_int_dec;
  Builtin_idents.print_char;
  Builtin_idents.print_str;
  Builtin_idents.print_bits
]

let arith_functions = Identset.mk_bindings [
  ( Builtin_idents.eq_sintN,         "arith.cmpi eq,");
  ( Builtin_idents.ne_sintN,         "arith.cmpi ne,");
  ( Builtin_idents.gt_sintN,         "arith.cmpi sgt,");
  ( Builtin_idents.ge_sintN,         "arith.cmpi sge,");
  ( Builtin_idents.le_sintN,         "arith.cmpi sle,");
  ( Builtin_idents.lt_sintN,         "arith.cmpi slt,");
  ( Builtin_idents.add_sintN,        "arith.addi");
  ( Builtin_idents.sub_sintN,        "arith.subi");
  ( Builtin_idents.shl_sintN,        "arith.shli");
  ( Builtin_idents.shr_sintN,        "arith.shrsi");
  ( Builtin_idents.mul_sintN,        "arith.muli");
  ( Builtin_idents.exact_div_sintN,  "arith.divsi");
  ( Builtin_idents.zdiv_sintN,       "arith.divsi");
  ( Builtin_idents.zrem_sintN,       "arith.remsi");
  ( Builtin_idents.fdiv_sintN,       "arith.floordivsi")
  (*
  ( Builtin_idents.neg_sintN,        "");
  ( Builtin_idents.frem_sintN,       "");
  ( Builtin_idents.is_pow2_sintN,    "");
  ( Builtin_idents.pow2_sintN,       "");
  ( Builtin_idents.align_sintN,      "");
  ( Builtin_idents.mod_pow2_sintN,   "");
  ( Builtin_idents.cvt_sintN_bits,   "");
  ( Builtin_idents.cvt_bits_ssintN,  "");
  ( Builtin_idents.cvt_bits_usintN,  "");
  ( Builtin_idents.cvt_sintN_int,    "");
  ( Builtin_idents.cvt_int_sintN,    "");
  ( Builtin_idents.resize_sintN,     "");
  *)
]

let prim_name (fmt : PP.formatter) (x : Ident.t) : unit =
  let nm = Ident.name x in 
  if String.starts_with ~prefix:"asl_" nm then begin
    PP.fprintf fmt "asl.%s" (Utils.string_drop 4 nm)
  end else begin
    PP.fprintf fmt "asl.%s" nm
  end

(****************************************************************
 * 
 ****************************************************************)

(* the environment tracks the following about local variables
 * - for mutable variables, what SSA variable holds its current value
 * - is it a mutable variable (needed for uninitialized variables)
 * - their type
 *)

type env_entry = (Ident.t option * bool * AST.ty)
type environment = env_entry ScopeStack.t

let ppEnv (fmt : PP.formatter) (env : environment) : unit =
  let pp_entry (fmt : PP.formatter) (x : env_entry) : unit =
      let (v, mut, ty) = x in
      PP.fprintf fmt "%a :: %a "
        (Format.pp_print_option varident) v
        FMT.ty ty;
      if mut then PP.fprintf fmt " mutable"
  in
  PP.fprintf fmt "{ %a }" (ScopeStack.pp pp_entry) env

let locals = new Asl_utils.nameSupply "%"

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : AST.ty =
  ( match x with
  | VInt v ->
      PP.fprintf fmt "asl.constant_int %s" (Z.to_string v);
      Asl_utils.type_integer
  | VIntN v ->
      PP.fprintf fmt "arith.constant %s : i%d" (Z.to_string v.v) v.n;
      Asl_utils.type_sintN (mk_litint v.n)
  | VBits v ->
      PP.fprintf fmt "asl.constant_bits %s : !asl.bits<%d>" (Z.to_string v.v) v.n;
      Asl_utils.type_bits (Asl_utils.mk_litint v.n)
  | VString v ->
      PP.fprintf fmt "asl.constant_string \"%s\"" (String.escaped v);
      Asl_utils.type_string
  | _ -> raise (InternalError (loc, "valueLit", (fun fmt -> Value.pp_value fmt x), __LOC__))
  )

let mk_bool_const (fmt : PP.formatter) (x : bool) : (Ident.t * AST.ty) =
  let t = locals#fresh in
  let x' = if x then 1 else 0 in
  PP.fprintf fmt "%a = arith.constant %d : i1@," varident t x';
  (t, Asl_utils.type_bool)

(* Todo: the following is a hack that can only cope with a few simple kinds of expression
 * that appear in types but this is definitely not sufficient to express all ASL types.
 *
 * The problem we face is that an expression like "N + 1" would normally
 * be flattened to
 *
 *   %t0 = 1
 *   %t1 = asl.add_int %N %t0
 *
 * and it is not clear where that sequence of statements should go in the places
 * where types can occur
 *)
let simple_expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  ( match x with
  | Expr_Lit (VInt v) -> PP.pp_print_string fmt (Z.to_string v)
  | Expr_TApply (f, [], [Expr_Lit (VInt x); Expr_Lit (VInt y)], _) when Ident.equal f Builtin_idents.add_int ->
    (* hack to handle append primop *)
    PP.pp_print_string fmt (Z.to_string (Z.add x y))
  | Expr_TApply (f, [], [Expr_Lit (VInt x); Expr_Lit (VInt y)], _) when Ident.equal f Builtin_idents.mul_int ->
    (* hack to handle replicate primop *)
    PP.pp_print_string fmt (Z.to_string (Z.mul x y))
  | Expr_Var v -> varident fmt v
  | _ ->
      let msg = Format.asprintf "simple_expr: overly complex expression" in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

let simple_exprs (loc : Loc.t) (fmt : PP.formatter) (xs : AST.expr list) : unit =
  commasep (simple_expr loc) fmt xs

let parameters (loc : Loc.t) (fmt : PP.formatter) (ps : AST.expr list) : unit =
  if Utils.is_empty ps then begin
    ()
  end else begin
    PP.fprintf fmt "<%a>"
      (simple_exprs loc) ps
  end

let constraints (loc : Loc.t) (fmt : PP.formatter) (x : AST.constraint_range list) : unit =
  ()

let rec pp_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
  ( match x with
  | Type_Bits (e, _) -> PP.fprintf fmt "!asl.bits<%a>" (simple_expr loc) e
  | Type_Constructor (tc, []) when tc = Builtin_idents.boolean_ident ->
      PP.fprintf fmt "i1"
  | Type_Constructor (tc, []) when tc = Builtin_idents.real_ident ->
      PP.fprintf fmt "!asl.real"
  | Type_Constructor (tc, []) when tc = Builtin_idents.string_ident ->
      PP.fprintf fmt "!asl.string"
  | Type_Constructor (tc, []) when tc = Builtin_idents.ram ->
      PP.fprintf fmt "!asl.ram"
  | Type_Constructor (tc, []) when Identset.IdentSet.mem tc !enum_types ->
      PP.fprintf fmt "i%d" enum_size
  | Type_Constructor (i, [n]) when Ident.equal i Builtin_idents.sintN ->
      PP.fprintf fmt "i%a" (simple_expr loc) n
  | Type_Constructor (tc, []) ->
      PP.fprintf fmt "%a" ident tc
  | Type_Constructor (tc, ps) ->
      PP.fprintf fmt "%a<%a>"
        ident tc
        (simple_exprs loc) ps
  | Type_Integer ocrs ->
      ( match ocrs with
      | Some crs -> PP.fprintf fmt "!asl.int%a" (constraints loc) crs
      | None -> PP.fprintf fmt "!asl.int"
      )
  | Type_Array (Index_Int ixty, elty) ->
      PP.fprintf fmt "!asl.array<%a x %a>"
        (simple_expr loc) ixty
        (pp_type loc) elty
  | Type_Tuple tys ->
      PP.fprintf fmt "(%a)"
        (commasep (pp_type loc)) tys
  | _ ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "type", pp))
  )

let formal_param (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty option)) : unit =
  let (v, ot) = x in
  (* Note: formal parameters are assumed to be integer (with no constraints) *)
  PP.fprintf fmt "%a"
    varident v

let formal_params (loc : Loc.t) (fmt : PP.formatter) (xs : (Ident.t * AST.ty option) list) : unit =
  if Utils.is_empty xs then begin
    ()
  end else begin
    PP.fprintf fmt "<%a>"
      (commasep (formal_param loc)) xs
  end

let formal_arg (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty * AST.expr option)) : unit =
  let (v, t, _) = x in
  PP.fprintf fmt "%a : %a"
    varident v
    (pp_type loc) t

let pp_arg_type (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty * AST.expr option)) : unit =
  let (_, t, _) = x in
  PP.fprintf fmt "%a"
    (pp_type loc) t

let pp_arg_types (loc : Loc.t) (fmt : PP.formatter) (xs : (Ident.t * AST.ty * AST.expr option) list) : unit =
  ( match xs with
  | [] -> ()
  | [x] -> pp_arg_type loc fmt x
  | _ -> PP.fprintf fmt "(%a)" (commasep (pp_arg_type loc)) xs
  )

let pp_return_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
  pp_type loc fmt x

let prim_funtype (loc : Loc.t) (fmt : PP.formatter) (x : AST.function_type) : unit =
  PP.fprintf fmt "%a -> %a"
    (pp_arg_types loc) x.args
    (pp_return_type loc) x.rty

let funtype (loc : Loc.t) (fmt : PP.formatter) (x : AST.function_type) : unit =
  PP.fprintf fmt "(%a) -> %a"
    (commasep (pp_arg_type loc)) x.args
    (pp_return_type loc) x.rty

let rec prim_apply (loc : Loc.t) (env : environment) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : (Ident.t * AST.ty) =
  let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
  begin if not (Identset.Bindings.mem f !funtypes) then PP.fprintf fmt "// Can't find %a@," ident f end;
  let fty = Identset.Bindings.find f !funtypes in
  let fty' = instantiate_funtype ps fty in
  let t = locals#fresh in
  PP.fprintf fmt "%a = %a %a : %a@,"
    varident t
    prim_name f
    (commasep varident) avs
    (prim_funtype loc) fty';
  (t, fty'.rty)

and user_apply (loc : Loc.t) (env : environment) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : (Ident.t * AST.ty) =
  let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
  let fty = Identset.Bindings.find f !funtypes in
  let fty' = instantiate_funtype ps fty in
  let t = locals#fresh in
  PP.fprintf fmt "%a = asl.call @%a%a(%a) : %a@,"
    varident t
    ident f
    (parameters loc) ps
    (commasep varident) avs
    (funtype loc) fty';
  (t, fty'.rty)

and mk_binop (loc : Loc.t) (fmt : PP.formatter) (op : string) (x : Ident.t) (y : Ident.t) (ty : AST.ty) : Ident.t =
  let t = locals#fresh in
  PP.fprintf fmt "%a = %s %a, %a : %a@,"
    varident t
    op
    varident x
    varident y
    (pp_type loc) ty;
  t

and mk_ite (loc : Loc.t) (env : environment) (fmt : PP.formatter)
    (c : AST.expr)
    (mk_then : PP.formatter -> (Ident.t * AST.ty))
    (mk_else : PP.formatter -> (Ident.t * AST.ty))
    : (Ident.t * AST.ty)
  =
  let (c', _) = expr loc env fmt c in
  let t = locals#fresh in
  let (_, rty) = mk_then Utils.null_formatter in
  PP.fprintf fmt "%a = scf.if %a -> (%a) {"
    varident t
    varident c'
    (pp_type loc) rty;
  let xty = indented fmt (fun _ ->
    let (x', xty) = mk_then fmt in
    PP.fprintf fmt "scf.yield %a : %a"
      varident x'
      (pp_type loc) xty;
    xty
  ) in
  PP.fprintf fmt "@,} else {";
  let _ = indented fmt (fun _ ->
    let (y', yty) = mk_else fmt in
    PP.fprintf fmt "scf.yield %a : %a"
      varident y'
      (pp_type loc) yty;
    yty
  ) in
  PP.fprintf fmt "@,}@,";
  (t, xty)

and expr (loc : Loc.t) (env : environment) (fmt : PP.formatter) (x : AST.expr) : (Ident.t * AST.ty) =
  ( match x with
  | Expr_Lit l ->
      let v = locals#fresh in
      PP.fprintf fmt "%a = " varident v;
      let t = valueLit loc fmt l in
      PP.fprintf fmt "@,";
      (v, t)
  | Expr_Slices (Type_Bits (Expr_Lit (VInt m), _), x, [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) ->
      let (x', _) = expr loc env fmt x in
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.get_slice %a, %a, %a : (!asl.bits<%s>, !asl.int, !asl.int) -> !asl.bits<%s>@,"
        varident t
        varident x'
        varident lo'
        varident wd'
        (Z.to_string m)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  | Expr_Slices (Type_Integer _, Expr_Lit (VInt v), [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) when lo = Asl_utils.zero ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits %s : !asl.bits<%s>@,"
        ident t
        (Z.to_string v)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  | Expr_Var v when Ident.equal v Builtin_idents.false_ident ->
      mk_bool_const fmt false
  | Expr_Var v when Ident.equal v Builtin_idents.true_ident ->
      mk_bool_const fmt true
  | Expr_Var v when Identset.Bindings.mem v !enums ->
      let value = Identset.Bindings.find v !enums in
      let t = locals#fresh in
      PP.fprintf fmt "%a = arith.constant %d : i%d@,"
        varident t
        value
        enum_size;
      (t, Asl_utils.type_bits (Asl_utils.mk_litint enum_size))
  | Expr_Var v ->
      ( match ScopeStack.get env v with
      | None -> (* global variable *)
          assert (Identset.Bindings.mem v !vartypes);
          let ty = Identset.Bindings.find v !vartypes in
          let ref = locals#fresh in
          PP.fprintf fmt "%a = asl.address_of(@@%a) : !asl.ref<%a>@,"
            varident ref
            ident v
            (pp_type loc) ty;
          let t = locals#fresh in
          PP.fprintf fmt "%a = asl.load(%a) : %a@,"
            varident t
            varident ref
            (pp_type loc) ty;
          (t, ty)
      | Some (None, mut, ty) -> (v, ty) (* immutable variable *)
      | Some (Some v', mut, ty) -> (v', ty)
      )
  | Expr_Array (Expr_Var v, ix) ->
      (* todo: handle local and 2-D arrays too *)
      let ty = Identset.Bindings.find v !vartypes in
      let (ixty, elty) = ( match ty with
        | Type_Array (Index_Int ixty, elty) -> (ixty, elty)
        | _ -> raise (InternalError (loc, "Array read", (fun fmt -> FMT.ty fmt ty), __LOC__))
        )
      in
      let aref = locals#fresh in
      PP.fprintf fmt "%a = asl.address_of(@@%a) : !asl.ref<%a>@,"
        varident aref
        ident v
        (pp_type loc) ty;
      let (ix', _) = expr loc env fmt ix in
      let eref = locals#fresh in
      PP.fprintf fmt "%a = asl.array_ref(%a, %a) : !asl.ref<%a>@,"
        varident eref
        varident aref
        varident ix'
        (pp_type loc) ty;
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.load(%a) : %a@,"
        varident t
        varident eref
        (pp_type loc) ty;
      (t, elty)
  | Expr_If (els, e) ->
      let rec mk_ites els e : (Ident.t * AST.ty) =
        ( match els with
        | [] -> expr loc env fmt e
        | (c,t)::els' ->
           mk_ite loc env fmt c
             (fun fmt -> expr loc env fmt t)
             (fun fmt -> mk_ites els' e)
        )
      in mk_ites els e
  | Expr_In (e, Pat_Lit (VMask mask)) ->
      let (e', ty) = expr loc env fmt e in
      let v = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits %s : !asl.bits<%d>@," varident v (Z.to_string mask.v) mask.n;
      let m = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits %s : !asl.bits<%d>@," varident m (Z.to_string mask.m) mask.n;
      let masked = locals#fresh in
      PP.fprintf fmt "%a = asl.and_bits(%a, %a) : !asl.bits<%d>@,"
        varident masked
        varident e'
        varident m
        mask.n;
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.eq_bits(%a, %a) : !asl.boolean@,"
        varident t
        varident masked
        varident v;
      (t, type_bool)
  | Expr_TApply (f, ps, args, NoThrow) when Identset.Bindings.mem f arith_functions ->
      let f' = Identset.Bindings.find f arith_functions in
      let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
      let fty = Identset.Bindings.find f !funtypes in
      let fty' = instantiate_funtype ps fty in
      let snd3 (x,y,z) = y in
      let ty = snd3 (List.hd fty'.args) in
      let t = locals#fresh in
      PP.fprintf fmt "%a = %s %a : %a@,"
        varident t
        f'
        (commasep varident) avs
        (pp_type loc) ty;
      (t, fty'.rty)
  | Expr_TApply (f, [], [x], NoThrow) when Ident.equal f Builtin_idents.not_bool ->
      let (x', xty) = expr loc env fmt x in
      let (one, _) = mk_bool_const fmt true in
      (mk_binop loc fmt "arith.subi" one x' Asl_utils.type_bool, Asl_utils.type_bool)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.lazy_and_bool ->
      mk_ite loc env fmt
        x
        (fun fmt -> expr loc env fmt y)
        (fun fmt -> mk_bool_const fmt false)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.lazy_or_bool ->
      mk_ite loc env fmt
        x
        (fun fmt -> mk_bool_const fmt true)
        (fun fmt -> expr loc env fmt y)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.implies_bool ->
      mk_ite loc env fmt
        x
        (fun fmt -> expr loc env fmt y)
        (fun fmt -> mk_bool_const fmt true)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.eq_bool ->
      let (x', _) = expr loc env fmt x in
      let (y', _) = expr loc env fmt y in
      (mk_binop loc fmt "arith.cmpi eq," x' y' Asl_utils.type_bool, Asl_utils.type_bool)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.ne_bool ->
      let (x', _) = expr loc env fmt x in
      let (y', _) = expr loc env fmt y in
      (mk_binop loc fmt "arith.cmpi ne," x' y' Asl_utils.type_bool, Asl_utils.type_bool)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.matches f ~name:"asl_eq_enum" ->
      let (x', xty) = expr loc env fmt x in
      let (y', yty) = expr loc env fmt y in
      (mk_binop loc fmt "arith.cmpi eq," x' y' xty, Asl_utils.type_bool)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.matches f ~name:"asl_ne_enum" ->
      let (x', xty) = expr loc env fmt x in
      let (y', yty) = expr loc env fmt y in
      (mk_binop loc fmt "arith.cmpi ne," x' y' xty, Asl_utils.type_bool)
  | Expr_TApply (f, [n], [_], NoThrow) when Ident.equal f Builtin_idents.zeros_bits ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits 0 : !asl.bits<%a>@,"
        varident t
        (simple_expr loc) n;
      (t, Asl_utils.type_bits n)
  | Expr_TApply (f, [n], [_], NoThrow) when Ident.equal f Builtin_idents.ones_bits ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits -1 : !asl.bits<%a>@,"
        varident t
        (simple_expr loc) n;
      (t, Asl_utils.type_bits n)
  | Expr_TApply (f, ps, args, throws) ->
      if Identset.IdentSet.mem f primitive_operations then (
        prim_apply loc env fmt f ps args
      ) else (
        user_apply loc env fmt f ps args
      )
  | _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

let return_type = ref (AST.Type_Tuple [])

let prim_call (loc : Loc.t) (env : environment) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : unit =
  let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
  let fty = Identset.Bindings.find f !funtypes in
  PP.fprintf fmt "%a %a : %a@,"
    prim_name f
    (commasep varident) avs
    (prim_funtype loc) (instantiate_funtype ps fty)

let user_call (loc : Loc.t) (env : environment) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : unit =
  let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
  let fty = Identset.Bindings.find f !funtypes in
  PP.fprintf fmt "asl.call @%a%a(%a) : %a@,"
    ident f
    (parameters loc) ps
    (commasep varident) avs
    (funtype loc) (instantiate_funtype ps fty)

(* Since ASL code tends to have few mutable vars, we use all mutable vars as
 * an approximation of the set of variables modified by this if.
 *)
type renaming = (Ident.t * Ident.t * AST.ty) list

let mkRenaming (env : environment) : renaming =
  ScopeStack.bindings env
  |> List.concat_map (List.filter_map (fun (v, (b, mut, ty)) ->
       if mut then (
         let v' = locals#fresh in
         Some (v, v', ty)
       ) else (
         None
       )))

let ppReturnVars (fmt : PP.formatter) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    PP.fprintf fmt "%a = "
      (commasep varident) fresh_vars
  end

let ppBlockReturnType (loc : Loc.t) (fmt : PP.formatter) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    PP.fprintf fmt "-> (%a) "
      (commasep (pp_type loc)) types
  end

let ppYield (loc : Loc.t) (env : environment) (fmt : PP.formatter) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    let get_mutbind (v : Ident.t) : Ident.t =
      ( match ScopeStack.get env v with
      | Some (Some v', _, _) -> v'
      | _ -> raise (InternalError (loc, "Stmt_If", (fun fmt -> Ident.pp fmt v), __LOC__))
      )
    in
    let vars' = List.map get_mutbind vars in
    PP.fprintf fmt "scf.yield %a : %a@,"
      (commasep varident) vars'
      (commasep (pp_type loc)) types
  end

let update_environment (env : environment) (rename : renaming) =
  List.iter (fun (v, v', ty) ->
      ScopeStack.add env v (Some v', true, ty)
    )
    rename

let pattern (loc : Loc.t) (fmt : PP.formatter) (x : AST.pattern) : unit =
  ( match x with
  | Pat_Lit (VInt c) ->
    PP.fprintf fmt "%s" (Z.format "%d" c)
  | Pat_Lit (VBits c) ->
    PP.fprintf fmt "%s" (Z.format "%d" c.v)
  | Pat_Const v when Ident.equal v Builtin_idents.false_ident -> PP.fprintf fmt "0"
  | Pat_Const v when Ident.equal v Builtin_idents.true_ident -> PP.fprintf fmt "1"
  | Pat_Const v when Identset.Bindings.mem v !enums ->
      let value = Identset.Bindings.find v !enums in
      PP.fprintf fmt "%d" value
  | Pat_Lit _ | Pat_Const _
  | Pat_Range _ | Pat_Set _ | Pat_Single _
  | Pat_Tuple _ | Pat_Wildcard ->
      let pp fmt = FMT.pattern fmt x in
      raise (Error.Unimplemented (loc, "pattern", pp))
  )

let rec stmt (env : environment) (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDecl (is_constant, DeclItem_Var (v, _), i, loc) ->
      let (i', ty) = expr loc env fmt i in
      ScopeStack.add env v (Some i', not is_constant, ty)
  | Stmt_VarDecl (is_constant, DeclItem_Wildcard _, i, loc) ->
      ignore (expr loc env fmt i)
  | Stmt_Assign (LExpr_Var v, rhs, loc) ->
      if Identset.Bindings.mem v !vartypes then begin (* global *)
        let ty = Identset.Bindings.find v !vartypes in
        let ref = locals#fresh in
        PP.fprintf fmt "%a = asl.address_of(@@%a) : !asl.ref<%a>@,"
          varident ref
          ident v
          (pp_type loc) ty;
        let (rhs', ty) = expr loc env fmt rhs in
        PP.fprintf fmt "asl.store(%a) = %a : %a@,"
          varident ref
          varident rhs'
          (pp_type loc) ty
      end else begin
        let (rhs', ty) = expr loc env fmt rhs in
        ignore (ScopeStack.set env v (Some rhs', true, ty))
      end
  | Stmt_Assign (LExpr_Array (LExpr_Var v, ix), rhs, loc) ->
      let ty = Identset.Bindings.find v !vartypes in
      let aref = locals#fresh in
      PP.fprintf fmt "%a = asl.address_of(@@%a) : !asl.ref<%a>@,"
        varident aref
        ident v
        (pp_type loc) ty;
      let (ix', _) = expr loc env fmt ix in
      let eref = locals#fresh in
      PP.fprintf fmt "%a = asl.array_ref(%a, %a) : !asl.ref<%a>@,"
        varident eref
        varident aref
        varident ix'
        (pp_type loc) ty;
      let (rhs', ty) = expr loc env fmt rhs in
      PP.fprintf fmt "asl.store(%a) = %a : %a@,"
        varident eref
        varident rhs'
        (pp_type loc) ty
  | Stmt_Assign (LExpr_Wildcard, rhs, loc) ->
      ignore (expr loc env fmt rhs)
  | Stmt_Block (ss, loc) ->
      cutsep (stmt env) fmt ss
  | Stmt_Return (e, loc) ->
      ( match e with
      | Expr_Tuple [] -> ()
      | _ ->
        let (t, _) = expr loc env fmt e in
        PP.fprintf fmt "asl.return %a : %a@."
          varident t
          (pp_type loc) !return_type
      )
  | Stmt_Assert (e, loc) ->
      let (t, _) = expr loc env fmt e in
      PP.fprintf fmt "asl.assert \"%s\", \"%s\", %a@."
        (String.escaped (Loc.to_string loc))
        (String.escaped (Utils.to_string2 (Fun.flip FMT.expr e)))
        varident t
  | Stmt_TCall (f, ps, args, throws, loc) ->
      if Identset.IdentSet.mem f primitive_operations then begin
        prim_call loc env fmt f ps args
      end else begin
        user_call loc env fmt f ps args
      end
  | Stmt_If (els, (e, _), loc) ->
      let rec ppIfs (env : environment) (els : AST.s_elsif list) (e : AST.stmt list) : unit =
        ( match els with
        | [] ->
            indented_block env fmt e
        | ((c, t, loc) :: els') ->
            let (c', _) = expr loc env fmt c in
            let rename = mkRenaming env in
            let envt = ScopeStack.clone env in
            ppReturnVars fmt rename;
            PP.fprintf fmt "scf.if %a " varident c';
            ppBlockReturnType loc fmt rename;
            PP.fprintf fmt "{@,";
            indented_block envt fmt t;
            ppYield loc envt fmt rename;
            PP.fprintf fmt "@,} else {@,";
            let enve = ScopeStack.clone env in
            indented fmt (fun _ -> ppIfs enve els' e);
            ppYield loc enve fmt rename;
            PP.fprintf fmt "@,}@,@,";
            update_environment env rename
        )
      in
      ppIfs env els e

  | Stmt_Case (e, Some ty, alts, ob, loc) ->
      vbox fmt (fun _ ->
        let (e', _) = expr loc env fmt e in
        let rename = mkRenaming env in
        ppReturnVars fmt rename;
        PP.fprintf fmt "scf.index_switch %a " varident e';
        ppBlockReturnType loc fmt rename;
        PP.fprintf fmt "@,";
        List.iter
          (fun alt ->
            ( match alt with
            | AST.Alt_Alt ([p], None, ss, loc) ->
                let env' = ScopeStack.clone env in
                PP.fprintf fmt "case %a {@," (pattern loc) p;
                indented_block env' fmt ss;
                ppYield loc env' fmt rename;
                PP.fprintf fmt "}@,";
            | _ ->
                let pp fmt = FMT.stmt fmt x in
                raise (Error.Unimplemented (loc, "case-alt", pp))
            )
          )
          alts;
        PP.fprintf fmt "default {@,";
        ( match ob with
        | Some (b, _) ->
            let env' = ScopeStack.clone env in
            indented_block env' fmt b;
            ppYield loc env' fmt rename;
        | None ->
            (* todo: should assert false *)
            ppYield loc env fmt rename;
        );
        PP.fprintf fmt "@,}@,";
        update_environment env rename
      )
  | Stmt_For (ix, ty, efrom, Direction_Up, eto, b, loc) ->
      (* Since ASL code tends to have few mutable vars, we use all mutable vars as
       * an approximation of the set of variables modified by this if.
       *)
      let get_mutbind (v : Ident.t) : Ident.t =
        ( match ScopeStack.get env v with
        | Some (Some v', _, _) -> v'
        | _ -> raise (InternalError (loc, "Stmt_For", (fun fmt -> Ident.pp fmt v), __LOC__))
        )
      in
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (ob, mut, ty)) ->
             ( match ob with
             | Some current ->
                 let loopv = locals#fresh in
                 let finalv = locals#fresh in
                 Some (v, current, loopv, finalv, ty)
             | None ->
                 None
             )))
      in
      let (from', _) = expr loc env fmt efrom in
      let (to', _) = expr loc env fmt eto in
      let step = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_int 1@," varident step;
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) (List.map (fun (_, _, _, finalv, _) -> finalv) mutables)
      end;
      (* todo: deal with mismatch in index type *)
      PP.fprintf fmt "scf.for %a = %a to %a step %a"
        varident ix
        varident from'
        varident to'
        varident step;
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt " iter_args (";
        commasep (fun fmt (v, current, loopv, _, _) -> PP.fprintf fmt "%a = %a"
            varident loopv
            varident current)
          fmt
          mutables;
        PP.fprintf fmt ") -> (%a)"
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt " {";
      ScopeStack.nest env (fun env' ->
        ScopeStack.add env' ix (None, false, Asl_utils.type_integer);
        indented_block env' fmt b);
      if Utils.is_empty mutables then begin
        PP.fprintf fmt "scf.yield";
      end else begin
        PP.fprintf fmt "scf.yield %a : %a"
          (commasep varident) (List.map (fun (v, _, _, _, _) -> get_mutbind v) mutables)
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt "@,}@,@,";
      List.iter (fun (v, _, _, finalv, ty) ->
          ScopeStack.add env v (Some finalv, true, ty)
        )
        mutables
  | Stmt_While (cond, b, loc) ->
      (* Since ASL code tends to have few mutable vars, we use all mutable vars as
       * an approximation of the set of variables modified by this if.
       *)
      let get_mutbind (env : environment) (v : Ident.t) : Ident.t =
        ( match ScopeStack.get env v with
        | Some (Some v', _, _) -> v'
        | _ -> raise (InternalError (loc, "Stmt_For", (fun fmt -> Ident.pp fmt v), __LOC__))
        )
      in
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (ob, mut, ty)) ->
             ( match ob with
             | Some current ->
                 let loopv1 = locals#fresh in
                 let loopv2 = locals#fresh in
                 let finalv = locals#fresh in
                 Some (v, current, loopv1, loopv2, finalv, ty)
             | None ->
                 None
             )))
      in
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) (List.map (fun (_, _, _, _, finalv, _) -> finalv) mutables)
      end;
      PP.fprintf fmt "scf.while (";
      commasep (fun fmt (v, current, loopv1, _, _, _) -> PP.fprintf fmt "%a = %a"
          varident loopv1
          varident current)
        fmt
        mutables;
      PP.fprintf fmt ") {";
      indented fmt (fun _ ->
        let (cond', _) = expr loc env fmt cond in
        PP.fprintf fmt "scf.condition(%a) %a"
          varident cond'
          (commasep varident) (List.map (fun (v, _, _, _, _, ty) -> get_mutbind env v) mutables)
      );
      PP.fprintf fmt "} do {";
      PP.fprintf fmt "^bb(%a):"
        (commasep (fun fmt (v, _, _, loopv2, _, ty) -> PP.fprintf fmt "%a: %a"
            varident loopv2
            (pp_type loc) ty))
        mutables;
      indented fmt (fun _ ->
        indented_block env fmt b;
        PP.fprintf fmt "scf.yield ";
        commasep (fun fmt (v, _, _, loopv2, _, ty) -> PP.fprintf fmt "%a: %a"
            varident (get_mutbind env loopv2)
            (pp_type loc) ty)
          fmt
          mutables
      );
      PP.fprintf fmt "}@,@,"
  | _ ->
      let pp fmt = FMT.stmt fmt x in
      raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and indented_block (env : environment) (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ ->
      ScopeStack.nest env (fun env' ->
        cutsep (stmt env) fmt xs))
  end

let declaration (fmt : PP.formatter) ?(is_extern : bool option) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      ( match x with
      | Decl_BuiltinType _
      | Decl_BuiltinFunction _
      | Decl_Forward _
      | Decl_Operator1 _
      | Decl_Operator2 _
      | Decl_FunType _
        -> ()
      | Decl_FunDefn (f, fty, b, loc) ->
          locals#reset;
          let env : environment = ScopeStack.empty () in
          List.iter (fun (v, oty) -> ScopeStack.add env v (None, false, Option.get oty)) fty.parameters;
          List.iter (fun (v, ty, _) -> ScopeStack.add env v (None, false, ty)) fty.args;
          PP.fprintf fmt "asl.func @%a%a(%a) -> %a {"
            ident f
            (formal_params loc) fty.parameters
            (commasep (formal_arg loc)) fty.args
            (pp_return_type loc) fty.rty;
          return_type := fty.rty;
          indented_block env fmt b;
          ( match fty.rty with
          | Type_Tuple([]) -> PP.fprintf fmt "asl.return@."
          | _ -> ()
          );
          PP.fprintf fmt "}@.@."
      | Decl_Var (v, ty, loc) ->
          PP.fprintf fmt "asl.global \"%a\" : %a@.@."
            ident v
            (pp_type loc) ty
      | _ ->
          ( match Asl_utils.decl_name x with
          | Some nm -> PP.fprintf fmt "// skipping %a\n" ident nm
          | None -> ()
          )
      ))

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration fmt) xs)

(****************************************************************
 * Command: :generate_mlir
 ****************************************************************)

let _ =
  let opt_filename = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Utils.to_file !opt_filename (fun fmt ->
      let decls = !Commands.declarations in

      (* record function types *)
      List.iter (fun d ->
        ( match d with
        | AST.Decl_BuiltinFunction (f, fty, _)
        | AST.Decl_FunType (f, fty, _)
        | AST.Decl_FunDefn (f, fty, _, _)
        -> funtypes := Identset.Bindings.add f fty !funtypes
        | AST.Decl_Var (v, ty, _)
        -> vartypes := Identset.Bindings.add v ty !vartypes
        | _ -> ()
        )
      ) decls;

      (* record enumeration constants *)
      List.iter (fun d ->
        ( match d with
        | AST.Decl_Enum (tc, es, loc)
        ->
           enum_types := Identset.IdentSet.add tc !enum_types;
           List.iteri (fun i e -> enums := Identset.Bindings.add e i !enums) es
        | _ -> ()
        )
      ) decls;

      Identset.IdentSet.iter (fun f -> 
        ( match Identset.Bindings.find_opt f !funtypes with
        | None -> ()
        | Some fty ->
            let loc = Loc.Unknown in
            PP.fprintf fmt "asl.func @%a%a(%a) -> %a@,"
              ident f
              (formal_params loc) fty.parameters
              (commasep (formal_arg loc)) fty.args
              (pp_return_type loc) fty.rty;
        )
      ) standard_functions;

      List.iter (fun d ->
          let ir = declaration_to_ir d in
          Option.iter (HLIR.ppGlobal Format.std_formatter) ir
        )
        decls;
      PP.fprintf fmt "@,";
      declarations fmt (List.rev decls)
    );
    true
  in

  let flags = Arg.align [
        ("--output-file",   Arg.Set_string opt_filename,         "<filename>    Output MLIR file");
      ]
  in
  Commands.registerCommand "generate_mlir" flags [] [] "Generate MLIR" cmd

(****************************************************************
 * End
 ****************************************************************)
