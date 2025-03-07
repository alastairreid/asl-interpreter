(****************************************************************
 * ASL to MLIR backend
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to MLIR backend *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
open Asl_utils
open Format_utils
open Utils

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

(* set of all primitive operation names - these will be preceded by asl. *)
let primitive_operations = Identset.IdentSet.of_list [
  Builtin_idents.not_bool;
  Builtin_idents.neg_int;
  Builtin_idents.not_bits;
  Builtin_idents.zeros_bits;
  Builtin_idents.ones_bits;
  Builtin_idents.eq_enum;
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
  Builtin_idents.equiv_bool;
  Builtin_idents.ne_bool;
  Builtin_idents.ne_enum;
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
  Builtin_idents.eq_sintN;
  Builtin_idents.ne_sintN;
  Builtin_idents.gt_sintN;
  Builtin_idents.ge_sintN;
  Builtin_idents.le_sintN;
  Builtin_idents.lt_sintN;
  Builtin_idents.add_sintN;
  Builtin_idents.neg_sintN;
  Builtin_idents.sub_sintN;
  Builtin_idents.shl_sintN;
  Builtin_idents.shr_sintN;
  Builtin_idents.mul_sintN;
  Builtin_idents.exact_div_sintN;
  Builtin_idents.zdiv_sintN;
  Builtin_idents.zrem_sintN;
  Builtin_idents.fdiv_sintN;
  Builtin_idents.frem_sintN;
  Builtin_idents.is_pow2_sintN;
  Builtin_idents.pow2_sintN;
  Builtin_idents.align_sintN;
  Builtin_idents.mod_pow2_sintN;
  Builtin_idents.cvt_sintN_bits;
  Builtin_idents.cvt_bits_ssintN;
  Builtin_idents.cvt_bits_usintN;
  Builtin_idents.cvt_sintN_int;
  Builtin_idents.cvt_int_sintN;
  Builtin_idents.resize_sintN;
  Builtin_idents.print_sintN_dec;
  Builtin_idents.print_sintN_hex;
  Builtin_idents.asl_extract_bits;
  Builtin_idents.asl_extract_int;
  Builtin_idents.print_bits_hex
]
 
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

let prim_name (fmt : PP.formatter) (x : Ident.t) : unit =
  let nm = Ident.name x in 
  if String.starts_with ~prefix:"asl_" nm then begin
    PP.fprintf fmt "asl.%s" (Utils.string_drop 4 nm)
  end else begin
    PP.fprintf fmt "asl.%s" nm
  end

let funtypes : AST.function_type Identset.Bindings.t ref = ref Identset.Bindings.empty

let locals = new Asl_utils.nameSupply "%"

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : AST.ty =
  ( match x with
  | VInt v ->
      PP.fprintf fmt "asl.constant_int %s {attr_dict}" (Z.to_string v);
      Asl_utils.type_integer
  | VBits v ->
      PP.fprintf fmt "asl.constant_bits %s : !asl.bits<%d> {attr_dict}" (Z.to_string v.v) v.n;
      Asl_utils.type_bits (Asl_utils.mk_litint v.n)
  | VString v ->
      PP.fprintf fmt "asl.constant_string \"%s\" {attr_dict}" (String.escaped v);
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
  if List.is_empty ps then begin
    ()
  end else begin
    PP.fprintf fmt "<%a>"
      (simple_exprs loc) ps
  end

let instantiate_funtype (ps : AST.expr list) (fty : AST.function_type) : AST.function_type =
  let env = Identset.mk_bindings (List.map2 (fun (p, _) ty -> (p, ty)) fty.parameters ps) in
  Asl_utils.subst_funtype env fty

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
  if List.is_empty xs then begin
    ()
  end else begin
    PP.fprintf fmt "<%a>"
      (commasep (formal_param loc)) xs
  end

let formal_arg (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty)) : unit =
  let (v, t) = x in
  PP.fprintf fmt "%a : %a"
    varident v
    (pp_type loc) t

let pp_arg_type (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty)) : unit =
  let (_, t) = x in
  PP.fprintf fmt "%a"
    (pp_type loc) t

let pp_arg_types (loc : Loc.t) (fmt : PP.formatter) (xs : (Ident.t * AST.ty) list) : unit =
  ( match xs with
  | [] -> ()
  | [x] -> pp_arg_type loc fmt x
  | _ -> PP.fprintf fmt "(%a)" (commasep (pp_arg_type loc)) xs
  )

let pp_return_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty option) : unit =
  PP.pp_print_option
    ~none:(fun fmt _ -> PP.fprintf fmt "()")
    (fun fmt t -> pp_type loc fmt t)
    fmt
    x

let prim_funtype (loc : Loc.t) (fmt : PP.formatter) (x : AST.function_type) : unit =
  PP.fprintf fmt "%a -> %a"
    (pp_arg_types loc) x.args
    (pp_return_type loc) x.rty

let funtype (loc : Loc.t) (fmt : PP.formatter) (x : AST.function_type) : unit =
  PP.fprintf fmt "(%a) -> %a"
    (commasep (pp_arg_type loc)) x.args
    (pp_return_type loc) x.rty

(* the environment tracks the following about local variables
 * - for mutable variables, what SSA variable holds its current value
 * - their type
 *)

type environment = (Ident.t option * AST.ty) ScopeStack.t

let rec prim_apply (loc : Loc.t) (env : environment) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : (Ident.t * AST.ty) =
  let avs = List.map (fun arg -> fst (expr loc env fmt arg)) args in
  let fty = Identset.Bindings.find f !funtypes in
  let fty' = instantiate_funtype ps fty in
  let t = locals#fresh in
  PP.fprintf fmt "%a = %a %a : %a@,"
    varident t
    prim_name f
    (commasep varident) avs
    (prim_funtype loc) fty';
  (t, Option.get fty'.rty)

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
  (t, Option.get fty'.rty)

and mk_binop (loc : Loc.t) (env : environment) (fmt : PP.formatter) (op : string) (x : AST.expr) (y : AST.expr) : Ident.t =
  let (x', _) = expr loc env fmt x in
  let (y', _) = expr loc env fmt y in
  let t = locals#fresh in
  PP.fprintf fmt "%s %a %a"
    op
    varident x'
    varident y';
  t

and mk_ite (loc : Loc.t) (env : environment) (fmt : PP.formatter)
    (c : AST.expr)
    (mk_then : 'a -> (Ident.t * AST.ty))
    (mk_else : 'b -> (Ident.t * AST.ty))
    : (Ident.t * AST.ty)
  =
  let (c', _) = expr loc env fmt c in
  let t = locals#fresh in
  PP.fprintf fmt "%a = scf.if %a {"
    varident t
    varident c';
  let xty = indented fmt (fun _ ->
    let (x', xty) = mk_then () in
    PP.fprintf fmt "scf.yield %a : %a"
      varident x'
      (pp_type loc) xty;
    xty
  ) in
  PP.fprintf fmt "@,} else {";
  let _ = indented fmt (fun _ ->
    let (y', yty) = mk_then () in
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
      PP.fprintf fmt "%a = " ident v;
      let t = valueLit loc fmt l in
      PP.fprintf fmt "@,";
      (v, t)
  | Expr_Slices (Type_Bits (Expr_Lit (VInt m), _), x, [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) ->
      let (x', _) = expr loc env fmt x in
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.get_slice %a, %a, %a : (!asl.bits<%s>, !asl.int, !asl.int) -> !asl.bits<%s> {attr_dict}@,"
        varident t
        varident x'
        varident lo'
        varident wd'
        (Z.to_string m)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  | Expr_Slices (Type_Integer _, Expr_Lit (VInt v), [Slice_LoWd (lo, (Expr_Lit (VInt n) as wd))]) when lo = Asl_utils.zero ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = asl.constant_bits %s : !asl.bits<%s> {attr_dict}@,"
        ident t
        (Z.to_string v)
        (Z.to_string n);
      (t, Asl_utils.type_bits wd)
  | Expr_Var v when Ident.equal v Builtin_idents.false_ident ->
      mk_bool_const fmt false
  | Expr_Var v when Ident.equal v Builtin_idents.true_ident ->
      mk_bool_const fmt true
  | Expr_Var v ->
      (* todo: if v is a global, it needs to be copied into a local *)
      ( match ScopeStack.get env v with
      | None -> raise (InternalError (loc, "Expr_Var", (fun fmt -> Ident.pp fmt v), __LOC__))
      | Some (None, ty) -> (v, ty) (* immutable variable *)
      | Some (Some v', ty) -> (v', ty)
      )
  | Expr_If (c, t, [], e) ->
      mk_ite loc env fmt
        c
        (fun _ -> expr loc env fmt t)
        (fun _ -> expr loc env fmt e)
  | Expr_If (c, t, AST.E_Elsif_Cond (c', t')::els, e) ->
      mk_ite loc env fmt
        c
        (fun _ -> expr loc env fmt t)
        (fun _ -> expr loc env fmt (Expr_If (c', t', els, e)))
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.and_bool ->
      mk_ite loc env fmt
        x
        (fun _ -> expr loc env fmt y)
        (fun _ -> mk_bool_const fmt false)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.or_bool ->
      mk_ite loc env fmt
        x
        (fun _ -> mk_bool_const fmt true)
        (fun _ -> expr loc env fmt y)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.implies_bool ->
      mk_ite loc env fmt
        x
        (fun _ -> expr loc env fmt y)
        (fun _ -> mk_bool_const fmt true)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.in_list f [Builtin_idents.eq_bool; Builtin_idents.equiv_bool] ->
      (mk_binop loc env fmt "arith.cmp_i eq," x y, Asl_utils.type_bool)
  | Expr_TApply (f, [], [x; y], NoThrow) when Ident.equal f Builtin_idents.ne_bool ->
      (mk_binop loc env fmt "arith.cmp_i ne," x y, Asl_utils.type_bool)
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

let rec stmt (env : environment) (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_ConstDecl (DeclItem_Var (v, _), i, loc) ->
      let (i', ty) = expr loc env fmt i in
      PP.fprintf fmt "%a = asl.copy %a : %a@,"
        varident v
        varident i'
        (pp_type loc) ty;
      ignore (ScopeStack.set env v (None, ty))
  | Stmt_ConstDecl (DeclItem_Wildcard _, i, loc) ->
      ignore (expr loc env fmt i)
  | Stmt_VarDecl (DeclItem_Var (v, _), i, loc) ->
      let (i', ty) = expr loc env fmt i in
      PP.fprintf fmt "%a = asl.copy %a : %a@,"
        varident v
        varident i'
        (pp_type loc) ty;
      ignore (ScopeStack.add env v (Some v, ty))
  | Stmt_VarDecl (DeclItem_Wildcard _, i, loc) ->
      ignore (expr loc env fmt i)
  | Stmt_Assign (LExpr_Var v, rhs, loc) ->
      let (rhs', ty) = expr loc env fmt rhs in
      (* todo: check that v is mutable *)
      ignore (ScopeStack.add env v (Some rhs', ty))
  | Stmt_Assign (LExpr_Wildcard, rhs, loc) ->
      ignore (expr loc env fmt rhs)
  | Stmt_Block (ss, loc) ->
      cutsep (stmt env) fmt ss
  | Stmt_FunReturn (e, loc) ->
      let (t, _) = expr loc env fmt e in
      PP.fprintf fmt "asl.return %a : %a@."
        varident t
        (pp_type loc) !return_type
  | Stmt_ProcReturn loc ->
      PP.fprintf fmt "asl.return@."
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
  | Stmt_If (c, t, [], (e, _), loc) ->
      (* Since ASL code tends to have few mutable vars, we use all mutable vars as
       * an approximation of the set of variables modified by this if.
       *)
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (b, ty)) ->
             if Option.is_some b then (
               let v' = locals#fresh in
               Some (v, v', ty)
             ) else (
               None
             )))
      in
      let (mutable_vars, fresh_vars, mutable_types) = Utils.split3 mutables in
      let get_mutbind (v : Ident.t) : Ident.t =
        ( match ScopeStack.get env v with
        | Some (Some v', _) -> v'
        | _ -> raise (InternalError (loc, "Stmt_If", (fun fmt -> Ident.pp fmt v), __LOC__))
        )
      in
      let (c', _) = expr loc env fmt c in
      if not (List.is_empty fresh_vars) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) fresh_vars
      end;
      PP.fprintf fmt "scf.if %a " varident c';
      if not (List.is_empty mutable_types) then begin
        PP.fprintf fmt "-> (%a) "
          (commasep (pp_type loc)) mutable_types
      end;
      PP.fprintf fmt "{";
      indented_block env fmt t;
      if List.is_empty mutable_vars then begin
        PP.fprintf fmt "scf.yield";
      end else begin
        let mutable_vars' = List.map get_mutbind mutable_vars in
        PP.fprintf fmt "scf.yield %a : %a"
          (commasep varident) mutable_vars'
          (commasep (pp_type loc)) mutable_types
      end;
      PP.fprintf fmt "@,} else {";
      indented_block env fmt e;
      if List.is_empty mutable_vars then begin
        PP.fprintf fmt "scf.yield";
      end else begin
        let mutable_vars' = List.map get_mutbind mutable_vars in
        PP.fprintf fmt "scf.yield %a : %a"
          (commasep varident) mutable_vars'
          (commasep (pp_type loc)) mutable_types
      end;
      PP.fprintf fmt "@,}@,@,";
      List.iter (fun (v, v', ty) ->
          ignore (ScopeStack.add env v (Some v', ty))
        )
        mutables
  | Stmt_For (ix, ty, efrom, Direction_Up, eto, b, loc) ->
      (* Since ASL code tends to have few mutable vars, we use all mutable vars as
       * an approximation of the set of variables modified by this if.
       *)
      let get_mutbind (v : Ident.t) : Ident.t =
        ( match ScopeStack.get env v with
        | Some (Some v', _) -> v'
        | _ -> raise (InternalError (loc, "Stmt_For", (fun fmt -> Ident.pp fmt v), __LOC__))
        )
      in
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (ob, ty)) ->
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
      PP.fprintf fmt "%a = asl.constant_int 1 {attr_dict}@," varident step;
      if not (List.is_empty mutables) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) (List.map (fun (_, _, _, finalv, _) -> finalv) mutables)
      end;
      (* todo: deal with mismatch in index type *)
      PP.fprintf fmt "scf.for %a = %a to %a step %a"
        varident ix
        varident from'
        varident to'
        varident step;
      if not (List.is_empty mutables) then begin
        PP.fprintf fmt " iter_args (";
        commasep (fun fmt (v, current, loopv, _, _) -> PP.fprintf fmt "%a = %a"
            varident loopv
            varident current)
          fmt
          mutables;
        PP.fprintf fmt ") -> (%a)"
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt "{";
      ScopeStack.nest env (fun env' ->
        ignore (ScopeStack.add env' ix (None, Asl_utils.type_integer));
        indented_block env' fmt b);
      if List.is_empty mutables then begin
        PP.fprintf fmt "scf.yield";
      end else begin
        PP.fprintf fmt "scf.yield %a : %a"
          (commasep varident) (List.map (fun (v, _, _, _, _) -> get_mutbind v) mutables)
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt "@,}@,@,";
      List.iter (fun (v, _, _, finalv, ty) ->
          ignore (ScopeStack.add env v (Some finalv, ty))
        )
        mutables
  | Stmt_While (cond, b, loc) ->
      (* Since ASL code tends to have few mutable vars, we use all mutable vars as
       * an approximation of the set of variables modified by this if.
       *)
      let get_mutbind (env : environment) (v : Ident.t) : Ident.t =
        ( match ScopeStack.get env v with
        | Some (Some v', _) -> v'
        | _ -> raise (InternalError (loc, "Stmt_For", (fun fmt -> Ident.pp fmt v), __LOC__))
        )
      in
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (ob, ty)) ->
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
      if not (List.is_empty mutables) then begin
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
          List.iter (fun (v, oty) -> ScopeStack.add env v (None, Option.get oty)) fty.parameters;
          List.iter (fun (v, ty) -> ScopeStack.add env v (None, ty)) fty.args;
          PP.fprintf fmt "asl.func @%a%a(%a) -> %a {"
            ident f
            (formal_params loc) fty.parameters
            (commasep (formal_arg loc)) fty.args
            (pp_return_type loc) fty.rty;
          return_type := Option.value ~default:(AST.Type_Tuple []) fty.rty;
          indented_block env fmt b;
          if Option.is_none fty.rty then PP.fprintf fmt "asl.return@.";
          PP.fprintf fmt "}@.@."
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
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let decls = !Commands.declarations in

    (* record function types *)
    List.iter (fun d ->
      ( match d with
      | AST.Decl_BuiltinFunction (f, fty, _)
      | AST.Decl_FunType (f, fty, _)
      | AST.Decl_FunDefn (f, fty, _, _)
      -> funtypes := Identset.Bindings.add f fty !funtypes
      | _ -> ()
      )
    ) decls;

    Identset.IdentSet.iter (fun f -> 
      ( match Identset.Bindings.find_opt f !funtypes with
      | None -> ()
      | Some fty ->
          let loc = Loc.Unknown in
          PP.fprintf Format.std_formatter "asl.func @%a%a(%a) -> %a@,"
            ident f
            (formal_params loc) fty.parameters
            (commasep (formal_arg loc)) fty.args
            (pp_return_type loc) fty.rty;
      )
    ) standard_functions;
    PP.fprintf Format.std_formatter "@,";
    declarations Format.std_formatter (List.rev decls);
    true
  in

  let flags = Arg.align [
      ]
  in
  Commands.registerCommand "generate_mlir" flags [] [] "Generate MLIR" cmd

(****************************************************************
 * End
 ****************************************************************)
