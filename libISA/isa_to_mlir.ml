(****************************************************************
 * ISA to MLIR conversion
 *
 * Conversion based on use of bigint dialect and using dynamic
 * typing (insertion of explicit assertions) to convey dependent
 * type information (e.g., the size of a bitvector) to MLIR.
 *
 * This is a second take on how to generate MLIR from .isa
 * (see backend_mlir.ml for the first take).
 *
 * Copyright (C) 2024-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(* Turn off warnings about unused (debug) functions. *)
[@@@warning "-32"]
[@@@warning "-37-39-69"] (* temporary - until new IR is in place *)

(** ISA to MLIR backend *)

module AST = Isa_ast
module FMT = Isa_fmt
module PP = Format
module V = Value
module Builtins = Builtin_idents
module HLIR = Hlir
open Isa_utils
open Format_utils
open Utils

(****************************************************************
 * Utils (to move to utils module)
 ****************************************************************)

(* see Option.blend in latest version *)
let option_blend (f : 'a -> 'b -> 'c) (o1 : 'a option) (o2 : 'b option) : 'c option =
  ( match (o1, o2) with
  | (Some x1, Some x2) -> Some (f x1 x2)
  | (Some x1, _) -> o1
  | _ -> o2
  )

let lift (f : Ident.t -> Ident.t -> Ident.t) (x : Ident.t) (oy : Ident.t option) : Ident.t option =
  Option.map (f x) oy


(****************************************************************
 * Pretty printing helpers
 ****************************************************************)

let vbox (fmt : PP.formatter) (pp : unit -> 'a) : 'a=
  PP.pp_open_vbox fmt 0;
  let r = pp () in
  PP.pp_close_box fmt ();
  r

let hlist (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  Format.pp_print_list ~pp_sep:(fun fmt _ -> Format_utils.nbsp fmt) pp fmt xs

let indentation = 4

let indented (fmt : PP.formatter) (pp : unit -> 'a) =
  PP.pp_print_break fmt indentation indentation;
  vbox fmt pp

let ident_str (fmt : PP.formatter) (x : string) : unit =
  let x' = Str.global_replace (Str.regexp "::") "$" x in
  PP.pp_print_string fmt x'

let ident (fmt : PP.formatter) (x : Ident.t) : unit =
  ident_str fmt (Ident.name x)

let varident (fmt : PP.formatter) (x : Ident.t) : unit =
  if not (String.starts_with ~prefix:"%" (Ident.name x)) then begin
    PP.fprintf fmt "%%";
  end;
  ident_str fmt (Ident.name x)

let label (fmt : PP.formatter) (x : Ident.t) : unit =
  Ident.pp fmt x

(****************************************************************
 * Primop support
 ****************************************************************)

let standard_functions = Identset.IdentSet.of_list [
  Builtin_idents.eq_bool;
  Builtin_idents.ne_bool;
  Builtin_idents.not_bool;
  Builtin_idents.strict_and_bool;
  Builtin_idents.strict_or_bool;
  Builtin_idents.lazy_and_bool;
  Builtin_idents.lazy_or_bool;

  (*
  Builtin_idents.eq_int;
  Builtin_idents.ne_int;
  Builtin_idents.ge_int;
  Builtin_idents.gt_int;
  Builtin_idents.le_int;
  Builtin_idents.lt_int;
  Builtin_idents.neg_int;
  Builtin_idents.add_int;
  Builtin_idents.sub_int;
  Builtin_idents.mul_int;
  Builtin_idents.pow2_int;
  *)
  Builtin_idents.shl_int;
  Builtin_idents.shr_int;
  Builtin_idents.mod_pow2_int;
  Builtin_idents.is_pow2_int;
  Builtin_idents.align_int;
  Builtin_idents.cdiv_int;
  Builtin_idents.crem_int;
  Builtin_idents.pdiv_int;
  Builtin_idents.prem_int;
  Builtin_idents.zdiv_int;
  Builtin_idents.zrem_int;
  (*
  Builtin_idents.pow_int_int;
  Builtin_idents.max;
  Builtin_idents.min;
  Builtin_idents.abs;
  *)

  Builtin_idents.eq_bits;
  Builtin_idents.ne_bits;
  Builtin_idents.not_bits;
  Builtin_idents.and_bits;
  Builtin_idents.or_bits;
  Builtin_idents.xor_bits;
  (*
  Builtin_idents.add_bits;
  Builtin_idents.sub_bits;
  *)
  Builtin_idents.mul_bits;
  (*
  Builtin_idents.add_bits_int;
  Builtin_idents.sub_bits_int;
  Builtin_idents.mul_bits_int;
  *)
  Builtin_idents.asr_bits;
  (*
  Builtin_idents.lsl_bits;
  *)
  Builtin_idents.lsr_bits;
  Builtin_idents.append_bits;
  Builtin_idents.replicate_bits;
  Builtin_idents.zeros_bits;
  Builtin_idents.ones_bits;
  Builtin_idents.sign_extend_bits;
  Builtin_idents.zero_extend_bits;
  Builtin_idents.pow2_bits;
  (*
  Builtin_idents.cvt_bits_sint;
  Builtin_idents.cvt_bits_uint;
  *)
  Builtin_idents.cvt_int_bits;
  (*
  Builtin_idents.is_ones;
  Builtin_idents.is_zero;
  Builtin_idents.length;
  *)
  (*
  Builtin_idents.mask_int;
  Builtin_idents.in_mask;
  Builtin_idents.mk_mask;
  Builtin_idents.notin_mask;
  *)

  Builtin_idents.ram_init;
  Builtin_idents.ram_read;
  Builtin_idents.ram_write;

  Builtin_idents.asl_end_execution;

  Builtin_idents.print_int_hex;
  Builtin_idents.print_int_dec;
  Builtin_idents.print_char;
  Builtin_idents.print_str;
  Builtin_idents.print_bits_hex
]

(****************************************************************
 * Global environment
 ****************************************************************)

let enum_size = 8 (* assume this is big enough for all enumerated types *)
let enums : int Identset.Bindings.t ref = ref Identset.Bindings.empty
let type_of_enum : AST.ty Identset.Bindings.t ref = ref Identset.Bindings.empty
let enum_types : Identset.IdentSet.t ref = ref Identset.IdentSet.empty

let global_vartypes : AST.ty Identset.Bindings.t ref = ref Identset.Bindings.empty
let funtypes : AST.function_type Identset.Bindings.t ref = ref Identset.Bindings.empty
let fieldtypes : ((Ident.t * AST.ty) list) Identset.Bindings.t ref = ref Identset.Bindings.empty

(****************************************************************
 * Types
 ****************************************************************)

let rec pp_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
  ( match x with
  | Type_Bits (e, _) -> PP.fprintf fmt "!Std$Bits"
  | Type_Constructor (tc, []) when tc = Builtin_idents.boolean_ident ->
      PP.fprintf fmt "i1"
  | Type_Constructor (tc, []) when tc = Builtin_idents.string_ident ->
      PP.fprintf fmt "!Std$String"
  | Type_Constructor (tc, []) when tc = Builtin_idents.ram ->
      PP.fprintf fmt "!Std$RAM"
  | Type_Constructor (tc, []) when Ident.name(tc) = "Bit" -> (* todo: why is this alias not expanded? *)
      PP.fprintf fmt "!Std$Bits"
  | Type_Constructor (tc, []) when Identset.IdentSet.mem tc !enum_types ->
      PP.fprintf fmt "i%d" enum_size
  | Type_Constructor (tc, ps) ->
      PP.fprintf fmt "!%a" ident tc
  | Type_Integer ocrs ->
      PP.fprintf fmt "!Std$Integer"
  | Type_Array (Index_Int ixty, elty) ->
      PP.fprintf fmt "array<%a>" (pp_type loc) elty
  | Type_Tuple tys ->
      PP.fprintf fmt "tuple<%a>" (commasep (pp_type loc)) tys
  | _ ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "type", pp))
  )

let pp_return_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
  ( match x with
  | Type_Tuple [] ->
      PP.fprintf fmt "()"
  | Type_Tuple tys ->
      PP.fprintf fmt "(%a)" (commasep (pp_type loc)) tys
  | _ ->
      pp_type loc fmt x
  )

(****************************************************************
 * Local environment
 ****************************************************************)

(* the environment tracks the following about local variables
 * - for mutable variables, what SSA variable holds its current value
 * - is it a mutable variable (needed for uninitialized variables)
 * - their type
 *)

type env_entry = (Ident.t option * bool * AST.ty)

let pp_env_entry (fmt : Format.formatter) (e : env_entry) : unit =
  let (ov, is_constant, t) = e in
  Format.fprintf fmt "%s %a : %a"
    (if is_constant then "const" else "var")
    (PP.pp_print_option varident) ov
    FMT.ty t

type environment = env_entry ScopeStack.t

let pp_environment (fmt : Format.formatter) (env : environment) : unit =
  PP.fprintf fmt "{ %a }" (ScopeStack.pp pp_env_entry) env

let locals = new Isa_utils.nameSupply "%"
let labels = new Isa_utils.nameSupply "^bb"

let return_types = ref []
let return_label : Ident.t ref = ref labels#fresh
let return_vars : (Ident.t * AST.ty) list ref = ref []

let rebind (loc : Loc.t) (env : environment) (v : Ident.t) (v' : Ident.t) : unit =
  ( match ScopeStack.get env v with
  | Some (_, is_constant, ty) -> ignore (ScopeStack.set env v (Some v', is_constant, ty))
  | None -> raise (InternalError (loc, "rebind", (fun fmt -> Ident.pp fmt v), __LOC__))
  )

let get_mutbind (loc : Loc.t) (env : environment) (v : Ident.t) : Ident.t =
  ( match ScopeStack.get env v with
  | Some (Some v', _, _) -> v'
  | _ -> raise (InternalError (loc, "get_mutbind", (fun fmt -> Ident.pp fmt v), __LOC__))
  )

(* Since ISA code tends to have few mutable vars, we use all mutable vars as
 * an approximation of the set of variables modified by this if.
 *)
type renaming = (Ident.t * Ident.t * AST.ty) list

let mk_renaming (env : environment) : renaming =
  ScopeStack.bindings env
  |> List.concat_map (List.filter_map (fun (v, (b, is_constant, ty)) ->
       if is_constant then (
         None
       ) else (
         let v' = locals#fresh in
         Some (v, v', ty)
       )))

type var = (Ident.t * AST.ty)

let varty (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty)) : unit =
  let (v, t) = x in
  PP.fprintf fmt "%a : %a"
    varident v
    (pp_type loc) t

let get_mutables (env : environment) : renaming =
  ScopeStack.bindings env
  |> List.concat_map (List.filter_map (fun (v, (b, is_constant, ty)) -> if not is_constant && Option.is_some b then Some (v, Option.get b, ty) else None))

let update_environment (env : environment) (rename : renaming) : unit =
  List.iter (fun (v, v', ty) ->
      ignore (ScopeStack.set env v (Some v', false, ty))
    )
    rename

let fresh_env (env : environment) (mutables : renaming) : environment =
  let env' = ScopeStack.clone env in
  update_environment env' mutables;
  env'

let branch_label (loc : Loc.t) (fmt : PP.formatter) (target : Ident.t) (args : var list) : unit =
  if not (Utils.is_empty args) then begin
    PP.fprintf fmt "%a(%a):@,"
      label target
      (commasep (varty loc)) args
  end else begin
    PP.fprintf fmt "%a:@," label target;
  end

(* Note that there is a weird inconsistency between the syntax for arguments in cf.br/cf.cond_br
 * and in basic blocks because basic blocks intersperse variable name and type while branches
 * list all the variables and then all of the types.
 *)
let print_br_label (loc : Loc.t) (fmt : PP.formatter) (target : Ident.t) (args : var list) : unit =
  if not (Utils.is_empty args) then begin
    PP.fprintf fmt "%a(%a : %a)"
      label target
      (commasep (fun fmt (v, t) -> varident fmt v)) args
      (commasep (fun fmt (v, t) -> pp_type loc fmt t)) args
  end else begin
    PP.fprintf fmt "%a" label target;
  end

let cf_br (loc : Loc.t) (fmt : PP.formatter) (target : Ident.t) (src_vars : var list) : unit =
  PP.fprintf fmt "cf.br ";
  print_br_label loc fmt target src_vars;
  PP.fprintf fmt "@,"

(* Make an unconditional branch
 * This is useful for backward branches
 *)
let make_backward_branch (loc : Loc.t) (fmt : PP.formatter) (env : environment) (mutables : renaming) (label : Ident.t) : unit =
  let src_vars = List.map (fun (v, _, t) -> (get_mutbind loc env v, t)) mutables in
  cf_br loc fmt label src_vars

(* Make an unconditional branch and return the bindings needed to update the environment
 * This is useful for forward branches
 *)
let make_forward_branch (loc : Loc.t) (fmt : PP.formatter) (env : environment) (mutables : renaming) (label : Ident.t) : renaming =
  let src_vars = List.map (fun (v, _, t) -> (get_mutbind loc env v, t)) mutables in
  cf_br loc fmt label src_vars;
  List.map (fun (v, curr, ty) -> (v, locals#fresh, ty)) mutables

let cf_cond_br (loc : Loc.t) (fmt : PP.formatter)
    (cond : Ident.t)
    (target1 : Ident.t) (args1 : var list)
    (target2 : Ident.t) (args2 : var list)
    : unit =
  PP.fprintf fmt "cf.cond_br %a, " varident cond;
  print_br_label loc fmt target1 args1;
  PP.fprintf fmt ", ";
  print_br_label loc fmt target2 args2;
  PP.fprintf fmt "@,"

let pp_yield_vars (fmt : PP.formatter) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    PP.fprintf fmt "%a = "
      (commasep varident) fresh_vars
  end

let pp_yield_type (loc : Loc.t) (fmt : PP.formatter) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    PP.fprintf fmt "-> (%a) "
      (commasep (pp_type loc)) types
  end

let pp_yield (loc : Loc.t) (env : environment) (fmt : PP.formatter) (keyword : string) (rename : renaming) : unit =
  if not (Utils.is_empty rename) then begin
    let (vars, fresh_vars, types) = Utils.split3 rename in
    let vars' = List.map (get_mutbind loc env) vars in
    PP.fprintf fmt "%s %a : %a@,"
      keyword
      (commasep varident) vars'
      (commasep (pp_type loc)) types
  end

(****************************************************************
 * Record support
 *
 * This code centralizes the magic names made up for
 * manipulating records
 ****************************************************************)

let record_constructor (fmt : PP.formatter) (r : Ident.t) : unit =
  PP.fprintf fmt "Internal$Make$%a" ident r

let record_field_get (fmt : PP.formatter) (r : Ident.t) (f : Ident.t) : unit =
  PP.fprintf fmt "Internal$GetField$%a$%a" ident r ident f

let record_field_set (fmt : PP.formatter) (r : Ident.t) (f : Ident.t) : unit =
  PP.fprintf fmt "Internal$SetField$%a$%a" ident r ident f


(****************************************************************
 * Functions
 ****************************************************************)

let formal_args (fty : AST.function_type) : (Ident.t * AST.ty) list =
  let tvs = List.map (fun (v, t) -> v) fty.parameters in
  let vtys = List.map (fun (v, t) -> (v, Option.get t)) fty.parameters
           @ (List.filter (fun (v, _) -> not (List.mem v tvs)) (List.map (fun (v, t, _) -> (v, t)) fty.args))
           @ Option.to_list (Option.map (fun (v, t) -> (v, t)) fty.setter_arg)
  in
  vtys

let actual_args (fty : AST.function_type) (tes : AST.expr list) (es : AST.expr list) : AST.expr list =
  let tvs = List.map (fun (v, t) -> v) fty.parameters in
  let es' = Utils.filter_map2 (fun (v, t, _) e-> if List.mem v tvs then None else Some e) fty.args es in
  (tes @ es')

let formal_args_decls (loc : Loc.t) (fmt : PP.formatter) (fty : AST.function_type) : unit =
  let vtys = formal_args fty in
  commasep (varty loc) fmt vtys

let formal_arg_types (loc : Loc.t) (fmt : PP.formatter) (fty : AST.function_type) : unit =
  let vtys = formal_args fty in
  commasep (fun fmt (v, t) -> pp_type loc fmt t) fmt vtys

let mk_formal_env (fty : AST.function_type) (actuals : Ident.t list) : environment =
  let formal_env = ScopeStack.empty () in
  List.iter2
    (fun (formal, t) actual -> ScopeStack.add formal_env formal (Some actual, false, t))
    (formal_args fty) actuals;
  formal_env


(****************************************************************
 * Expressions
 ****************************************************************)

let with_fresh_typed (t : AST.ty) (f : Ident.t -> unit) : (Ident.t * AST.ty) =
  let v = locals#fresh in
  f v;
  (v, t)

let with_fresh (f : Ident.t -> unit) : Ident.t =
  let v = locals#fresh in
  f v;
  v

let to_index (fmt : PP.formatter) (x : Ident.t) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = index.casts %a : !Std$Integer to index@,"
      varident t
      varident x
  )

(****************************************************************
 * Constants
 ****************************************************************)

let arith_constant (fmt : PP.formatter) (x : Z.t) (width : int) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = arith.constant %s : i%d@," varident t (Z.to_string x) width
  )

let bool_constant (fmt : PP.formatter) (x : bool) : Ident.t =
  arith_constant fmt (if x then Z.one else Z.zero) 1

let bigint_constant (fmt : PP.formatter) (x : Z.t) : Ident.t =
  with_fresh (fun t ->
    (* PP.fprintf fmt "%a = bigint.constant %s : !Std$Integer@," varident t (Z.to_string x) *)
    PP.fprintf fmt "%a = arith.constant %s : !Std$Integer@," varident t (Z.to_string x)
  )

let bitvector_constant (fmt : PP.formatter) (x : Primops.bitvector) : Ident.t =
  let sz = with_fresh (fun sz ->
    PP.fprintf fmt "%a = arith.constant %d : !Std$Integer@," varident sz x.n
  ) in
  let c = with_fresh (fun c ->
    PP.fprintf fmt "%a = arith.constant %s : i128@," varident c (Z.to_string x.v)
  ) in
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Bits$Constant(%a, %a) : (!Std$Integer, i128) -> !Std$Bits@,"
      varident r
      varident sz
      varident c
  )

let string_constant (fmt : PP.formatter) (x : string) : Ident.t =
  (*
  with_fresh (fun t ->
    PP.fprintf fmt "%a = string.constant \"%s\" : !Std$String@," varident t (String.escaped x)
  )
  *)
  bool_constant fmt false (* todo: do strings properly *)

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : (Ident.t * AST.ty) =
  ( match x with
  | VBool v   -> (bool_constant fmt v, type_bool)
  | VInt v    -> (bigint_constant fmt v, type_integer)
  | VBits v   -> (bitvector_constant fmt v, type_bits (mk_litint v.n))
  | VString v -> (string_constant fmt v, type_string)
  | _ -> raise (InternalError (loc, "valueLit", (fun fmt -> Value.pp_value fmt x), __LOC__))
  )

(****************************************************************
 * Useful operations
 ****************************************************************)

let type_checks = ref false

let cf_assume (fmt : PP.formatter) (x : Ident.t) : unit =
  if !type_checks then begin
    (* todo: should be cf.assume *)
    PP.fprintf fmt "cf.assert %a, \"type assumption\"@,"
      varident x
  end

let cf_assert (fmt : PP.formatter) (x : Ident.t) : unit =
  if !type_checks then begin
    PP.fprintf fmt "cf.assert %a, \"type assertion\"@,"
      varident x
  end

let int_add (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Integer$Add(%a, %a) : (!Std$Integer, !Std$Integer) -> !Std$Integer@,"
      varident r
      varident x
      varident y
  )

let int_sub (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Integer$Subtract(%a, %a) : (!Std$Integer, !Std$Integer) -> !Std$Integer@,"
      varident r
      varident x
      varident y
  )

let int_eq (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Integer$Eq(%a, %a) : (!Std$Integer, !Std$Integer) -> i1@,"
      varident r
      varident x
      varident y
  )

let int_le (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Integer$Le(%a, %a) : (!Std$Integer, !Std$Integer) -> i1@,"
      varident r
      varident x
      varident y
  )

let int_lt (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Integer$Lt(%a, %a) : (!Std$Integer, !Std$Integer) -> i1@,"
      varident r
      varident x
      varident y
  )

let bv_eq (fmt : PP.formatter) (sz : Ident.t) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Bits$Eq(%a, %a, %a) : (!Std$Integer, !Std$Bits, !Std$Bits) -> i1@,"
      varident r
      varident sz
      varident x
      varident y
  )

let bv_and (fmt : PP.formatter) (sz : Ident.t) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Bits$And(%a, %a, %a) : (!Std$Integer, !Std$Bits, !Std$Bits) -> !Std$Bits@,"
      varident r
      varident sz
      varident x
      varident y
  )

let bv_length (fmt : PP.formatter) (x : Ident.t) : Ident.t =
  with_fresh (fun r ->
    PP.fprintf fmt "%a = func.call @Std$Bits$MyLength(%a) : (!Std$Bits) -> !Std$Integer@,"
      varident r
      varident x
  )

let bool_or (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = arith.ori %a, %a : i1@,"
      varident t
      varident x
      varident y
  )

let bool_and (fmt : PP.formatter) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = arith.andi %a, %a : i1@,"
      varident t
      varident x
      varident y
  )

let rec or_reduce (fmt : PP.formatter) (cs : Ident.t option list) : Ident.t option =
  ( match cs with
  | [] -> None
  | [c] -> c
  | c::cs' -> option_blend (bool_or fmt) c (or_reduce fmt cs')
  )

let bv_append (fmt : PP.formatter) (wx : Ident.t) (wy : Ident.t) (x : Ident.t) (y : Ident.t) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = func.call @Std$Bits$Append(%a, %a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Bits, !Std$Bits) -> !Std$Bits@,"
      varident t
      varident wx
      varident wy
      varident x
      varident y
  )

let bv_slice (fmt : PP.formatter) (x : Ident.t) (i : Ident.t) (w : Ident.t) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = func.call @Std$Bits$Slice(%a, %a, %a) : (!Std$Bits, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
      varident t
      varident x
      varident i
      varident w
  )

let memref_global_scalar (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (ty : AST.ty) : unit =
  PP.fprintf fmt "memref.global @%a : memref<%a>@,@,"
    ident v
    (pp_type loc) ty

let memref_get_global_scalar (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (ty : AST.ty) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = memref.get_global @@%a : memref<%a>@,"
      varident t
      ident v
      (pp_type loc) ty
  )

let memref_load_scalar (loc : Loc.t) (fmt : PP.formatter) (ref : Ident.t) (ty : AST.ty) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = memref.load %a[] : memref<%a>@,"
      varident t
      varident ref
      (pp_type loc) ty
  )

let memref_store_scalar (loc : Loc.t) (fmt : PP.formatter) (ref : Ident.t) (x : Ident.t) (ty : AST.ty) : unit =
  PP.fprintf fmt "memref.store %a, %a[] : memref<%a>@,"
    varident x
    varident ref
    (pp_type loc) ty

let memref_global_array (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (sz : Z.t) (ty : AST.ty) : unit =
  PP.fprintf fmt "memref.global @%a : memref<%s x %a>@,@,"
    ident v
    (Z.to_string sz)
    (pp_type loc) ty

let memref_get_global_array (loc : Loc.t) (fmt : PP.formatter) (v : Ident.t) (sz : Z.t) (ty : AST.ty) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = memref.get_global @@%a : memref<%s x %a>@,"
      varident t
      ident v
      (Z.to_string sz)
      (pp_type loc) ty
  )

let memref_load_array (loc : Loc.t) (fmt : PP.formatter) (aref : Ident.t) (ix : Ident.t) (sz : Z.t) (ty : AST.ty) : Ident.t =
  with_fresh (fun t ->
    PP.fprintf fmt "%a = memref.load %a[%a] : memref<%s x %a>@,"
      varident t
      varident aref
      varident ix
      (Z.to_string sz)
      (pp_type loc) ty
  )

let memref_store_array (loc : Loc.t) (fmt : PP.formatter) (aref : Ident.t) (ix : Ident.t) (x : Ident.t) (sz : Z.t) (ty : AST.ty) : unit =
  PP.fprintf fmt "memref.store %a, %a[%a] : memref<%s x %a>@,"
    varident x
    varident aref
    varident ix
    (Z.to_string sz)
    (pp_type loc) ty

let rec concat (fmt : PP.formatter) (xs : (Ident.t * Ident.t * AST.expr) list) : (Ident.t * Ident.t * AST.expr) =
  ( match xs with
  | [] ->
     let zero' = bigint_constant fmt Z.zero in
     (bitvector_constant fmt Primops.empty_bits, zero', zero)
  | [(x, xw', xw)] -> (x, xw', xw)
  | ((y, yw', yw) :: ys) ->
      let (ys', ysw', ysw) = concat fmt ys in
      let w = mk_add_int yw ysw in
      let w' = int_add fmt yw' ysw' in
      let t = bv_append fmt yw' ysw' y ys' in
      (t, w', w)
  )

(****************************************************************
 * Patterns
 ****************************************************************)

let rec pattern (loc : Loc.t) (fmt : PP.formatter) (p : AST.pattern) (discriminant : Ident.t) : Ident.t =
  ( match p with
  | Pat_Lit (VBits v) ->
      let v' = bitvector_constant fmt v in
      let sz = bigint_constant fmt (Z.of_int v.n) in
      bv_eq fmt sz v' discriminant
  | Pat_Lit (VBool v) ->
      let v' = bool_constant fmt v in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = func.call @Std$Bool$Eq(%a, %a) : (i1, i1) -> i1@,"
          varident t
          varident v'
          varident discriminant
      )
  | Pat_Lit (VMask mask) ->
      let (v, m) = Primops.prim_mask_to_bits mask in
      let v' = bitvector_constant fmt v in
      let m' = bitvector_constant fmt m in
      let sz = bigint_constant fmt (Z.of_int v.n) in
      let masked' = bv_and fmt sz discriminant m' in
      bv_eq fmt sz masked' v'
  | Pat_Set ps ->
      patterns loc fmt ps discriminant
  | Pat_Lit (VInt v) ->
      let v' = bigint_constant fmt v in
      int_eq fmt v' discriminant
  | Pat_Range (Expr_Lit (VInt lo), Expr_Lit (VInt hi)) ->
      let lo' = bigint_constant fmt lo in
      let hi' = bigint_constant fmt hi in
      let c1 = int_le fmt lo' discriminant in
      let c2 = int_le fmt discriminant hi' in
      bool_or fmt c1 c2
  | _ -> raise (InternalError (loc, "pattern", (fun fmt -> FMT.pattern fmt p), __LOC__))
  )

and patterns (loc : Loc.t) (fmt : PP.formatter) (ps : AST.pattern list) (discriminant : Ident.t) : Ident.t =
  ( match ps with
  | [] -> bool_constant fmt false
  | [p] -> pattern loc fmt p discriminant
  | (q :: qs) ->
      let q' = pattern loc fmt q discriminant in
      let qs' = patterns loc fmt qs discriminant in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = arith.ori %a, %a : i1@,"
          varident t
          varident q'
          varident qs'
      )
  )

(****************************************************************
 * Expressions
 ****************************************************************)

let rec expr (loc : Loc.t) (env : environment) (fmt : PP.formatter) (x : AST.expr) : (Ident.t * AST.ty) =
  ( match x with
  | Expr_Lit v -> valueLit loc fmt v

  | Expr_Var v ->
      if Ident.equal v Builtins.true_ident then (bool_constant fmt true, type_bool)
      else if Ident.equal v Builtins.false_ident then (bool_constant fmt false, type_bool)
      else (
        (* todo: enumeration variables *)
        ( match ScopeStack.get env v with
        | None -> (* global variable *)
            assert (Identset.Bindings.mem v !global_vartypes);
            let ty = Identset.Bindings.find v !global_vartypes in
            let ref = memref_get_global_scalar loc fmt v ty in
            (memref_load_scalar loc fmt ref ty, ty)
        | Some (Some v', _, ty) -> (v', ty)
        | Some (None, _, ty) -> (v, ty)
        )
      )

  | Expr_Array(Expr_Var v, ix) ->
      assert (Identset.Bindings.mem v !global_vartypes);
      let ty = Identset.Bindings.find v !global_vartypes in
      let (sz, elty) = ( match ty with
                       | Type_Array (Index_Int (Expr_Lit (VInt sz)), elty) -> (sz, elty)
                       | _ -> let pp fmt = FMT.ty fmt ty in
                              raise (Error.Unimplemented (loc, "type", pp))
                       )
      in
      let (ix', _) = expr loc env fmt ix in
      let ix'' = to_index fmt ix' in
      let aref = memref_get_global_array loc fmt v sz elty in
      (memref_load_array loc fmt aref ix'' sz elty, elty)

  | Expr_Slices (Type_Integer _, e, [Slice_Single i]) ->
      let (e', _) = expr loc env fmt e in
      let (i', _) = expr loc env fmt i in
      let wd' = bigint_constant fmt Z.one in
      with_fresh_typed (type_bits one) (fun t ->
        PP.fprintf fmt "%a = func.call @Std$Integer$Slice(%a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
          varident t
          varident e'
          varident i'
          varident wd'
      )

  | Expr_Slices (Type_Integer _, e, [Slice_LoWd (lo, wd)]) ->
      let (e',  _) = expr loc env fmt e in
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      with_fresh_typed (type_bits wd) (fun t ->
        PP.fprintf fmt "%a = func.call @Std$Integer$Slice(%a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
          varident t
          varident e'
          varident lo'
          varident wd'
      )

  | Expr_Slices (Type_Bits _, e, ss) ->
      let (e', _) = expr loc env fmt e in
      slices loc env fmt e' ss

  | Expr_TApply (f, tes, es, NoThrow) ->
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes es in
      let actuals' = List.map (Fun.compose fst (expr loc env fmt)) actuals in
      let formal_env = mk_formal_env fty actuals' in
      check_actuals loc fmt formal_env fty actuals';
      let r = with_fresh (fun t ->
        PP.fprintf fmt "%a = func.call @%a(%a) : (%a) -> %a@,"
          varident t
          ident f
          (commasep varident) actuals'
          (formal_arg_types loc) fty
          (pp_return_type loc) fty.rty
        )
      in
      if !type_checks then begin
        let ensures = check_type loc formal_env fmt r fty.rty in
        Option.iter (cf_assume fmt) ensures
      end;
      (r, fty.rty)

  | Expr_If ([], e) ->
      expr loc env fmt e
  | Expr_If ((c, t) :: cts, e) ->
      let l_true  = labels#fresh in
      let l_false = labels#fresh in
      let l_end   = labels#fresh in

      let (c', _) = expr loc env fmt c in

      cf_cond_br loc fmt c' l_true [] l_false [];

      branch_label loc fmt l_true [];
      let t' = expr loc env fmt t in
      cf_br loc fmt l_end [t'];

      branch_label loc fmt l_false [];
      let e' = expr loc env fmt (Expr_If (cts, e)) in
      cf_br loc fmt l_end [e'];

      let ty = snd t' in
      let r = (locals#fresh, ty) in
      branch_label loc fmt l_end [r];
      r

  | Expr_Assert (c, e, loc) ->
      let (c', _) = expr loc env fmt c in
      PP.fprintf fmt "cf.assert %a, \"%a\""
        varident c'
        FMT.expr x;
      expr loc env fmt e

  | Expr_In (e, p) ->
      let (e', _) = expr loc env fmt e in
      (pattern loc fmt p e', type_bool)

  | Expr_Let (v, t, e1, e2) ->
      let (e1', _) = expr loc env fmt e1 in
      ScopeStack.nest env (fun env' ->
        ScopeStack.add env v (Some e1', true, t);
        expr loc env fmt e2
      )

  | Expr_Field (e, f) ->
      let (e', record_ty) = expr loc env fmt e in
      let rtc = ( match record_ty with
                     | Type_Constructor (rtc, []) -> rtc
                     | _ ->
                         raise (InternalError (Loc.Unknown, "isa_to_mlir.expr_field", (fun fmt -> FMT.ty fmt record_ty), __LOC__))
                     )
      in
      let field_tys = Identset.Bindings.find rtc !fieldtypes in
      let field_ty = List.assq f field_tys in
      with_fresh_typed field_ty (fun t ->
        PP.fprintf fmt "%a = func.call @%a(%a) : (%a) -> %a@,"
          varident t
          (Fun.flip record_field_get rtc) f
          varident e'
          (pp_type loc) record_ty
          (pp_type loc) field_ty
      )

  | Expr_WithChanges (record_ty, e, cs) ->
      let (e', _) = expr loc env fmt e in
      let acc = ref e' in
      List.iter (fun (c, ce) ->
        let (ce', t) = expr loc env fmt ce in
        acc := apply_change loc env fmt record_ty t c ce' !acc
        )
        cs;
      (!acc, record_ty)

  | Expr_Record (rtc, [], fas) ->
      (* Note: the typechecker has checked that all fields are present and in the same
       * order as the record declaration
       *)
      let fas' = List.map (fun (f, e) -> fst (expr loc env fmt e)) fas in
      let fts = Identset.Bindings.find rtc !fieldtypes in
      let ftys = List.map (fun (f, t) -> t) fts in
      let rty = AST.Type_Constructor (rtc, []) in
      with_fresh_typed rty (fun t ->
        PP.fprintf fmt "%a = func.call @%a(%a) : (%a) -> %a@,"
          varident t
          record_constructor rtc
          (commasep varident) fas'
          (commasep (pp_type loc)) ftys
          (pp_type loc) rty
      )

  | Expr_Tuple _

  | Expr_ArrayInit _
  | Expr_Array _

  | Expr_Unknown _

  | Expr_Slice _

  | Expr_Concat _ (* not part of .isa language *)
  | Expr_Fields _ (* not part of .isa language *)
  | Expr_AsType _ (* not part of .isa language *)

  | Expr_UApply _ (* not used after typechecking *)
  | Expr_Binop _ (* not used after typechecking *)
  | Expr_Unop _ (* not used after typechecking *)
  | _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

and slices (loc : Loc.t) (env : environment) (fmt : PP.formatter) (b : Ident.t) (xs : AST.slice list) : (Ident.t * AST.ty) =
  let xs' = List.map (slice loc env fmt b) xs in
  let (r, wd', wd) = concat fmt xs' in
  (r, type_bits wd)

and slice (loc : Loc.t) (env : environment) (fmt : PP.formatter) (b : Ident.t) (x : AST.slice) : (Ident.t * Ident.t * AST.expr) =
  ( match x with
  | Slice_Single i ->
      let wd = one in
      let (i',  _) = expr loc env fmt i in
      let (wd', _) = expr loc env fmt wd in
      let t = bv_slice fmt b i' wd' in
      (t, wd', wd)

  | Slice_LoWd (lo, wd) ->
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      let t = bv_slice fmt b lo' wd' in
      (t, wd', wd)

  | Slice_HiLo (hi, lo) ->
      let wd = mk_add_int (mk_sub_int hi lo) one in
      let (lo', _) = expr loc env fmt lo in
      let (wd', _) = expr loc env fmt wd in
      let t = bv_slice fmt b lo' wd' in
      (t, wd', wd)

  (*
  | Slice_HiWd (hi, wd) ->
      let hi' = eval_expr loc env hi in
      let wd' = eval_expr loc env wd in
      let lo' = eval_add_int loc (eval_sub_int loc hi' wd') (VInt Z.one) in
      (lo', wd')
  | Slice_Element (lo, wd) ->
      let wd' = eval_expr loc env wd in
      let lo' = eval_mul_int loc (eval_expr loc env lo) wd' in
      (lo', wd')
  *)
  | _ -> raise (InternalError (loc, "slice", (fun fmt -> FMT.slice fmt x), __LOC__))
  )

(****************************************************************
 * Dynamic type checks
 *
 * Generate runtime checks that a value satisfies the constraints
 * implied by its dependent type.
 *
 * For example, if 'x : Bits(e)', check that 'Length(x) == e'.
 ****************************************************************)

and check_set_range (loc : Loc.t) (env : environment) (fmt : PP.formatter) (v : Ident.t) (x : AST.set_range) : Ident.t option =
  ( match x with
  | Set_Single e -> Some (int_eq fmt v (fst (expr loc env fmt e)))
  | Set_Range (lo, hi) ->
      let c_lo = lift (Fun.flip (int_le fmt)) v (Option.map (Fun.compose fst (expr loc env fmt)) lo) in
      let c_hi = lift (int_le fmt) v (Option.map (Fun.compose fst (expr loc env fmt)) hi) in
      option_blend (bool_and fmt) c_lo c_hi
  )

and check_type (loc : Loc.t) (env : environment) (fmt : PP.formatter) (v : Ident.t) (x : AST.ty) : Ident.t option =
  ( match x with
  | Type_Bits (e, _) ->
      let t = bv_length fmt v in
      let (e', _) = expr loc env fmt e in
      Some (int_eq fmt t e')
  | Type_Integer None -> None
  | Type_Integer (Some srs) ->
      let cs = List.map (check_set_range loc env fmt v) srs in
      or_reduce fmt cs
  | Type_Constructor (tc, ps) -> None
  | Type_Array _ -> None
  | Type_Tuple [] -> None
  | _ ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "check_type", pp))
  )

and check_actuals (loc : Loc.t) (fmt : Format.formatter) (env : environment) (fty : AST.function_type) (actuals : Ident.t list) : unit =
  if !type_checks then begin
    List.iter2 (fun (formal, t) actual ->
      let requires = check_type loc env fmt actual t in
      Option.iter (cf_assert fmt) requires
      )
      (formal_args fty)
      actuals
  end

and check_types (loc : Loc.t) (env : environment) (fmt : PP.formatter) (v : Ident.t) (xs : AST.ty list) : Ident.t option =
  ( match xs with
  | [t] -> check_type loc env fmt v t
  | _ -> check_type loc env fmt v (Type_Tuple xs)
  )

and apply_change (loc : Loc.t) (env : environment) (fmt : PP.formatter) (rty : AST.ty) (vty : AST.ty) (c : AST.change) (v : Ident.t) (r : Ident.t) : Ident.t =
  ( match c with
  | Change_Field f ->
      let rtc = ( match rty with
                | Type_Constructor (tc, []) -> tc
                | _ ->
                    let pp fmt = FMT.ty fmt rty in
                    raise (Error.Unimplemented (loc, "apply_change", pp))
                )
      in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = func.call @%a(%a, %a) : (%a, %a) -> %a@,"
          varident t
          (Fun.flip record_field_set rtc) f
          varident r
          varident v
          (pp_type loc) rty
          (pp_type loc) vty
          (pp_type loc) rty
      )
  | Change_Slices ss ->
      set_slices loc env fmt rty ss v r
  )

and set_slices (loc : Loc.t) (env : environment) (fmt : PP.formatter) (rty : AST.ty) (ss : AST.slice list) (v : Ident.t) (r : Ident.t) : Ident.t =
  let acc = ref r in
  List.iter (fun s ->
    acc := set_slice loc env fmt rty s v r;
    )
    ss;
  !acc

and set_slice (loc : Loc.t) (env : environment) (fmt : PP.formatter) (rty : AST.ty) (s : AST.slice) (v : Ident.t) (r : Ident.t) : Ident.t =
  ( match s with
  | _ ->
      let pp fmt = FMT.slice fmt s in
      raise (Error.Unimplemented (loc, "set_slice", pp))
  )

(****************************************************************
 * Statements
 ****************************************************************)

(* returns 'is_terminator' flag indicating whether all subsequent statements are dead *)
let rec stmt (env : environment) (fmt : PP.formatter) (x : AST.stmt) : bool =
  ( match x with
  | Stmt_Assert (e, loc) ->
      let (e', _) = expr loc env fmt e in
      PP.fprintf fmt "cf.assert %a, \"%a\""
        varident e'
        FMT.expr e;
      false

  | Stmt_Return (Expr_Tuple es, loc) ->
      let es' = List.map (expr loc env fmt) es in
      cf_br loc fmt !return_label es';
      true

  | Stmt_Return (e, loc) ->
      let e' = expr loc env fmt e in
      if !type_checks then begin
        let ensures = check_types loc env fmt (fst e') !return_types in
        Option.iter (cf_assert fmt) ensures
      end;
      cf_br loc fmt !return_label [e'];
      true

  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v -> ScopeStack.add env v (None, false, t)) vs;
      false

  | Stmt_VarDecl (is_constant, DeclItem_Var (v, Some ty), i, loc) ->
      let (i', _) = expr loc env fmt i in
      ScopeStack.add env v (Some i', is_constant, ty);
      false

  | Stmt_Assign (LExpr_Var v, rhs, loc) ->
      let (rhs', _) = expr loc env fmt rhs in
      if Identset.Bindings.mem v !global_vartypes then begin (* global *)
        let ty = Identset.Bindings.find v !global_vartypes in
        let ref = memref_get_global_scalar loc fmt v ty in
        memref_store_scalar loc fmt ref rhs' ty
      end else begin
        rebind loc env v rhs'
      end;
      false

  (*
  | Stmt_Assign (LExpr_Field (Type_Constructor(r, []), l, f), rhs, loc) ->
      let rhs' = expr loc env fmt rhs in
      let e = Option.get (lexpr_to_expr l) in
      let e' = expr loc env fmt e in
      let fts = Identset.Bindings.find r !fieldtypes in
      let fty = List.assq f fts in
      let t = locals#fresh in
      PP.fprintf fmt "%a = func.call @%a(%a, %a) : (!%a, %a) -> !%a@,"
        varident t
        (Fun.flip record_field_set r) f
        varident e'
        varident rhs'
        ident r
        (pp_type loc) fty
        ident r;
      rebind loc env v rhs'
  *)

  | Stmt_Assign (LExpr_Array (LExpr_Var v, ix), rhs, loc) ->
      assert (Identset.Bindings.mem v !global_vartypes);
      let ty = Identset.Bindings.find v !global_vartypes in
      let (sz, elty) = ( match ty with
                       | Type_Array (Index_Int (Expr_Lit (VInt sz)), elty) -> (sz, elty)
                       | _ -> let pp fmt = FMT.ty fmt ty in
                              raise (Error.Unimplemented (loc, "type", pp))
                       )
      in
      let (rhs', _) = expr loc env fmt rhs in
      let (ix', _) = expr loc env fmt ix in
      let ix'' = to_index fmt ix' in
      let aref = memref_get_global_array loc fmt v sz elty in
      memref_store_array loc fmt aref ix'' rhs' sz elty;
      false

  | Stmt_Assign (LExpr_Write (f, tes, args, throws), rhs, loc) ->
      let rhs' = expr loc env fmt rhs in
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes args in
      let actuals' = List.map (expr loc env fmt) actuals @ [rhs'] in
      let actuals'' = List.map fst actuals' in
      let formal_env = mk_formal_env fty actuals'' in
      check_actuals loc fmt formal_env fty actuals'';
      PP.fprintf fmt "func.call @%a(%a) : (%a) -> %a@,"
        ident f
        (commasep varident) actuals''
        (formal_arg_types loc) fty
        (pp_return_type loc) fty.rty;
      (* todo: exceptions *)
      false


  | Stmt_TCall (f, tes, args, throws, loc) ->
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes args in
      let actuals' = List.map (expr loc env fmt) actuals in
      let actuals'' = List.map fst actuals' in
      let formal_env = mk_formal_env fty actuals'' in
      check_actuals loc fmt formal_env fty actuals'';
      PP.fprintf fmt "func.call @%a(%a) : (%a) -> %a@,"
        ident f
        (commasep varident) actuals''
        (formal_arg_types loc) fty
        (pp_return_type loc) fty.rty;
      (* todo: exceptions *)
      false

  | Stmt_Block (ss, loc) ->
      block env fmt ss

  (* todo: this generates a long chain of branch to branch to branch to branch at the end - is it worth cleaning this up as we generate code? *)
  | Stmt_If ([], (e, _), _) ->
      block env fmt e
  | Stmt_If ((c, t, loc)::cs, e, l) ->
      let l_true  = labels#fresh in
      let l_false = labels#fresh in
      let l_end   = labels#fresh in

      let (c', _) = expr loc env fmt c in

      let mutables = get_mutables env in
      let renames = List.map (fun (v, curr, t) -> (v, curr, locals#fresh, locals#fresh, t)) mutables in

      let env_true  = fresh_env env (List.map (fun (v, curr, t, f, ty) -> (v, t, ty)) renames) in
      let env_false = fresh_env env (List.map (fun (v, curr, t, f, ty) -> (v, f, ty)) renames) in

      let src_vars = List.map (fun (v, curr, t, f, ty) -> (curr, ty)) renames in
      let true_tgt_vars = List.map (fun (v, curr, t, f, ty) -> (t, ty)) renames in
      let false_tgt_vars = List.map (fun (v, curr, t, f, ty) -> (f, ty)) renames in

      cf_cond_br loc fmt c' l_true src_vars l_false src_vars;

      branch_label loc fmt l_true true_tgt_vars;
      let term_t = block env_true fmt t in
      let binds_true = if term_t
                       then []
                       else make_forward_branch loc fmt env_true mutables l_end
      in

      branch_label loc fmt l_false false_tgt_vars;
      let term_f = stmt env_false fmt (Stmt_If (cs, e, l)) in
      let binds_false = if term_f
                        then []
                        else make_forward_branch loc fmt env_false mutables l_end
      in

      if term_t && term_f then
        true
      else (
        (* todo: check_match loc binds_true binds_false; *)
        let binds = if term_t then binds_false else binds_true in

        let end_vars = List.map (fun (v, tgt, ty) -> (tgt, ty)) binds in
        branch_label loc fmt l_end end_vars;
        update_environment env binds_true;
        false
      )

  | Stmt_Case (e, oty, [], None, loc) ->
      false
  | Stmt_Case (e, oty, [], Some (d, dloc), loc) ->
      block env fmt d
  | Stmt_Case (e, oty, Alt_Alt (ps, None, b, loc)::alts, deflt, case_loc) ->
      let l_true  = labels#fresh in
      let l_false = labels#fresh in
      let l_end   = labels#fresh in

      let (e', _) = expr loc env fmt e in
      let c = patterns loc fmt ps e' in

      let mutables = get_mutables env in
      let renames = List.map (fun (v, init, t) -> (v, init, locals#fresh, locals#fresh, t)) mutables in

      let env_true  = fresh_env env (List.map (fun (v, curr, t, f, ty) -> (v, t, ty)) renames) in
      let env_false = fresh_env env (List.map (fun (v, curr, t, f, ty) -> (v, f, ty)) renames) in

      let src_vars = List.map (fun (v, curr, t, f, ty) -> (curr, ty)) renames in
      let true_tgt_vars = List.map (fun (v, curr, t, f, ty) -> (t, ty)) renames in
      let false_tgt_vars = List.map (fun (v, curr, t, f, ty) -> (f, ty)) renames in

      cf_cond_br loc fmt c l_true src_vars l_false src_vars;

      branch_label loc fmt l_true true_tgt_vars;
      let term_t = block env_true fmt b in
      let binds_true = if term_t
                       then []
                       else make_forward_branch loc fmt env_true mutables l_end
      in

      branch_label loc fmt l_false false_tgt_vars;
      let term_f = stmt env_false fmt (Stmt_Case (e, oty, alts, deflt, case_loc)) in
      let binds_false = if term_f
                        then []
                        else make_forward_branch loc fmt env_false mutables l_end
      in

      if term_t && term_f then
        true
      else (
        (* todo: check_match loc binds_true binds_false; *)
        let binds = if term_t then binds_false else binds_true in

        let end_vars = List.map (fun (v, tgt, ty) -> (tgt, ty)) binds in
        branch_label loc fmt l_end end_vars;
        update_environment env binds_true;
        false
      )

  | Stmt_While (c, b, loc) ->
      let mutables = get_mutables env in
      let renames = List.map (fun (v, init, t) -> (v, init, locals#fresh, locals#fresh, locals#fresh, t)) mutables in

      let l_test = labels#fresh in
      let l_cont = labels#fresh in
      let l_fini = labels#fresh in

      let init_vars = List.map (fun (v, init, test, body, fini, t) -> (init, t)) renames in
      let test_vars = List.map (fun (v, init, test, body, fini, t) -> (test, t)) renames in
      let cont_vars = List.map (fun (v, init, test, body, fini, t) -> (body, t)) renames in
      let fini_vars = List.map (fun (v, init, test, body, fini, t) -> (fini, t)) renames in
      let fini_bind = List.map (fun (v, init, test, body, fini, t) -> (v, fini, t)) renames in
      let test_env  = fresh_env env (List.map (fun (v, init, test, body, fini, t) -> (v, test, t)) renames) in
      let cont_env  = fresh_env env (List.map (fun (v, init, test, body, fini, t) -> (v, body, t)) renames) in

      cf_br loc fmt l_test init_vars;

      branch_label loc fmt l_test test_vars;
      let (c', _) = expr loc test_env fmt c in
      cf_cond_br loc fmt c' l_cont test_vars l_fini test_vars;

      branch_label loc fmt l_cont cont_vars;
      let term = block cont_env fmt b in

      if not term then begin
        let loop_vars = List.map (fun (v, _, ty) -> (get_mutbind loc cont_env v, ty)) mutables in
        cf_br loc fmt l_test loop_vars;
      end;

      branch_label loc fmt l_fini fini_vars;
      update_environment env fini_bind;
      false

  | Stmt_Repeat (b, c, loc1, loc) ->
      let mutables = get_mutables env in
      let renames = List.map (fun (v, init, t) -> (v, init, locals#fresh, locals#fresh, t)) mutables in

      let l_body = labels#fresh in
      let l_fini = labels#fresh in

      let init_vars = List.map (fun (v, init, body, fini, t) -> (init, t)) renames in
      let body_vars = List.map (fun (v, init, body, fini, t) -> (body, t)) renames in
      let fini_vars = List.map (fun (v, init, body, fini, t) -> (fini, t)) renames in
      let fini_bind = List.map (fun (v, init, body, fini, t) -> (v, fini, t)) renames in
      let body_env  = fresh_env env (List.map (fun (v, init, body, fini, t) -> (v, body, t)) renames) in

      cf_br loc fmt l_body init_vars;

      branch_label loc fmt l_body body_vars;
      let term = block body_env fmt b in
      if term then
        true
      else (
        let (c', _) = expr loc body_env fmt c in
        let loop_vars = List.map (fun (v, _, ty) -> (get_mutbind loc body_env v, ty)) mutables in
        cf_cond_br loc fmt c' l_fini loop_vars l_body loop_vars;

        branch_label loc fmt l_fini fini_vars;
        update_environment env fini_bind;
        false
      )

  | Stmt_For (ix, ty, f, direction, t, b, loc) ->
      let (f', _) = expr loc env fmt f in
      let (t', _) = expr loc env fmt t in
      let step = if direction == Direction_Up then Z.one else Z.minus_one in
      let step' = bigint_constant fmt step in

      let l_test = labels#fresh in
      let l_cont = labels#fresh in
      let l_fini = labels#fresh in

      let ix' = locals#fresh in
      let ix'' = locals#fresh in
      let ix''' = locals#fresh in

      let mutables = get_mutables env in
      let renames = List.map (fun (v, init, t) -> (v, init, locals#fresh, locals#fresh, locals#fresh, t)) mutables in

      let init_vars = (f', ty) :: List.map (fun (v, init, test, body, fini, t) -> (init, t)) renames in
      let test_vars = (ix', ty) :: List.map (fun (v, init, test, body, fini, t) -> (test, t)) renames in
      let cont_vars = (ix'', ty) :: List.map (fun (v, init, test, body, fini, t) -> (body, t)) renames in
      let fini_vars = (ix''', ty) :: List.map (fun (v, init, test, body, fini, t) -> (fini, t)) renames in
      let fini_bind = List.map (fun (v, init, test, body, fini, t) -> (v, fini, t)) renames in
      let env_cont  = fresh_env env (List.map (fun (v, init, test, body, fini, t) -> (v, body, t)) renames) in
      ScopeStack.add env_cont ix (Some ix', false, ty);

      cf_br loc fmt l_test init_vars;

      branch_label loc fmt l_test test_vars;
      let continue = ( match direction with
                     | Direction_Up -> int_le fmt ix' t'
                     | Direction_Down -> int_le fmt t' ix'
                     )
      in
      cf_cond_br loc fmt continue l_cont test_vars l_fini test_vars;

      branch_label loc fmt l_cont cont_vars;
      let term = block env_cont fmt b in

      if not term then begin
        let next = int_add fmt ix'' step' in
        ignore (ScopeStack.set env_cont ix (Some next, false, ty));
        make_backward_branch loc fmt env_cont ((ix, next, ty)::mutables) l_test
      end;

      branch_label loc fmt l_fini fini_vars;
      update_environment env fini_bind;
      false

  | Stmt_Throw _
  | Stmt_Try _
  | _ ->
      let pp fmt = FMT.stmt fmt x in
      raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and block (env : environment) (fmt : PP.formatter) (xs : AST.stmt list) : bool =
  ScopeStack.nest env (fun env' ->
    let rec stmts (xs : AST.stmt list) : bool =
      ( match xs with
      | [] -> false
      | (x :: xs) ->
          if stmt env fmt x then
            true
          else
            stmts xs
      )
    in
    stmts xs
  )

and indented_block (env : environment) (fmt : PP.formatter) (xs : AST.stmt list) : bool =
  if xs <> [] then
    indented fmt (fun _ -> block env fmt xs)
  else
    false

(****************************************************************
 * Declarations
 ****************************************************************)

let declaration (fmt : PP.formatter) ?(is_extern : bool option) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      ( match x with
      | Decl_BuiltinType _
      | Decl_Forward _
      | Decl_Operator1 _
      | Decl_Operator2 _
      | Decl_FunType _
        -> ()
      | Decl_FunDefn (f, fty, body, loc) when fty.is_builtin
        -> ()
      | Decl_FunDefn (f, fty, b, loc) ->
          locals#reset;
          labels#reset;
          let env : environment = ScopeStack.empty () in
          List.iter (fun (v, oty) -> ScopeStack.add env v (None, false, Option.get oty)) fty.parameters;
          List.iter (fun (v, ty, _) -> ScopeStack.add env v (None, false, ty)) fty.args;
          Option.iter (fun (v, ty) -> ScopeStack.add env v (None, false, ty)) fty.setter_arg;
          PP.fprintf fmt "@,func.func @%a(%a) -> %a {@,"
            ident f
            (formal_args_decls loc) fty
            (pp_return_type loc) fty.rty;
          return_label := labels#fresh;
          return_types :=
              ( match fty.rty with
              | Type_Tuple [] -> []
              | Type_Tuple tys -> tys
              | t -> [t]
              );
          return_vars :=
              ( match fty.rty with
              | Type_Tuple([]) -> []
              | Type_Tuple(tys) -> List.map (fun ty -> (locals#fresh, ty)) tys
              | rty -> [(locals#fresh, rty)]
              );
          indented fmt (fun _ ->
            if !type_checks then begin
                List.iter (fun (v, t) ->
                  let requires = check_type loc env fmt v t in
                  Option.iter (cf_assume fmt) requires
                  )
                  (formal_args fty)
            end;
            let term = block env fmt b in
            if not term && List.is_empty !return_vars then begin
              cf_br loc fmt !return_label []
            end
          );

          branch_label loc fmt !return_label !return_vars;
          if List.is_empty !return_vars then begin
            PP.fprintf fmt "    func.return@,"
          end else begin
            PP.fprintf fmt "    func.return %a : %a@,"
              (commasep (fun fmt (v, t) -> varident fmt v)) !return_vars
              (commasep (fun fmt (v, t) -> pp_type loc fmt t)) !return_vars
          end;
          PP.fprintf fmt "}@,@,"
      | Decl_Var (v, Type_Array (Index_Int (Expr_Lit (VInt sz)), elty), loc) ->
          memref_global_array loc fmt v sz elty
      | Decl_Var (v, ty, loc) ->
          memref_global_scalar loc fmt v ty
      | Decl_Const (v, Some ty, e, loc) -> (* todo: don't treat this like a variable! *)
          memref_global_scalar loc fmt v ty
      | Decl_Exception _
      | Decl_Typedef _
      | Decl_Enum _
      | Decl_FunInstance _
      | Decl_FunFFI _
      | Decl_VarFFI _
      | Decl_TypeFFI _
      | _ ->
          ( match Isa_utils.decl_name x with
          | Some nm -> PP.fprintf fmt "// skipping %a@," ident nm
          | None -> ()
          )
      ))

let declaration' (fmt : PP.formatter) (x : AST.declaration) : unit =
  try
    declaration fmt x
  with
  | e -> begin
    PP.fprintf fmt "@.ERROR@.";
    PP.fprintf fmt "@.";
    (* Error.fprint_exception fmt e; *)
    PP.fprintf Format.std_formatter " ERROR@.";
    Error.fprint_exception Format.std_formatter e
  end

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ -> map fmt (declaration' fmt) xs)

(****************************************************************
 * Command: :to_mlir
 ****************************************************************)

let _ =
  let opt_filename = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Utils.to_file !opt_filename (fun fmt ->
      let decls = !Commands.declarations in

      (* record function types *)
      List.iter (fun d ->
        ( match d with
        | AST.Decl_FunType (f, fty, _)
        | AST.Decl_FunDefn (f, fty, _, _)
        -> funtypes := Identset.Bindings.add f fty !funtypes
        | AST.Decl_Var (v, ty, _)
        -> global_vartypes := Identset.Bindings.add v ty !global_vartypes
        | Decl_Const (v, Some ty, e, _) (* todo: don't treat this like a variable! *)
        -> global_vartypes := Identset.Bindings.add v ty !global_vartypes
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
            PP.fprintf fmt "func.func private @%a(%a) -> %a@,"
              ident f
              (formal_args_decls loc) fty
              (pp_return_type loc) fty.rty;
        )
      ) standard_functions;

      (* declare records *)
      List.iter (fun d ->
        ( match d with
        | AST.Decl_Record (r, [], fs, loc) ->
            fieldtypes := Identset.Bindings.add r fs !fieldtypes;
            PP.fprintf fmt "@,!%a = tuple<%a>@,"
              ident r
              (commasep (pp_type loc)) (List.map (fun (f, t) -> t) fs);
            PP.fprintf fmt "func.func private @%a(%a) -> !%a@,"
              record_constructor r
              (commasep (varty loc)) fs
              ident r;
            List.iter (fun (v, t) ->
                PP.fprintf fmt "func.func private @%a(%%x : !%a) -> %a@,"
                  (fun fmt -> record_field_get fmt r) v
                  ident r
                  (pp_type loc) t;
                PP.fprintf fmt "func.func private @%a(%%x : !%a, %%y : %a) -> !%a@,"
                  (fun fmt -> record_field_set fmt r) v
                  ident r
                  (pp_type loc) t
                  ident r
              )
              fs;
            PP.fprintf fmt "@,"
        | _ -> ()
        )
      ) decls;

      declarations fmt (List.rev decls)
    );
    true
  in

  let flags = Arg.align [
        ("--typecheck",     Arg.Set type_checks,                 "              Insert dynamic typechecks");
        ("--no-typecheck",  Arg.Clear type_checks,               "              Don't insert dynamic typechecks");
        ("--output-file",   Arg.Set_string opt_filename,         "<filename>    Output MLIR file");
      ]
  in
  Commands.registerCommand "to_mlir" flags [] [] "Convert to MLIR" cmd

(****************************************************************
 * End
 ****************************************************************)
