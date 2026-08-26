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
  ( match oy with
  | Some y -> Some (f x y)
  | None -> None
  )

(****************************************************************
 * Pretty printing helpers
 ****************************************************************)

let vbox (fmt : PP.formatter) (pp : unit -> 'a) : 'a=
  PP.pp_open_vbox fmt 0;
  let r = pp () in
  PP.pp_close_box fmt ();
  r

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

let vartypes : AST.ty Identset.Bindings.t ref = ref Identset.Bindings.empty
let funtypes : AST.function_type Identset.Bindings.t ref = ref Identset.Bindings.empty

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

let return_type = ref (AST.Type_Tuple [])

(* the environment tracks the following about local variables
 * - for mutable variables, what SSA variable holds its current value
 * - is it a mutable variable (needed for uninitialized variables)
 * - their type
 *)

type env_entry = (Ident.t option * bool * AST.ty)
type environment = env_entry ScopeStack.t
let locals = new Isa_utils.nameSupply "%"

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
  |> List.concat_map (List.filter_map (fun (v, (b, mut, ty)) ->
       if mut then (
         let v' = locals#fresh in
         Some (v, v', ty)
       ) else (
         None
       )))

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

let update_environment (env : environment) (rename : renaming) =
  List.iter (fun (v, v', ty) ->
      ScopeStack.add env v (Some v', true, ty)
    )
    rename

(****************************************************************
 * Functions
 ****************************************************************)

let varty (loc : Loc.t) (fmt : PP.formatter) (x : (Ident.t * AST.ty)) : unit =
  let (v, t) = x in
  PP.fprintf fmt "%a : %a"
    varident v
    (pp_type loc) t

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

let with_fresh (f : Ident.t -> unit) : Ident.t =
  let t = locals#fresh in
  f t;
  t

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

let rec concat (fmt : PP.formatter) (xs : (Ident.t * Ident.t) list) : (Ident.t * Ident.t) =
  ( match xs with
  | [] ->
     let zero = bigint_constant fmt Z.zero in
     (bitvector_constant fmt Primops.empty_bits, zero)
  | [(x, xw)] -> (x, xw)
  | ((y, yw) :: ys) ->
      let (ys', ysw) = concat fmt ys in
      let w = with_fresh (fun w ->
        PP.fprintf fmt "%a = func.call @Std$Integer$Add(%a, %a) : (!Std$Integer, !Std$Integer) -> !Std$Integer@,"
          varident w
          varident yw
          varident ysw
      ) in
      let t = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Bits$Append(%a, %a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Bits, !Std$Bits) -> !Std$Bits@,"
        varident t
        varident yw
        varident ysw
        varident y
        varident ys';
      (t, w)
  )

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : Ident.t =
  ( match x with
  | VBool v   -> bool_constant fmt v
  | VInt v    -> bigint_constant fmt v
  | VBits v   -> bitvector_constant fmt v
  | VString v -> string_constant fmt v
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

let rec expr (loc : Loc.t) (env : environment) (fmt : PP.formatter) (x : AST.expr) : Ident.t =
  ( match x with
  | Expr_Lit v -> valueLit loc fmt v

  | Expr_Var v ->
      if Ident.equal v Builtins.true_ident then bool_constant fmt true
      else if Ident.equal v Builtins.false_ident then bool_constant fmt false
      else (
        (* todo: enumeration variables *)
        ( match ScopeStack.get env v with
        | None -> (* global variable *)
            assert (Identset.Bindings.mem v !vartypes);
            let ty = Identset.Bindings.find v !vartypes in
            let ref = locals#fresh in
            PP.fprintf fmt "%a = memref.get_global @@%a : memref<%a>@,"
              varident ref
              ident v
              (pp_type loc) ty;
            let t = locals#fresh in
            PP.fprintf fmt "%a = memref.load %a[] : memref<%a>@,"
              varident t
              varident ref
              (pp_type loc) ty;
            t
        | Some (Some v', _, _) -> v'
        | Some (None, _, _) -> v
        )
      )

  | Expr_Array(Expr_Var v, ix) ->
      assert (Identset.Bindings.mem v !vartypes);
      let ty = Identset.Bindings.find v !vartypes in
      let (sz, elty) = ( match ty with
                      | Type_Array (Index_Int (Expr_Lit (VInt sz)), elty) -> (sz, elty)
                      | _ -> let pp fmt = FMT.ty fmt ty in
                             raise (Error.Unimplemented (loc, "type", pp))
                      )
      in
      let aref = locals#fresh in
      PP.fprintf fmt "%a = memref.get_global @@%a : memref<%sx%a>@,"
        varident aref
        ident v
        (Z.to_string sz)
        (pp_type loc) elty;
      let ix' = expr loc env fmt ix in
      let ix'' = to_index fmt ix' in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = memref.load %a[%a] : memref<%sx%a>@,"
          varident t
          varident aref
          varident ix''
          (Z.to_string sz)
          (pp_type loc) elty
      )

  | Expr_Slices (Type_Integer _, e, [Slice_Single i]) ->
      let e' = expr loc env fmt e in
      let i' = expr loc env fmt i in
      let wd' = bigint_constant fmt Z.one in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = func.call @Std$Integer$Slice(%a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
          varident t
          varident e'
          varident i'
          varident wd'
      )

  | Expr_Slices (Type_Integer _, e, [Slice_LoWd (lo, wd)]) ->
      let e' = expr loc env fmt e in
      let lo' = expr loc env fmt lo in
      let wd' = expr loc env fmt wd in
      with_fresh (fun t ->
        PP.fprintf fmt "%a = func.call @Std$Integer$Slice(%a, %a, %a) : (!Std$Integer, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
          varident t
          varident e'
          varident lo'
          varident wd'
      )

  | Expr_Slices (Type_Bits _, e, ss) ->
      let e' = expr loc env fmt e in
      slices loc env fmt e' ss

  (*
  | Expr_Let (v, t, e, b) ->
      let v' = expr loc env fmt e in
      expr loc env fmt b
  *)

  | Expr_TApply (f, tes, es, NoThrow) ->
      (* todo: primops *)
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes es in
      let actuals' = List.map (expr loc env fmt) actuals in
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
      r

  | Expr_If ([], e, oty) ->
      expr loc env fmt e
  | Expr_If ((c, t) :: cts, e, Some ty) ->
      let c' = expr loc env fmt c in
      let r = locals#fresh in
      PP.fprintf fmt "%a = scf.if %a -> %a {@,"
        varident r
        varident c'
        (pp_type loc) ty;
      let t' = indented fmt (fun _ -> expr loc env fmt t) in
      PP.fprintf fmt "scf.yield %a : %a@," varident t' (pp_type loc) ty;
      PP.fprintf fmt "@,} else {@,";
      let e' = indented fmt (fun _ -> expr loc env fmt (Expr_If (cts, e, Some ty))) in
      PP.fprintf fmt "scf.yield %a : %a@," varident e' (pp_type loc) ty;
      PP.fprintf fmt "@,}@,";
      r

  | _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

and slices (loc : Loc.t) (env : environment) (fmt : PP.formatter) (b : Ident.t) (xs : AST.slice list) : Ident.t =
  let xs' = List.map (slice loc env fmt b) xs in
  let (r, _) = concat fmt xs' in
  r

and slice (loc : Loc.t) (env : environment) (fmt : PP.formatter) (b : Ident.t) (x : AST.slice) : (Ident.t * Ident.t) =
  ( match x with
  | Slice_Single i ->
      let i' = expr loc env fmt i in
      let w' = bigint_constant fmt Z.one in
      let t = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Bits$Slice(%a, %a, %a) : (!Std$Bits, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
        varident t
        varident b
        varident i'
        varident w';
      (t, w')
  | Slice_LoWd (lo, wd) ->
      let lo' = expr loc env fmt lo in
      let wd' = expr loc env fmt wd in
      let t = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Bits$Slice(%a, %a, %a) : (!Std$Bits, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
        varident t
        varident b
        varident lo'
        varident wd';
      (t, wd')

  | Slice_HiLo (hi, lo) ->
      let hi' = expr loc env fmt hi in
      let lo' = expr loc env fmt lo in
      let t0 = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Integer$Subtract(%a, %a) : (!Std$Integer, !Std$Integer) -> !Std$Integer@,"
        varident t0
        varident hi'
        varident lo';
      let one = bigint_constant fmt Z.one in
      let wd = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Integer$Add(%a, %a) : (!Std$Integer, !Std$Integer) -> !Std$Integer@,"
        varident wd
        varident t0
        varident one;
      let t = locals#fresh in
      PP.fprintf fmt "%a = func.call @Std$Bits$Slice(%a, %a, %a) : (!Std$Bits, !Std$Integer, !Std$Integer) -> !Std$Bits@,"
        varident t
        varident b
        varident lo'
        varident wd;
      (t, wd)

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
  | Set_Single e -> Some (int_eq fmt v (expr loc env fmt e))
  | Set_Range (lo, hi) ->
      let c_lo = lift (Fun.flip (int_le fmt)) v (Option.map (expr loc env fmt) lo) in
      let c_hi = lift (int_le fmt) v (Option.map (expr loc env fmt) hi) in
      option_blend (bool_and fmt) c_lo c_hi
  )

and check_type (loc : Loc.t) (env : environment) (fmt : PP.formatter) (v : Ident.t) (x : AST.ty) : Ident.t option =
  ( match x with
  | Type_Bits (e, _) ->
      let t = bv_length fmt v in
      let e' = expr loc env fmt e in
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

(****************************************************************
 * Statements
 ****************************************************************)

let rec stmt (env : environment) (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_Assert (e, loc) ->
      let e' = expr loc env fmt e in
      PP.fprintf fmt "cf.assert %a, \"%a\""
        varident e'
        FMT.expr e

  | Stmt_Return (Expr_Tuple [], loc) ->
      PP.fprintf fmt "@,func.return@,"

  | Stmt_Return (e, loc) ->
      let e' = expr loc env fmt e in
      if !type_checks then begin
        let ensures = check_type loc env fmt e' !return_type in
        Option.iter (cf_assert fmt) ensures
      end;
      PP.fprintf fmt "@,func.return %a : %a@,"
        varident e'
        (pp_return_type loc) !return_type

  | Stmt_VarDecl (is_constant, DeclItem_Var (v, Some ty), i, loc) ->
      let i' = expr loc env fmt i in
      ScopeStack.add env v (Some i', is_constant, ty)

  | Stmt_Assign (LExpr_Var v, rhs, loc) ->
      let rhs' = expr loc env fmt rhs in
      if Identset.Bindings.mem v !vartypes then begin (* global *)
        let ty = Identset.Bindings.find v !vartypes in
        let ref = locals#fresh in
        PP.fprintf fmt "%a = memref.get_global @@%a : memref<%a>@,"
          varident ref
          ident v
          (pp_type loc) ty;
        PP.fprintf fmt "memref.store %a, %a[] : memref<%a>@,"
          varident rhs'
          varident ref
          (pp_type loc) ty
      end else begin
        rebind loc env v rhs'
      end

  | Stmt_Assign (LExpr_Array (LExpr_Var v, ix), rhs, loc) ->
      let rhs' = expr loc env fmt rhs in
      assert (Identset.Bindings.mem v !vartypes);
      let ty = Identset.Bindings.find v !vartypes in
      let (sz, elty) = ( match ty with
                      | Type_Array (Index_Int (Expr_Lit (VInt sz)), elty) -> (sz, elty)
                      | _ -> let pp fmt = FMT.ty fmt ty in
                             raise (Error.Unimplemented (loc, "type", pp))
                      )
      in
      let aref = locals#fresh in
      PP.fprintf fmt "%a = memref.get_global @@%a : memref<%sx%a>@,"
        varident aref
        ident v
        (Z.to_string sz)
        (pp_type loc) elty;
      let ix' = expr loc env fmt ix in
      let ix'' = to_index fmt ix' in
      PP.fprintf fmt "memref.store %a, %a[%a] : memref<%sx%a>@,"
        varident rhs'
        varident aref
        varident ix''
        (Z.to_string sz)
        (pp_type loc) elty

  | Stmt_Assign (LExpr_Write (f, tes, args, throws), rhs, loc) ->
      let rhs' = expr loc env fmt rhs in
      (* todo: exceptions *)
      (* todo: primops *)
      (* todo: check type constraints *)
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes args in
      let actuals' = List.map (expr loc env fmt) actuals @ [rhs'] in
      let formal_env = mk_formal_env fty actuals' in
      check_actuals loc fmt formal_env fty actuals';
      PP.fprintf fmt "func.call @%a(%a) : (%a) -> %a@,"
        ident f
        (commasep varident) actuals'
        (formal_arg_types loc) fty
        (pp_return_type loc) fty.rty


  | Stmt_TCall (f, tes, args, throws, loc) ->
      (* todo: exceptions *)
      (* todo: primops *)
      let fty = Identset.Bindings.find f !funtypes in
      let actuals = actual_args fty tes args in
      let actuals' = List.map (expr loc env fmt) actuals in
      let formal_env = mk_formal_env fty actuals' in
      check_actuals loc fmt formal_env fty actuals';
      PP.fprintf fmt "func.call @%a(%a) : (%a) -> %a@,"
        ident f
        (commasep varident) actuals'
        (formal_arg_types loc) fty
        (pp_return_type loc) fty.rty

  | Stmt_Block (ss, loc) ->
      block env fmt ss

  | Stmt_If ([], (e, _), _) ->
      block env fmt e
  | Stmt_If ((c, t, loc)::cs, e, l) ->
      let c' = expr loc env fmt c in
      let rename = mk_renaming env in
      let envt = ScopeStack.clone env in
      pp_yield_vars fmt rename;
      PP.fprintf fmt "scf.if %a " varident c';
      pp_yield_type loc fmt rename;
      PP.fprintf fmt " {@,";
      indented_block envt fmt t;
      pp_yield loc envt fmt "scf.yield" rename;
      PP.fprintf fmt "@,} else {";
      let envf = ScopeStack.clone env in
      indented fmt (fun _ -> stmt envf fmt (Stmt_If (cs, e, l)));
      pp_yield loc envf fmt "scf.yield" rename;
      PP.fprintf fmt "@,}@,";
      update_environment env rename

  | Stmt_Case (e, oty, [], None, loc) ->
      ()

  | Stmt_Case (e, oty, [], Some (d, dloc), loc) ->
      block env fmt d

  | Stmt_Case (e, oty, Alt_Alt (ps, None, b, loc)::alts, deflt, case_loc) ->
      let e' = expr loc env fmt e in
      let c = patterns loc fmt ps e' in
      let rename = mk_renaming env in
      let envt = ScopeStack.clone env in
      pp_yield_vars fmt rename;
      PP.fprintf fmt "scf.if %a {" varident c;
      pp_yield_type loc fmt rename;
      indented_block env fmt b;
      pp_yield loc envt fmt "scf.yield" rename;
      PP.fprintf fmt "@,} else {";
      let envf = ScopeStack.clone env in
      indented fmt (fun _ -> stmt env fmt (Stmt_Case (e, oty, alts, deflt, case_loc)));
      pp_yield case_loc envf fmt "scf.yield" rename;
      PP.fprintf fmt "@,}@,";
      update_environment env rename

  | Stmt_While (c, b, loc) ->
      let mutables =
        ScopeStack.bindings env
        |> List.concat_map (List.filter_map (fun (v, (ob, mut, ty)) ->
             ( match ob with
             | Some current ->
                 let v_cond = locals#fresh in
                 let v_body = locals#fresh in
                 let v_result = locals#fresh in
                 Some (v, current, v_cond, v_body, v_result, ty)
             | None ->
                 None
             )))
      in
      let cond_rename = List.map (fun (v, current, v_cond, v_body, v_result, ty) -> (v, v_cond, ty)) mutables in
      let body_rename = List.map (fun (v, current, v_cond, v_body, v_result, ty) -> (v, v_body, ty)) mutables in
      let final_rename = List.map (fun (v, current, v_cond, v_body, v_result, ty) -> (v, v_result, ty)) mutables in
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) (List.map (fun (v, current, v_cond, v_body, v_result, ty) -> v_result) mutables)
      end;
      PP.fprintf fmt "scf.while (";
      if not (Utils.is_empty mutables) then begin
        commasep (fun fmt (v, current, v_cond, v_body, v_result, ty) ->
            PP.fprintf fmt "%a = %a"
              varident v_cond
              varident current)
            fmt
            mutables;
        PP.fprintf fmt ") : (";
        commasep (pp_type loc) fmt (List.map (fun (v, current, v_cond, v_body, v_result, ty) -> ty) mutables);
        PP.fprintf fmt ") -> (";
        commasep (pp_type loc) fmt (List.map (fun (v, current, v_cond, v_body, v_result, ty) -> ty) mutables);
      end;
      PP.fprintf fmt ") {@,";
      indented fmt (fun _ ->
        let cond_env = ScopeStack.clone env in
        update_environment cond_env cond_rename;
        let c' = expr loc env fmt c in

        let (vars, fresh_vars, types) = Utils.split3 cond_rename in
        let vars' = List.map (get_mutbind loc cond_env) vars in
        PP.fprintf fmt "scf.condition (%a) %a : %a@,"
          varident c'
          (commasep varident) vars'
          (commasep (pp_type loc)) types
      );

      PP.fprintf fmt "@,} do {@,";

      indented fmt (fun _ ->
          PP.fprintf fmt "^bb0(";
          commasep (fun fmt (v, current, v_cond, v_body, v_result, ty) ->
            PP.fprintf fmt "%a : %a"
              varident v_body
              (pp_type loc) ty
            )
            fmt
            mutables;
          PP.fprintf fmt "):@,";
          indented fmt (fun _ ->
            let body_env = ScopeStack.clone env in
            update_environment body_env body_rename;
            block body_env fmt b;
            pp_yield loc body_env fmt "scf.yield" body_rename;
          )
      );
      PP.fprintf fmt "@,}@,";
      update_environment env final_rename

  | Stmt_For (ix, ty, f, direction, t, b, loc) ->
      let f' = to_index fmt (expr loc env fmt f) in
      let t' = to_index fmt (expr loc env fmt t) in
      let step = if direction == Direction_Up then Z.one else Z.minus_one in
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
      let step' = to_index fmt (bigint_constant fmt step) in
      let ix' = locals#fresh in
      PP.fprintf fmt  "//  todo: need to increment upper bound@,";
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt "%a = "
          (commasep varident) (List.map (fun (_, _, _, finalv, _) -> finalv) mutables)
      end;
      PP.fprintf fmt  "scf.for %a = %a to %a step %a "
        varident ix'
        varident f'
        varident t'
        varident step';
      if not (Utils.is_empty mutables) then begin
        PP.fprintf fmt "iter_args (";
        commasep (fun fmt (v, current, loopv, _, _) -> PP.fprintf fmt "%a = %a"
            varident loopv
            varident current)
          fmt
          mutables;
        PP.fprintf fmt ") -> (%a) "
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt "{@,";
      PP.fprintf fmt "    %a = index.casts %a : index to !Std$Integer@,"
        varident ix
        varident ix';
      ScopeStack.nest env (fun env' ->
        ScopeStack.add env' ix (None, false, Isa_utils.type_integer);
        List.iter (fun (v, current, loopv, finalv, ty) ->
          ScopeStack.add env' v (Some loopv, true, Isa_utils.type_integer);
          )
          mutables;
        indented_block env' fmt b);
      if Utils.is_empty mutables then begin
        PP.fprintf fmt "scf.yield";
      end else begin
        PP.fprintf fmt "scf.yield %a : %a"
          (commasep varident) (List.map (fun (v, _, _, _, _) -> get_mutbind loc env v) mutables)
          (commasep (pp_type loc)) (List.map (fun (_, _, _, _, ty) -> ty) mutables)
      end;
      PP.fprintf fmt "@,}@,";
      List.iter (fun (v, _, _, finalv, ty) ->
          ScopeStack.add env v (Some finalv, true, ty)
        )
        mutables

  | _ ->
      let pp fmt = FMT.stmt fmt x in
      raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and block (env : environment) (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  ScopeStack.nest env (fun env' ->
    cutsep (stmt env) fmt xs
  )

and indented_block (env : environment) (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ -> block env fmt xs)
  end

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
          let env : environment = ScopeStack.empty () in
          List.iter (fun (v, oty) -> ScopeStack.add env v (None, false, Option.get oty)) fty.parameters;
          List.iter (fun (v, ty, _) -> ScopeStack.add env v (None, false, ty)) fty.args;
          Option.iter (fun (v, ty) -> ScopeStack.add env v (None, false, ty)) fty.setter_arg;
          PP.fprintf fmt "@,func.func @%a(%a) -> %a {@,"
            ident f
            (formal_args_decls loc) fty
            (pp_return_type loc) fty.rty;
          return_type := fty.rty;
          indented fmt (fun _ ->
            if !type_checks then begin
                List.iter (fun (v, t) ->
                  let requires = check_type loc env fmt v t in
                  Option.iter (cf_assume fmt) requires
                  )
                  (formal_args fty)
            end;
            block env fmt b
          );
          ( match fty.rty with
          | Type_Tuple([]) -> PP.fprintf fmt "func.return@,"
          | _ -> ()
          );
          PP.fprintf fmt "}@,@,"
      | Decl_Var (v, Type_Array (Index_Int (Expr_Lit (VInt sz)), elty), loc) ->
          PP.fprintf fmt "memref.global @%a : memref<%sx%a>@,@,"
            ident v
            (Z.to_string sz)
            (pp_type loc) elty
      | Decl_Var (v, ty, loc) ->
          PP.fprintf fmt "memref.global @%a : memref<%a>@,@,"
            ident v
            (pp_type loc) ty
      | Decl_Const (v, Some ty, e, loc) -> (* todo: don't treat this like a variable! *)
          PP.fprintf fmt "memref.global @%a : memref<%a>@,@,"
            ident v
            (pp_type loc) ty
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
        -> vartypes := Identset.Bindings.add v ty !vartypes
        | Decl_Const (v, Some ty, e, _) (* todo: don't treat this like a variable! *)
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
            PP.fprintf fmt "func.func private @%a(%a) -> %a@,"
              ident f
              (formal_args_decls loc) fty
              (pp_return_type loc) fty.rty;
        )
      ) standard_functions;

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
