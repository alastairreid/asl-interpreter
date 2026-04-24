(****************************************************************
 * High level intermediate representation
 *
 * An SSA+region-based IR inspired by MLIR but supporting
 * - Single-Entry, Multiple-Exit regions (e.g., fn return)
 * - Dependent types
 * - [planned] Foreign function interface
 *
 * Copyright (C) 2025-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast
module FMT = Isa_fmt
module PP = Format
module FMTUtils = Format_utils

(****************************************************************
 * Pretty printing helpers
 ****************************************************************)

let vbox (fmt : PP.formatter) (pp : unit -> 'a) : 'a=
  PP.pp_open_vbox fmt 0;
  let r = pp () in
  PP.pp_close_box fmt ();
  r

(****************************************************************
 * HLIR AST
 ****************************************************************)

(* The IR needs to be able to refer to references to types *)
type ty =
  | Type of AST.ty
  | Ref of AST.ty

type ident =
  Ident of Ident.t * ty

type op =
  | Builtin of Ident.t
  | Call of Ident.t
  | Constant of Value.value
  | MkRef of Ident.t
  | AddIndex
  | Load
  | Store
  | Symbol of Ident.t
  | For of bool
  | While
  | Repeat
  | If
  | Case
  | Fail (* try next pattern in Case operation *)
  | Return
  | Assert of string

(* Note: at the moment, we do not need successors *)
type operation = {
  results : ident list;
  op : op;
  operands : ident list;
  regions : region list;
  loc : Loc.t;
}

(* At the moment, we do not use the block abstraction at all and
 * we capture the inputs/ outputs of a region as a list of idents *)
and region = {
  outputs : ident list;
  operations : operation list;
  inputs : ident list;
}

type global =
  | Variable of Ident.t * ty * Loc.t
  | Function of Ident.t * region * Loc.t

(****************************************************************
 * HLIR Pretty-printing
 ****************************************************************)

let show_loc = ref false

let ppType (fmt : PP.formatter) (x : ty) : unit =
  ( match x with
  | Type t -> FMT.ty fmt t
  | Ref t -> PP.fprintf fmt "AIR.Ref<%a>" FMT.ty t
  )

let ppIdent (fmt : PP.formatter) (x : ident) : unit =
  let Ident(v, t) = x in
  PP.fprintf fmt "%a : %a"
    Ident.pp v
    ppType t

let ppIdentName (fmt : PP.formatter) (x : ident) : unit =
  let Ident(v, t) = x in
  Ident.pp fmt v

let ppIdentType (fmt : PP.formatter) (x : ident) : unit =
  let Ident(v, t) = x in
  ppType fmt t

let ppOp (fmt : PP.formatter) (x : op) : unit =
  ( match x with
  | Builtin f -> Ident.pp fmt f
  | Call f -> PP.fprintf fmt "HLIR.call @%a" Ident.pp f
  | Constant c -> PP.fprintf fmt "HLIR.constant %a" Value.pp_value c
  | Symbol v -> PP.fprintf fmt "HLIR.symbol @%a" Ident.pp v
  | MkRef v -> PP.fprintf fmt "HLIR.ref @%a" Ident.pp v
  | AddIndex -> PP.fprintf fmt "HLIR.add_index"
  | Load -> PP.fprintf fmt "HLIR.load"
  | Store -> PP.fprintf fmt "HLIR.store"
  | For up -> PP.fprintf fmt "HLIR.for(%s)" (if up then "up" else "down")
  | While -> PP.pp_print_string fmt "HLIR.while"
  | Repeat -> PP.pp_print_string fmt "HLIR.repeat"
  | If -> PP.pp_print_string fmt "HLIR.if"
  | Case -> PP.pp_print_string fmt "HLIR.case"
  | Fail -> PP.pp_print_string fmt "HLIR.fail"
  | Return -> PP.pp_print_string fmt "HLIR.return"
  | Assert msg -> PP.fprintf fmt "HLIR.assert(%s)" msg
  )

let rec ppOperation (fmt : PP.formatter) (x : operation) : unit =
  PP.fprintf fmt "(%a) = %a(%a) ("
    (Utils.commasep ppIdentName) x.results
    ppOp x.op
    (Utils.commasep ppIdentName) x.operands;
  if not (Utils.is_empty x.regions) then begin
    FMTUtils.indented fmt (fun _ ->
      List.iter (ppRegion fmt) x.regions
    )
  end;
  PP.fprintf fmt ") : (%a) -> (%a)"
    (Utils.commasep ppIdentType) x.operands
    (Utils.commasep ppIdentType) x.results;
  if !show_loc then PP.fprintf fmt " // %a" Loc.pp x.loc;
  PP.fprintf fmt "@,"

and ppRegion (fmt : PP.formatter) (x : region) : unit =
  PP.fprintf fmt "{ input (%a)" (Utils.commasep ppIdent) x.inputs;
  FMTUtils.indented fmt (fun _ ->
    List.iter (ppOperation fmt) x.operations;
  );
  PP.fprintf fmt "@,output (%a)@,}@," (Utils.commasep ppIdent) x.outputs

let ppGlobal (fmt : PP.formatter) (x : global) : unit =
  ( match x with
  | Variable (v, ty, loc) ->
      PP.fprintf fmt "global @%a : %a"
        Ident.pp v
        ppType ty;
      if !show_loc then PP.fprintf fmt " // %a" Loc.pp loc;
      PP.fprintf fmt "@,\n"
  | Function (f, r, loc) ->
      PP.fprintf fmt "function @%a" Ident.pp f;
      if !show_loc then PP.fprintf fmt " // %a" Loc.pp loc;
      PP.fprintf fmt "@,";
      ppRegion fmt r;
      PP.fprintf fmt "}@,\n"
  )

(****************************************************************
 * Utilities
 ****************************************************************)

let typeof (x : ident) : ty =
  let Ident(v, t) = x in
  t

let mkType (x : AST.ty) : ty =
  Type x

(****************************************************************
 * End
 ****************************************************************)

