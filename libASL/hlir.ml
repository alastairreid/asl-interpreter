(****************************************************************
 * High level intermediate representation
 *
 * An SSA+region-based IR inspired by MLIR but supporting
 * - Single-Entry, Multiple-Exit regions (e.g., fn return)
 * - Dependent types
 * - [planned] Foreign function interface
 *
 * Copyright (C) 2025-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module FMT_Utils = Format_utils

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
  | Variable of Ident.t * AST.ty * Loc.t
  | Function of Ident.t * region * Loc.t

(****************************************************************
 * HLIR Pretty-printing
 ****************************************************************)

let ppType (fmt : PP.formatter) (x : ty) : unit =
  ( match x with
  | Type t -> FMT.ty fmt t
  | Ref t -> Format.fprintf fmt "AIR.Ref<%a>" FMT.ty t
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
  | Call f -> Format.fprintf fmt "HLIR.call @%a" Ident.pp f
  | Constant c -> Format.fprintf fmt "HLIR.constant %a" Value.pp_value c
  | Symbol v -> Format.fprintf fmt "HLIR.symbol @%a" Ident.pp v
  | MkRef v -> Format.fprintf fmt "HLIR.ref @%a" Ident.pp v
  | AddIndex -> Format.fprintf fmt "HLIR.add_index"
  | Load -> Format.fprintf fmt "HLIR.load"
  | Store -> Format.fprintf fmt "HLIR.store"
  | While -> Format.pp_print_string fmt "HLIR.while"
  | Repeat -> Format.pp_print_string fmt "HLIR.repeat"
  | If -> Format.pp_print_string fmt "HLIR.if"
  | Case -> Format.pp_print_string fmt "HLIR.case"
  | Fail -> Format.pp_print_string fmt "HLIR.fail"
  | Return -> Format.pp_print_string fmt "HLIR.return"
  | Assert msg -> Format.fprintf fmt "HLIR.assert(%s)" msg
  )

let rec ppOperation (fmt : PP.formatter) (x : operation) : unit =
  Format.fprintf fmt "(%a) = %a(%a) ("
    (commasep ppIdentName) x.results
    ppOp x.op
    (commasep ppIdentName) x.operands;
  List.iter (fun r ->
    Format.fprintf fmt "{@,";
    ppRegion fmt r;
    Format.fprintf fmt "}";
  ) x.regions;
  Format.fprintf fmt ") : (%a) -> (%a)"
    (commasep ppIdentType) x.operands
    (commasep ppIdentType) x.results;
  Format.fprintf fmt " // %a@,"
    Loc.pp x.loc

and ppRegion (fmt : PP.formatter) (x : region) : unit =
  FMT_Utils.indented fmt (fun _ ->
    Format.fprintf fmt "input (%a)@," (commasep ppIdent) x.inputs;
    cutsep ppOperation fmt x.operations;
    Format.fprintf fmt "output (%a)@," (commasep ppIdent) x.outputs
  )

let ppGlobal (fmt : PP.formatter) (x : global) : unit =
  ( match x with
  | Variable (v, ty, loc) ->
      Format.fprintf fmt "global @%a : %a // %a@,"
        Ident.pp v
        FMT.ty ty
        Loc.pp loc
  | Function (f, r, loc) ->
      Format.fprintf fmt "function @%a { // %a@,"
        Ident.pp f
        Loc.pp loc;
      ppRegion fmt r;
      Format.fprintf fmt "}@,"
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

