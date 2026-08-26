(****************************************************************
 * Error
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module Parser = Asl_parser
module FMTUtils = Format_utils
module ISA_FMT = Isa_fmt
open Isa_utils

exception Unimplemented of (Loc.t * string * (Format.formatter -> unit))

(* Exceptions thrown by typechecker *)

exception UnknownObject of (Loc.t * string * string)
exception DoesNotMatch of (Loc.t * string * string * string)
exception IsNotA of (Loc.t * string * string)
exception Ambiguous of (Loc.t * string * string)
exception TypeError of (Loc.t * string)

exception ParseError of (Loc.t * string)

let fprint_exception (fmt : Format.formatter) (e : exn) : unit =
  ( match e with
  | Isa_ast.Parse_error_locn (l, s) ->
      Format.fprintf fmt "  Syntax error '%s' at %s\n" s (Loc.to_string l);
  | Isa_ast.PrecedenceError (loc, op1, op2) ->
      Format.fprintf fmt
        "  Syntax error: operators %s and %s require parentheses to \
         disambiguate expression at location %s\n"
        (pp_binop op1) (pp_binop op2) (Loc.to_string loc);
  | Parser.Error ->
      Format.fprintf fmt "  Parser error\n";
  | ParseError (loc, msg) ->
      Format.fprintf fmt "  %s: Parser error %s\n" (Loc.to_string loc) msg;
  | UnknownObject (loc, what, x) ->
    Format.fprintf fmt "  %s: Type error: Unknown %s %s\n" (Loc.to_string loc) what x
  | DoesNotMatch (loc, what, x, y) ->
    Format.fprintf fmt "  %s: Type error: %s %s does not match %s\n" (Loc.to_string loc) what x y
  | IsNotA (loc, what, x) ->
    Format.fprintf fmt "  %s: Type error: %s is not a %s\n" (Loc.to_string loc) x what
  | Ambiguous (loc, what, x) ->
    Format.fprintf fmt "  %s: Type error: %s %s is ambiguous\n" (Loc.to_string loc) what x
  | TypeError (loc, what) ->
    Format.fprintf fmt "  %s: Type error: %s\n" (Loc.to_string loc) what
  | Value.EvalError (loc, msg) ->
    Format.fprintf fmt "  %s: Evaluation error: %s\n" (Loc.to_string loc) msg
  | Value.Throw (loc, exc) ->
      Format.fprintf fmt "ISA error: uncaught exception '%a' taken at %a\n"
        Value.pp_value exc
        ISA_FMT.loc loc
  | Value.EndExecution loc ->
      Format.fprintf fmt "End execution at %s\n" (Loc.to_string loc)
  | Utils.InternalError (loc, s, pp, ml_loc) ->
      ISA_FMT.show_type_params := true;
      Format.fprintf fmt "@.%a: internal compiler error: %s" ISA_FMT.loc loc s;
      FMTUtils.indented fmt (fun _ -> pp fmt);
      FMTUtils.cut fmt;
      Format.fprintf fmt "Please submit a bug report. %s@," ml_loc
  | Unimplemented (loc, what, pp) ->
      Format.pp_print_newline fmt ();
      FMTUtils.vbox fmt (fun _ ->
          ISA_FMT.loc fmt loc;
          Format.fprintf fmt ": Unimplemented %s:" what;
          FMTUtils.indented fmt (fun _ -> pp fmt);
          FMTUtils.cut fmt)
  | _ ->
    Format.fprintf fmt "  Error %s\n" (Printexc.to_string e);
    (* todo: change the next line to use fmt *)
    Printexc.print_backtrace stdout;
  )

let print_exception (e : exn) : unit =
  fprint_exception Format.std_formatter e

(****************************************************************
 * End
 ****************************************************************)
