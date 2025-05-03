(****************************************************************
 * Error
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module Parser = Asl_parser
module FMT_Utils = Format_utils
module ASL_FMT = Asl_fmt
open Asl_utils

exception Unimplemented of (Loc.t * string * (Format.formatter -> unit))

(* Exceptions thrown by typechecker *)

exception UnknownObject of (Loc.t * string * string)
exception DoesNotMatch of (Loc.t * string * string * string)
exception IsNotA of (Loc.t * string * string)
exception Ambiguous of (Loc.t * string * string)
exception TypeError of (Loc.t * string)

exception ParseError of Loc.t

let print_exception (e : exn) : unit =
  match e with
  | Asl_ast.Parse_error_locn (l, s) ->
      Printf.printf "  Syntax error '%s' at %s\n" s (Loc.to_string l);
  | Asl_ast.PrecedenceError (loc, op1, op2) ->
      Printf.printf
        "  Syntax error: operators %s and %s require parentheses to \
         disambiguate expression at location %s\n"
        (pp_binop op1) (pp_binop op2) (Loc.to_string loc);
  | Parser.Error ->
      Printf.printf "  Parser error\n";
  | ParseError loc ->
      Printf.printf "  Parser error\n%s\n" (Loc.to_string loc);
  | UnknownObject (loc, what, x) ->
    Printf.printf "  %s: Type error: Unknown %s %s\n" (Loc.to_string loc) what x
  | DoesNotMatch (loc, what, x, y) ->
    Printf.printf "  %s: Type error: %s %s does not match %s\n" (Loc.to_string loc) what x y
  | IsNotA (loc, what, x) ->
    Printf.printf "  %s: Type error: %s is not a %s\n" (Loc.to_string loc) x what
  | Ambiguous (loc, what, x) ->
    Printf.printf "  %s: Type error: %s %s is ambiguous\n" (Loc.to_string loc) what x
  | TypeError (loc, what) ->
    Printf.printf "  %s: Type error: %s\n" (Loc.to_string loc) what
  | Value.EvalError (loc, msg) ->
    Printf.printf "  %s: Evaluation error: %s\n" (Loc.to_string loc) msg
  | Value.Throw (loc, exc) ->
      let fmt = Format.std_formatter in
      Format.fprintf fmt "ASL error: uncaught exception '%a' taken at %a\n"
        Value.pp_value exc
        ASL_FMT.loc loc
  | Value.EndExecution loc ->
      Printf.printf "End execution at %s\n" (Loc.to_string loc)
  | Utils.InternalError (loc, s, pp, ml_loc) ->
      ASL_FMT.show_type_params := true;
      let fmt = Format.std_formatter in
      Format.fprintf fmt "@.%a: internal compiler error: %s" ASL_FMT.loc loc s;
      FMT_Utils.indented fmt (fun _ -> pp fmt);
      FMT_Utils.cut fmt;
      Format.fprintf fmt "Please submit a bug report. %s@," ml_loc
  | Unimplemented (loc, what, pp) ->
      let fmt = Format.std_formatter in
      Format.pp_print_newline fmt ();
      FMT_Utils.vbox fmt (fun _ ->
          ASL_FMT.loc fmt loc;
          Format.fprintf fmt ": Unimplemented %s:" what;
          FMT_Utils.indented fmt (fun _ -> pp fmt);
          FMT_Utils.cut fmt)
  | _ ->
    Printf.printf "  Error %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;

(****************************************************************
 * End
 ****************************************************************)
