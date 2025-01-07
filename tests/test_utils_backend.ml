(****************************************************************
 * Utilities for backend tests
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open LibASL
module AST = Asl_ast
module TC = Tcheck

let check_declaration (tcenv : TC.GlobalEnv.t)
    (decls : AST.declaration list -> unit)
    (check_ext : string -> string -> unit) (name : string) (s : string) : unit =
  try
    let tcenv = TC.GlobalEnv.clone tcenv in
    let ds = LoadASL.read_declarations_unsorted tcenv s in
    Alcotest.(check pass) name () (decls ds);

    let s = Format.flush_str_formatter () in
    check_ext name s
  with e ->
    Error.print_exception e;
    Alcotest.fail "exception during test"

let check_compiler
    (language : string)
    (suffix : string)
    (prog : string)
    (args : string list)
    (name : string)
    (header : string)
    (body : string)
  : unit =
  let (tmp, chan) = Filename.open_temp_file "test" suffix in
  Out_channel.output_string chan header;
  Out_channel.output_string chan "\n";
  Out_channel.output_string chan body;
  Out_channel.close chan;
  let args' = args @ [ tmp ] in

  if true then begin
    (* output test to log - for ease of debugging *)
    output_string stdout header;
    output_string stdout "\n";
    output_string stdout body;

    Printf.printf "Wrote to %s\n" tmp;
    Printf.printf "Executing %s %s\n" prog (String.concat " " args');
    flush stdout
  end;

  let p = Unix.open_process_args_out prog (Array.of_list (prog :: args')) in
  let status = Unix.close_process_out p in
  let exit_status =
    match status with
    | Unix.WEXITED s ->
        if s <> 0 then Printf.printf "%s exited with %d\n" prog s;
        s
    | Unix.WSIGNALED _ ->
        Printf.printf "%s killed\n" prog;
        1
    | Unix.WSTOPPED _ ->
        Printf.printf "%s stopped\n" prog;
        1
  in
  Sys.remove tmp;
  Alcotest.(check int) (language ^ " syntax: " ^ name) 0 exit_status

(****************************************************************
 * End
 ****************************************************************)
