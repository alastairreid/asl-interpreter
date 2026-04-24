(****************************************************************
 * Functions for processing ISA files
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Isa_ast
module ASL_Lexer = Asl_lexer
module ASL_Parser = Asl_parser
module ASL_ParserMessages = Asl_parser_messages
module ASL_Interp = ASL_Parser.MenhirInterpreter
module ISA_Lexer  = Isa_lexer
module ISA_Parser = Isa_parser
module ISA_Interp = ISA_Parser.MenhirInterpreter
module ISA_ParserMessages = Isa_parser_messages
module LexerUtils = MenhirLib.LexerUtil
module ErrorReports = MenhirLib.ErrorReports
module TC = Tcheck
module FMT = Isa_fmt
module AST = Isa_ast
open Lexing
open Isa_utils

let find_file (search_path : string list) (filename : string) : string =
  if Filename.is_relative filename then (
    let rec find (paths : string list) : string =
      ( match paths with
      | [] -> failwith ("Can't find file '" ^ filename ^ "' on path '" ^ String.concat ":" search_path)
      | path :: paths' ->
          let fname = Filename.concat path filename in
          if Sys.file_exists fname then fname else find paths'
      )
    in find search_path
  ) else (
    if Sys.file_exists filename then
      filename
    else
      failwith ("Can't find file '" ^ filename ^ "'")
  )

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2].

   (This function is based on an example in the Menhir documentation.)
*)

let show (text : string) (positions : Loc.pos * Loc.pos) : string =
  ErrorReports.extract text positions
  |> ErrorReports.sanitize
  |> ErrorReports.compress
  |> ErrorReports.shorten 20 (* max width 43 *)


(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env].

   (This function is based on an example in the Menhir documentation.)
*)

let asl_env (checkpoint : 'a ASL_Interp.checkpoint) : 'a ASL_Interp.env =
  ( match checkpoint with
  | ASL_Interp.HandlingError env ->
      env
  | _ ->
      assert false
  )

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint.

   (This function is based on an example in the Menhir documentation.)
*)

let asl_state (checkpoint : _ ASL_Interp.checkpoint) : int option =
  ( match ASL_Interp.top (asl_env checkpoint) with
  | Some (ASL_Interp.Element (s, _, _, _)) ->
      Some (ASL_Interp.number s)
  | None ->
      None
  )

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero.

   (This function is based on an example in the Menhir documentation.)
*)

let asl_get (text : string) (checkpoint : _ ASL_Interp.checkpoint) (i : int) : string =
  match ASL_Interp.get i (asl_env checkpoint) with
  | Some (ASL_Interp.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error.

   (This function is based on an example in the Menhir documentation.)
*)

let asl_fail (text : string) (buffer : (Loc.pos * Loc.pos) ErrorReports.buffer) (checkpoint : 'a ASL_Interp.checkpoint) : 'a =
  (* Indicate where in the input file the error occurred. *)
  let location = LexerUtils.range (ErrorReports.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = Printf.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message =
      (try
          let message = ( match asl_state checkpoint with
                        | Some msgid -> ASL_ParserMessages.message msgid
                        | None -> ""
                        )
          in
          (* Expand away the $i keywords that might appear in the message. *)
          ErrorReports.expand (asl_get text checkpoint) message
      with
      | Not_found -> "syntax error"
      )
  in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env].

   (This function is based on an example in the Menhir documentation.)
*)

let isa_env (checkpoint : 'a ISA_Interp.checkpoint) : 'a ISA_Interp.env =
  ( match checkpoint with
  | ISA_Interp.HandlingError env ->
      env
  | _ ->
      assert false
  )

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint.

   (This function is based on an example in the Menhir documentation.)
*)

let isa_state (checkpoint : _ ISA_Interp.checkpoint) : int option =
  ( match ISA_Interp.top (isa_env checkpoint) with
  | Some (ISA_Interp.Element (s, _, _, _)) ->
      Some (ISA_Interp.number s)
  | None ->
      None
  )

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero.

   (This function is based on an example in the Menhir documentation.)
*)

let isa_get (text : string) (checkpoint : _ ISA_Interp.checkpoint) (i : int) : string =
  match ISA_Interp.get i (isa_env checkpoint) with
  | Some (ISA_Interp.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error.

   (This function is based on an example in the Menhir documentation.)
*)

let isa_fail (text : string) (buffer : (Loc.pos * Loc.pos) ErrorReports.buffer) (checkpoint : 'a ISA_Interp.checkpoint) : 'a =
  (* Indicate where in the input file the error occurred. *)
  let location = LexerUtils.range (ErrorReports.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = Printf.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message =
      (try
          let message = ( match isa_state checkpoint with
                        | Some msgid -> ISA_ParserMessages.message msgid
                        | None -> ""
                        )
          in
          (* Expand away the $i keywords that might appear in the message. *)
          ErrorReports.expand (isa_get text checkpoint) message
      with
      | Not_found -> "syntax error"
      )
  in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1


(* [parse_asl_file paths filename verbose]
   searches for [filename] in the search path list [paths]
   and attempts to parse the file as a list of ASL declarations.
   *)
let parse_asl_file (paths : string list) (filename : string) (verbose : bool) : AST.declaration list =
  let fname = find_file paths filename in
  if verbose then Printf.printf "Processing %s\n" fname;

  let (text, lexbuf) = LexerUtils.read fname in

  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = LexerUtils.init fname (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = ASL_Interp.lexer_lexbuf_to_supplier ASL_Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let (buffer, supplier) = ErrorReports.wrap_supplier supplier in
  let start_pos = { lexbuf.lex_curr_p with pos_fname = fname } in

  try
    let chkpt = ASL_Parser.Incremental.declarations_start start_pos in
    ASL_Interp.loop_handle Fun.id (asl_fail text buffer) supplier chkpt
  with
  | ASL_Parser.Error ->
    let loc = Loc.Range (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    raise (Error.ParseError loc)

(* [parse_isa_file paths filename verbose]
   searches for [filename] in the search path list [paths]
   and attempts to parse the file as a list of ISA declarations.
   *)
let parse_isa_file (paths : string list) (filename : string) (verbose : bool) : AST.declaration list =
  let fname = find_file paths filename in
  if verbose then Printf.printf "Processing %s\n" fname;

  let (text, lexbuf) = LexerUtils.read fname in

  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = LexerUtils.init fname (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = ISA_Interp.lexer_lexbuf_to_supplier ISA_Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let (buffer, supplier) = ErrorReports.wrap_supplier supplier in
  let start_pos = { lexbuf.lex_curr_p with pos_fname = fname } in

  try
    let chkpt = ISA_Parser.Incremental.declarations_file start_pos in
    ISA_Interp.loop_handle Fun.id (isa_fail text buffer) supplier chkpt
  with
  | ISA_Parser.Error ->
    let loc = Loc.Range (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    raise (Error.ParseError loc)

let parse_file (paths : string list) (filename : string) (verbose : bool) : AST.declaration list =
  if String.ends_with filename ~suffix:".asl" then
    parse_asl_file paths filename verbose
  else if String.ends_with filename ~suffix:".isa" then
    parse_isa_file paths filename verbose
  else begin
    Printf.eprintf "Unrecognized file suffix on file '%s'%!" filename;
    exit 1
  end

let read_file (paths : string list) (filename : string) (isPrelude : bool)
    (verbose : bool) : AST.declaration list =
  let t = parse_file paths filename verbose in

  if false then (
    FMT.declarations Format.std_formatter t;
    Format.pp_print_flush Format.std_formatter ()
  );
  if verbose then (
    Printf.printf "  - Got %d declarations from %s\n%!" (List.length t) filename;
    Printf.printf "- Typechecking %s\n%!" filename;
  );

  let sort_decls = not isPrelude in (* sort everything but the Prelude *)
  let t' =
    ( match TC.tc_declarations TC.env0 ~isPrelude ~sort_decls t with 
    | None -> exit 1
    | Some t' -> t'
    )
  in

  if false then FMT.declarations Format.std_formatter t';
  if verbose then (
    Printf.printf "  - Got %d typechecked declarations from %s\n%!"
      (List.length t') filename;
    Printf.printf "Finished %s\n%!" filename;
  );
  flush stdout;
  t'

let parse_spec (paths : string list) (filename : string) (verbose : bool) :
    AST.declaration list =
  let r : AST.declaration list list ref = ref [] in
  let fname = find_file paths filename in
  let inchan = open_in fname in
  (try
     while true do
       let t = parse_file paths (input_line inchan) verbose in
       r := t :: !r
     done
   with End_of_file -> close_in inchan);
  List.concat (List.rev !r)

let read_config (tcenv : TC.Env.t) (loc : Loc.t) (s : string) : Ident.t * AST.expr * AST.ty =
  let lexbuf = Lexing.from_string s in
  let (CLI_Config (v, e)) = ASL_Parser.config_command_start ASL_Lexer.token lexbuf in
  let (e', ty) = TC.tc_expr tcenv loc e in
  (v, e', ty)

let read_expr (tcenv : TC.Env.t) (loc : Loc.t) (s : string) : AST.expr =
  let lexbuf = Lexing.from_string s in
  let e = ISA_Parser.expr_command_start ISA_Lexer.token lexbuf in
  let (e', _) = TC.tc_expr tcenv loc e in
  e'

let read_stmt (tcenv : TC.Env.t) (s : string) : AST.stmt list =
  let lexbuf = Lexing.from_string s in
  let s = ISA_Parser.stmt_command_start ISA_Lexer.token lexbuf in
  TC.tc_stmt tcenv s

let read_stmts (tcenv : TC.Env.t) (s : string) : AST.stmt list =
  let lexbuf = Lexing.from_string s in
  let s = ISA_Parser.stmts_command_start ISA_Lexer.token lexbuf in
  TC.tc_stmts tcenv Loc.Unknown s

(* This entrypoint is used for testing so it does not sort its inputs to make
 * the output easier to predict/control
 *)
let read_declarations_unsorted (tcenv : TC.GlobalEnv.t) (s : string) :
    AST.declaration list =
  let lexbuf = Lexing.from_string s in
  let s = ASL_Parser.declarations_start ASL_Lexer.token lexbuf in
  ( match TC.tc_declarations tcenv ~isPrelude:false ~sort_decls:false s with
  | None -> exit 1
  | Some s' -> s'
  )

let read_files (paths : string list) (filenames : string list) (verbose : bool)
    : AST.declaration list =
  let parse fname =
    if String.ends_with fname ~suffix:".spec" then
      parse_spec paths fname verbose
    else if String.ends_with fname ~suffix:".asl" then
      parse_asl_file paths fname verbose
    else if String.ends_with fname ~suffix:".isa" then
      parse_isa_file paths fname verbose
    else
      failwith ("Unrecognized file suffix on " ^ fname)
  in

  let ds = List.map parse filenames |> List.concat in
  if verbose then (
    Printf.printf "- Got %d declarations\n%!" (List.length ds);
    Printf.printf "- Typechecking\n%!"
  );

  let ds' =
    ( match TC.tc_declarations TC.env0 ~isPrelude:false ~sort_decls:true ds with
    | None -> exit 1
    | Some ds' -> ds'
    )
  in

  if verbose then (
    Printf.printf "  - Got %d typechecked declarations\n" (List.length ds');
    Printf.printf "Finished typechecking specification\n"
  );
  flush stdout;

  ds'

(****************************************************************
 * End
 ****************************************************************)
