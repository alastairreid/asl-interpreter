(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Asl_ast
module Lexer = Lexer
module Parser = Asl_parser
module ParserMessages = Asl_parser_messages
module LexerUtils = MenhirLib.LexerUtil
module ErrorReports = MenhirLib.ErrorReports
module Interp = Parser.MenhirInterpreter
module TC = Tcheck
module FMT = Asl_fmt
module AST = Asl_ast
open Lexing
open Asl_utils

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

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env].

   (This function is based on an example in the Menhir documentation.)
*)

let env (checkpoint : 'a Interp.checkpoint) : 'a Interp.env =
  ( match checkpoint with
  | Interp.HandlingError env ->
      env
  | _ ->
      assert false
  )

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint.

   (This function is based on an example in the Menhir documentation.)
*)

let state (checkpoint : _ Interp.checkpoint) : int option =
  ( match Interp.top (env checkpoint) with
  | Some (Interp.Element (s, _, _, _)) ->
      Some (Interp.number s)
  | None ->
      None
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


(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero.

   (This function is based on an example in the Menhir documentation.)
*)

let get (text : string) (checkpoint : _ Interp.checkpoint) (i : int) : string =
  match Interp.get i (env checkpoint) with
  | Some (Interp.Element (_, _, pos1, pos2)) ->
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

let fail (text : string) (buffer : (Loc.pos * Loc.pos) ErrorReports.buffer) (checkpoint : 'a Interp.checkpoint) : 'a =
  (* Indicate where in the input file the error occurred. *)
  let location = LexerUtils.range (ErrorReports.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = Printf.sprintf "Syntax error %s.\n" (ErrorReports.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ( match state checkpoint with
                | Some msgid -> ParserMessages.message msgid
                | None -> ""
                )
  in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = ErrorReports.expand (get text checkpoint) message in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1


(* [parse_file paths filename verbose]
   searches for [filename] in the search path list [paths]
   and attempts to parse the file as a list of ASL declarations.
   *)
let parse_file (paths : string list) (filename : string) (verbose : bool) : AST.declaration list =
  let fname = find_file paths filename in
  if verbose then Printf.printf "Processing %s\n" fname;

  let (text, lexbuf) = LexerUtils.read fname in

  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = LexerUtils.init fname (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = Interp.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let (buffer, supplier) = ErrorReports.wrap_supplier supplier in
  let start_pos = { lexbuf.lex_curr_p with pos_fname = fname } in

  try
    let chkpt = Parser.Incremental.declarations_start start_pos in
    Interp.loop_handle Fun.id (fail text buffer) supplier chkpt
  with
  | Parser.Error ->
    let loc = Loc.Range (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    raise (Error.ParseError loc)

let read_file (paths : string list) (filename : string) (isPrelude : bool)
    (verbose : bool) : AST.declaration list =
  let t = parse_file paths filename verbose in

  if false then (
    FMT.comment_list := Lexer.get_comments ();
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
  let (CLI_Config (v, e)) = Parser.config_command_start Lexer.token lexbuf in
  let (e', ty) = TC.tc_expr tcenv loc e in
  (v, e', ty)

let read_expr (tcenv : TC.Env.t) (loc : Loc.t) (s : string) : AST.expr =
  let lexbuf = Lexing.from_string s in
  let e = Parser.expr_command_start Lexer.token lexbuf in
  let e', _ = TC.tc_expr tcenv loc e in
  e'

let read_stmt (tcenv : TC.Env.t) (s : string) : AST.stmt =
  let lexbuf = Lexing.from_string s in
  let s = Parser.stmt_command_start Lexer.token lexbuf in
  TC.tc_stmt tcenv s

let read_stmts (tcenv : TC.Env.t) (s : string) : AST.stmt list =
  let lexbuf = Lexing.from_string s in
  let s = Parser.stmts_command_start Lexer.token lexbuf in
  TC.tc_stmts tcenv Loc.Unknown s

(* This entrypoint is used for testing so it does not sort its inputs to make
 * the output easier to predict/control
 *)
let read_declarations_unsorted (tcenv : TC.GlobalEnv.t) (s : string) :
    AST.declaration list =
  let lexbuf = Lexing.from_string s in
  let s = Parser.declarations_start Lexer.token lexbuf in
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
      parse_file paths fname verbose
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
