(****************************************************************
 * ISA interactive frontend
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** ISA interactive frontend *)

open LibISA
open Isa_ast
module Parser = Asl_parser
module TC = Tcheck
module AST = Isa_ast
module FMT = Isa_fmt

open Isa_utils

let opt_filenames : string list ref = ref []
let opt_print_version = ref false
let opt_print_spec = ref false
let opt_print_cflags = ref false
let opt_print_ldflags = ref false
let opt_print_includedir = ref false
let opt_print_runtimedir = ref false
let opt_print_stdlibdir = ref false
let opt_verbosity = ref 0
let opt_enable_legacy_prelude = ref false
let opt_batchmode = ref false
let opt_show_banner = ref true

let increase_verbosity () = opt_verbosity := !opt_verbosity + 1
let verbose (level : int) : bool = !opt_verbosity >= level

let history_file = "isa_history"

let cleanup_and_exit (code : int) : 'a =
  if not !opt_batchmode then begin
    LNoise.history_save ~filename:history_file |> ignore
  end;
  exit code

(* on error, optionally exit if in batchmode *)
let error () : unit =
  if !opt_batchmode then cleanup_and_exit 1

let projects : string list ref = ref []

let add_project (prj : string): unit =
  projects := !projects @ [prj]

let execs : string list ref = ref []

let add_exec (cmd : string): unit =
  execs := !execs @ [cmd]

(* verbosity control for :show and iii.exe *)
let formats = [ "simple"; "typed"; "raw" ]

let set_format (fmt : string) : unit =
  let (show_params, resugar) =
    ( match fmt with
    | "simple" -> (false, true)
    | "raw" -> (true, false)
    | "typed"
    | _ -> (true, true)
    )
  in
  FMT.show_type_params := show_params;
  FMT.resugar_operators := resugar

(****************************************************************
 * Interactive command support
 ****************************************************************)

let help_msg =
  [
    {|:? :help                                 Show this help message|};
    {|:project <file>                          Execute commands in <file>|};
    {|:q :quit                                 Exit the interpreter|};
    {|:set config <ident> = <expr>             Set configuration variable|};
    {|:set +<flag>                             Set flag|};
    {|:set -<flag>                             Clear flag|};
    {|<expr>                                   Execute ISA expression|};
    {|<stmt> ;                                 Execute ISA statement|};
  ]

(****************************************************************
 * Read Eval Print Loop
 ****************************************************************)

let mkLoc (fname : string) (input : string) : Loc.t =
  let len = String.length input in
  let start : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let finish : Lexing.position =
    { pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = len }
  in
  Loc.Range (start, finish)

let rec process_command (tcenv : TC.Env.t) (cpu : Cpu.cpu) (fname : string) (input0 : string) : unit =
  let input = String.trim input0 in
  match String.split_on_char ' ' input |> List.filter (fun a -> a <> "") with
  | [ "" ] -> ()
  | ("//"::_) -> () (* comment *)
  | [ ":help" ] | [ ":?" ] ->
      List.iter print_endline help_msg;
      Commands.print_help ();
      print_endline "\nFlags:";
      Flags.FlagMap.iter
        (fun nm (v, desc) -> Printf.printf "  %s%-27s %s\n" (if !v then "+" else "-") nm desc)
        !Flags.flags
  | ":set" :: "config" :: rest ->
      let cmd = String.concat " " rest in
      let loc = mkLoc fname cmd in
      let (v, e, ty) = LoadISA.read_config tcenv loc cmd in
      let value = Eval.eval_expr loc cpu.env e in
      cpu.setConfig v value
 | [ ":set"; flag ] when String.starts_with flag ~prefix:"+" -> (
     match Flags.FlagMap.find_opt (Utils.string_drop 1 flag) !Flags.flags with
     | None -> Printf.printf "Unknown flag %s\n" flag
     | Some (f, _) -> f := true)
 | [ ":set"; flag ] when String.starts_with flag ~prefix:"-" -> (
     match Flags.FlagMap.find_opt (Utils.string_drop 1 flag) !Flags.flags with
     | None -> Printf.printf "Unknown flag %s\n" flag
     | Some (f, _) -> f := false)
  | [ ":project"; prj ] -> (
      let inchan = open_in prj in
      try
        while true do
          process_command tcenv cpu prj (input_line inchan)
        done
      with End_of_file -> close_in inchan)
  | [ ":q" ] | [ ":quit" ] -> cleanup_and_exit 0
  | (cmd :: args) when String.starts_with ~prefix:":" cmd ->
     let old_show_params = !FMT.show_type_params in
     let old_resugar = !FMT.resugar_operators in
     if not (Commands.execute_command cmd args tcenv cpu) then error();
     FMT.show_type_params := old_show_params;
     FMT.resugar_operators := old_resugar
  | [] ->
     ()
  | _ ->
      if ';' = String.get input (String.length input - 1) then
        let ss = LoadISA.read_stmt tcenv input in
        List.iter (Eval.eval_stmt cpu.env) ss
      else
        let loc = mkLoc fname input in
        let e = LoadISA.read_expr tcenv loc input in
        let v = Eval.eval_expr loc cpu.env e in
        print_endline (Value.string_of_value v)

and load_project (tcenv : TC.Env.t) (cpu : Cpu.cpu) (prj : string) : unit =
  let inchan = open_in prj in
  try
    while true do
      process_command tcenv cpu prj (input_line inchan)
    done
  with End_of_file -> close_in inchan

let rec repl (tcenv : TC.Env.t) (cpu : Cpu.cpu) : unit =
  flush stdout;
  match LNoise.linenoise "ISA> " with
  | None -> ()
  | Some input ->
      LNoise.history_add input |> ignore;
      (try
        process_command tcenv cpu "<stdin>" input
      with
      | Value.EndExecution loc ->
        ()
      | e ->
        Error.print_exception e;
        error ();
      );
      repl tcenv cpu

(****************************************************************
 * Command: :filter_unlisted_functions
 ****************************************************************)

(* Delete a function definition if it occurs in the list of functions to be
 * deleted.
 *)
let delete_function (discard : Ident.t list) (x : AST.declaration) =
  ( match x with
  | AST.Decl_FunDefn (f, _, _, _) when List.mem f discard -> None
  | _ -> Some x
  )

let read_group_idents (group : string) : Ident.t list =
  let nms = Configuration.get_strings group in
  let xs = Ident.mk_fidents nms in
  let ys = Ident.mk_idents nms in
  xs @ ys

let read_foreign_idents (export : bool) (ds : AST.declaration list) :
    Ident.t list =
  List.filter_map (fun d ->
      match d with
      | Decl_FunFFI (nm, is_export, f, [], loc) when is_export = export -> Some f
      | Decl_FunFFI (nm, is_export, f, ps, loc) when is_export = export ->
          let name = Ident.name_with_tag f in
          raise (Utils.InternalError
            (loc, Format.asprintf "No specialized function '%s' created" name,
             (fun fmt -> FMT.declaration fmt d), __LOC__ ))
      | _ -> None
    ) ds

let read_foreign_import_idents (ds : AST.declaration list) : Ident.t list =
  read_foreign_idents false ds

let _ =
  let group = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let group_idents = read_group_idents !group in
    let foreign_idents = read_foreign_import_idents !Commands.declarations in
    let discarded = group_idents @ foreign_idents in
    Commands.declarations := List.filter_map (delete_function discarded) !Commands.declarations;
    true
  in
  let args = [
    (group, "config group");
  ]
  in
  Commands.registerCommand "filter_unlisted_functions" [] args [] "Discard listed functions" cmd

(****************************************************************
 * Command: :filter_listed_variables
 ****************************************************************)

(* Delete a variable declaration if it occurs in the list
 * of variables to be deleted.
 *)
let delete_variables_opt (discard : Ident.t list) (x : AST.declaration) :
    AST.declaration option =
  match x with
  | Decl_Var (v, ty, loc) when List.mem v discard -> None
  | _ -> Some x

let _ =
  let group = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let discard = read_group_idents !group in
    Commands.declarations := List.filter_map (delete_variables_opt discard) !Commands.declarations;
    true
  in
  let args = [
    (group, "config group");
  ]
  in
  Commands.registerCommand "filter_listed_variables" [] args [] "Discard listed variables" cmd

(****************************************************************
 * Command: :filter_reachable_from
 ****************************************************************)

let read_foreign_export_idents (ds : AST.declaration list) : Ident.t list =
  read_foreign_idents true ds

let _ =
  let keep_builtins = ref true in
  let group = ref "" in
  let builtin_funname (d : AST.declaration) : Ident.t option =
      ( match d with
      | Decl_FunType (f, fty, _) when fty.is_builtin -> Some(f)
      | _ -> None
      )
  in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let group_idents = read_group_idents !group in
    let foreign_idents = read_foreign_export_idents !Commands.declarations in
    let roots = group_idents @ foreign_idents in
    let prims = if !keep_builtins then List.filter_map builtin_funname !Commands.declarations else [] in
    if Utils.is_empty roots then (
      Printf.printf "No foreign export declarations and group '%s' is empty in \
                     :filter_reachable_from %s\n" !group !group;
      false
    ) else (
      Commands.declarations := Isa_utils.reachable_decls (roots @ prims) !Commands.declarations;
      true
    )
  in
  let flags = Arg.align [
    ("--keep-builtins", Arg.Set keep_builtins, "Output file");
    ("--no-keep-builtins", Arg.Clear keep_builtins, "Output file")
  ]
  in
  let args = [
    (group, "config group");
  ]
  in
  Commands.registerCommand "filter_reachable_from" flags args [] "Discard unreachable definitions" cmd

(****************************************************************
 * Command: :run
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    ( try
        while true do
          cpu.step ()
        done
    with
    | e -> Error.print_exception e; error ()
    );
    true
  in
  Commands.registerCommand "run" [] [] [] "Execute instructions" cmd

(****************************************************************
 * Command: :show
 ****************************************************************)

let _ =
  (* 'glob' matching against a pattern *)
  let pattern_match (x : string) (pattern : string) : bool =
    let n = String.length pattern in
    if String.get pattern (n-1) = '*' then
      String.starts_with ~prefix:(String.sub pattern 0 (n-1)) x
    else
      String.equal pattern x
  in

  (* declarations whose name matches pattern *)
  let decl_match (d : AST.declaration) (pattern : string) : bool =
    ( match Isa_utils.decl_name d with
    | Some nm -> pattern_match (Ident.name nm) pattern
    | None -> false
    )
  in

  let output = ref "" in
  let pattern = ref "" in

  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let ds = match !pattern with
      | "" -> !Commands.declarations
      | pat -> List.filter (fun d -> decl_match d pat) !Commands.declarations
    in

    ( match !output with
    | "" ->
        if Utils.is_empty ds then
          Format.printf "No function selected: try ':show A*'@."
        else
          List.iter (Format.printf "%a@,@." (FMT.declaration ~short:false)) ds;
    | filename ->
        Utils.to_file filename (fun fmt ->
          List.iter (Format.fprintf fmt "%a@,@," (FMT.declaration ~short:false)) ds;
        )
    );
    true
  in
  let flags = Arg.align [
    ("--output", Arg.Set_string output, "Output file");
    ("--format", Arg.Symbol (formats, set_format), "Control print format");
  ]
  in
  let opt_args = [
    (pattern, "pattern");
  ]
  in
  Commands.registerCommand "show" flags [] opt_args "Show matching definitions" cmd

(****************************************************************
 * Command: :step
 ****************************************************************)

let _ =
  let steps = ref "1"
  in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    ( try
        let n = int_of_string !steps in
        for i = 1 to n do
          cpu.step ();
        done
      with
      | e -> Error.print_exception e; error ()
    );
    true
  in
  let opt_args = [
    (steps, "count");
  ]
  in
  Commands.registerCommand "step" [] [] opt_args "Execute <count> instructions" cmd

(****************************************************************
 * Main program and command line options
 ****************************************************************)

let options =
  Arg.align
    [
      ("--print_spec",        Arg.Set opt_print_spec,              "       Print ISA spec");
      ("-v",                  Arg.Unit increase_verbosity,         "       Increase output verbosity");
      ("--version",           Arg.Set opt_print_version,           "       Print version");
      ("--print-c-flags",     Arg.Set opt_print_cflags,            "       Print the C flags needed to use the ISA C runtime");
      ("--print-ld-flags",    Arg.Set opt_print_ldflags,           "       Print the flags needed to link against the ISA C runtime");
      ("--print-lib-dir",     Arg.Set opt_print_stdlibdir,         "       Print the installation directory for ISA standard library");
      ("--print-runtime-dir", Arg.Set opt_print_runtimedir,        "       Print the installation directory for ISA C runtime");
      ("--print-include-dir", Arg.Set opt_print_includedir,        "       Print the installation directory for ISA C runtime include headers");
      ("--nobanner",          Arg.Clear opt_show_banner,           "       Suppress banner");
      ("--batchmode",         Arg.Set opt_batchmode,               "       Fail on error");
      ("--legacy-prelude",    Arg.Set opt_enable_legacy_prelude,   "       Enable use of legacy ISA function names");
      ("--no-legacy-prelude", Arg.Clear opt_enable_legacy_prelude, "       Disable use of legacy ISA function names");
      ("--configuration",     Arg.String Configuration.read_configuration_file,
                                                                   "       Load JSON configuration file");
      ("--exec",              Arg.String add_exec,                 "       Execute command");
      ("--project",           Arg.String add_project,              "       Execute project file");
      ("--format",            Arg.Symbol (formats, set_format),    "       Control print format: \
                                                                           'simple' - close to normal syntax; \
                                                                           'typed' - with type parameters; \
                                                                           'raw' - with type parameters on function calls and desugared operators");
      ("--max-errors",        Arg.Set_int Tcheck.max_errors,       "       Maximum number of typechecker errors");
      ("--check-exception-markers",    Arg.Set   Global_checks.check_defn_markers, "       Check that function definitions have correct exception markers");
      ("--no-check-exception-markers", Arg.Clear Global_checks.check_defn_markers, "       Do not check that function definitions have correct exception markers");
      ("--check-call-markers",         Arg.Set   Global_checks.check_call_markers, "       Check that function calls have correct exception markers");
      ("--no-check-call-markers",      Arg.Clear Global_checks.check_call_markers, "       Do not check that function calls have correct exception markers");
      ("--check-constraints",          Arg.Set   Tcheck.enable_constraint_checks,  "       Check type constraints");
      ("--no-check-constraints",       Arg.Clear Tcheck.enable_constraint_checks,  "       Do not check type constraints");
      ("--runtime-checks",             Arg.Set   Tcheck.enable_runtime_checks,     "       Insert runtime checks");
      ("--no-runtime-checks",          Arg.Clear Tcheck.enable_runtime_checks,     "       Do not insert runtime checks");
    ]

let version = "ISA compiler 0.0.0"

let banner =
  [
    {| _____  _____           |};
    {||_   _|/ ____|  /\      Intel(R) ISA specification tool|};
    {|  | | | (___   /  \     Copyright (C) 2022-2026 Intel Corporation|};
    {|  | |  \___ \ / /\ \    Copyright Arm Limited (c) 2017-2019|};
    {| _| |_ ____) / ____ \   |};
    {||_____|_____/_/    \_\  |} ^ version;
  ]

let usage_msg = version ^ "\nusage: iii <options> <file1> ... <fileN>\n"

(* Use PATH to search for the executable based on the name in argv[0] *)
let get_executable_path _ : string =
  let exe_name = Sys.argv.(0) in
  let search_path = if Filename.is_relative exe_name
      then Sys.getenv "PATH" |> String.split_on_char ':'
      else []
  in
  let opt_path = List.find_map (fun dir ->
    (* Note: we do not use Unix.realpath because that removes symlinks *)
    let filename = Filename.concat dir exe_name in
    if Sys.file_exists filename
    then Some filename
    else None
    )
    (""::search_path)
  in
  ( match opt_path with
  | Some path -> path
  | None ->
      Printf.printf "Error: unable to locate executable on PATH\n";
      exit 1
  )


let _ =
  Arg.parse options (fun s -> opt_filenames := !opt_filenames @ [ s ]) usage_msg

let main () =
  if not !opt_batchmode then begin
    Ocolor_format.prettify_formatter Format.std_formatter;
    Ocolor_config.set_color_capability Ocolor_config.Color4
  end;
  Tcheck.verbose := verbose 2;

  let exe_dir = Filename.dirname (get_executable_path ()) in
  let stdlib_dir = Filename.concat exe_dir "../share/isa-tools/stdlib" in
  let runtime_dir = Filename.concat exe_dir "../lib/isa-tools/runtime" in
  let runtime_include_dir = Filename.concat exe_dir "../lib/isa-tools/runtime_include" in
  let default_path = stdlib_dir ^ ":." in
  let legacy_path = Option.value (Sys.getenv_opt "ASL_PATH") ~default:default_path in
  let paths = Option.value (Sys.getenv_opt "ISA_PATH") ~default:legacy_path
    |> String.split_on_char ':' in
  if !opt_print_version then Printf.printf "%s\n" version
  else if !opt_print_cflags then Printf.printf "-I%s\n" runtime_include_dir
  else if !opt_print_ldflags then Printf.printf "-L%s -lASL\n" runtime_dir
  else if !opt_print_includedir then print_endline runtime_include_dir
  else if !opt_print_runtimedir then print_endline runtime_dir
  else if !opt_print_stdlibdir then print_endline stdlib_dir
  else begin
    if !opt_show_banner && not !opt_batchmode then begin
      List.iter print_endline banner;
      print_endline "\nType :? for help"
    end;
    try (
      let stdlibdirs : string list = stdlib_dir :: paths in
      let t = LoadISA.read_file stdlibdirs "stdlib.isa" true (verbose 1) in
      let old_t = if !opt_enable_legacy_prelude then LoadISA.read_file stdlibdirs "old_prelude.asl" true (verbose 1) else [] in
      let ts = LoadISA.read_files paths !opt_filenames (verbose 1) in
      let ds = t @ old_t @ ts in
      if (verbose 1) then Printf.printf "Performing global checks\n%!";
      let ds = Global_checks.check_decls ds in
      if (verbose 1) then Printf.printf "Performed global checks\n%!";
      if !opt_print_spec then (
        FMT.declarations Format.std_formatter ds;
        Format.pp_print_flush Format.std_formatter ());

      if (verbose 1) then Printf.printf "Building evaluation environment\n";
      let env = Eval.build_evaluation_environment ds in
      if (verbose 1) then Printf.printf "Built evaluation environment\n";

      Commands.declarations := ds;
      let tcenv = TC.Env.mkEnv TC.env0 in
      let cpu = Cpu.mkCPU env in

      List.iter (load_project tcenv cpu) !projects;
      List.iter (process_command tcenv cpu "<argv>") !execs;

      if not !opt_batchmode then begin
        LNoise.history_load ~filename:history_file |> ignore;
        LNoise.history_set ~max_length:100 |> ignore;
        repl tcenv cpu
      end;
      cleanup_and_exit 0
    ) with
    | Value.EndExecution loc ->
      cleanup_and_exit 0
    | e -> begin
      Error.print_exception e;
      cleanup_and_exit 1
    end
  end

let _ = ignore (main ())

(****************************************************************
 * End
 ****************************************************************)
