(****************************************************************
 * Command registry
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type command = Tcheck.Env.t -> Cpu.cpu -> bool
type flag = (Stdlib.Arg.key * Stdlib.Arg.spec * Stdlib.Arg.doc)
type arg = (string ref * Stdlib.Arg.doc)

module CommandMap = Map.Make(String)

let commands : (command * flag list * arg list * arg list * string) CommandMap.t ref = ref CommandMap.empty

let registerCommand (name : string) (flags : flag list) (args : arg list) (opt_args : arg list) (description : string) (cmd : command) : unit =
  commands := CommandMap.add name (cmd, flags, args, opt_args, description) !commands

let parse_args (cmd_name : string) (flags : flag list) (args : arg list) (opt_args : arg list) (arguments : string list) : unit =
  let args_expected : arg list ref = ref args in
  let opt_args_expected : arg list ref = ref opt_args in
  let anonymous_arg (s : string) : unit =
    ( match !args_expected with
    | ((argref, _) :: args') -> argref := s; args_expected := args'
    | [] ->
        ( match !opt_args_expected with
        | ((argref, _) :: args') -> argref := s; opt_args_expected := args'
        | [] -> raise (Stdlib.Arg.Bad ("Unexpected argument to :" ^ cmd_name ^ " '" ^ s ^ "'"))
        )
    )
  in
  let usage_msg = "Usage: :" ^ cmd_name ^ " <options> " ^ (String.concat " " (List.map snd args)) in
  let argv = Array.of_list ((":"^cmd_name) :: arguments) in
  Stdlib.Arg.parse_argv ~current:(ref 0) argv flags anonymous_arg usage_msg;
  if not (Utils.is_empty !args_expected) then begin
    raise (Stdlib.Arg.Bad ("Missing arguments to :" ^ cmd_name ^ " '" ^ (String.concat " " (List.map snd !args_expected)) ^ "'"))
  end

let execute_command (cmd : string) (arguments : string list) (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
  ( match CommandMap.find_opt (Utils.string_drop 1 cmd) !commands with
  | None -> Printf.printf "Unknown command %s\n" cmd; false
  | Some (exec, flags, args, opt_args, _) ->
    ( try
        parse_args cmd flags args opt_args arguments;
        exec tcenv cpu
      with
      | Stdlib.Arg.Help msg -> Printf.printf "%s\n" msg; true
      | Stdlib.Arg.Bad msg -> Printf.printf "%s\n" msg; false
    )
  )

let print_help _ : unit =
  CommandMap.iter
    (fun nm (_, flags, args, opt_args, desc) ->
       let arg_names = List.map (fun (_, nm) -> "<" ^ nm ^ ">") args in
       let opt_arg_names = List.map (fun (_, nm) -> "[<" ^ nm ^ ">]") opt_args in
       Printf.printf ":%-40s%s\n" (nm ^" "^ (String.concat " " (arg_names @ opt_arg_names))) desc;
       let pp_flags = String.split_on_char '\n' (Stdlib.Arg.usage_string flags "") in
       let pp_flags = List.tl pp_flags in (* drop leading blank line *)
       let pp_flags = Utils.drop_right 3 pp_flags in (* drop trailing help commands *)
       List.iter (Printf.printf "%s\n") pp_flags
    )
    !commands

let declarations : Asl_ast.declaration list ref = ref []

(****************************************************************
 * End
 ****************************************************************)
