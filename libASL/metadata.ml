(****************************************************************
 * Metadata generator
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Asl_utils
open Identset

let generate_callgraph (filename : string) (ds : Asl_ast.declaration list): unit =
  let cg = ref Bindings.empty in
  List.iter (fun d ->
      ( match decl_name d with
      | None -> ()
      | Some nm ->
          let callees = calls_of_decl d in
          let old = Bindings.find_opt nm !cg |> Option.value ~default:IdentSet.empty in
          cg := Bindings.add nm (IdentSet.union callees old) !cg
      ))
      ds;
  let t = Bindings.bindings !cg
        |> List.map (fun (caller, callees) ->
              let callee_names = IdentSet.elements callees |> List.map Ident.to_string in
              (Ident.to_string caller, `List (List.map (fun s -> `String(s)) callee_names)))
        |> (fun xs -> `Assoc xs)
  in
  let chan = open_out filename in
  Yojson.pretty_to_channel chan t

(****************************************************************
 * Command: :callgraph
 ****************************************************************)

let _ =
  let file = ref "" in
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Printf.printf "Generating callgraph metadata file %s.\n" !file;
    generate_callgraph !file !Commands.declarations;
    true
  in
  let args = [
    (file, "json file");
  ]
  in
  Commands.registerCommand "callgraph" [] args [] "Generate json file containing callgraph" cmd

(****************************************************************
 * End
 ****************************************************************)
