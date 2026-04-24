(****************************************************************
 * Metadata generator
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Isa_utils
open Identset

let generate_callgraph (include_variables : bool) (ds : Isa_ast.declaration list): Yojson.t =
  let cg = ref Bindings.empty in
  let fvs = ref Bindings.empty in
  List.iter (fun d ->
      ( match decl_name d with
      | None -> ()
      | Some nm ->
          let callees = calls_of_decl d in
          let old_cg = Bindings.find_opt nm !cg |> Option.value ~default:IdentSet.empty in
          cg := Bindings.add nm (IdentSet.union callees old_cg) !cg;

          let freevars = fv_decl d in
          let old_fvs = Bindings.find_opt nm !fvs |> Option.value ~default:IdentSet.empty in
          fvs := Bindings.add nm (IdentSet.union freevars old_fvs) !fvs;
      ))
      ds;
  let callgraph = Bindings.bindings !cg
        |> List.map (fun (caller, callees) ->
              let callee_names = IdentSet.elements callees |> List.map Ident.to_string in
              (Ident.to_string caller, `List (List.map (fun s -> `String(s)) callee_names)))
        |> (fun xs -> `Assoc xs)
  in
  let freevars = Bindings.bindings !fvs
        |> List.map (fun (decl, vars) ->
              let var_names = IdentSet.elements vars |> List.map Ident.to_string in
              (Ident.to_string decl, `List (List.map (fun s -> `String(s)) var_names)))
        |> (fun xs -> `Assoc xs)
  in
  if include_variables then
      (`Assoc
        [("callgraph", callgraph);
         ("variables", freevars)
        ])
  else
      callgraph

(****************************************************************
 * Command: :callgraph
 ****************************************************************)

let _ =
  let file = ref "" in
  let cmd (include_variables : bool) (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Printf.printf "Generating callgraph metadata file %s.\n" !file;
    let json = generate_callgraph include_variables !Commands.declarations in
    let chan = open_out !file in
    Yojson.pretty_to_channel chan json;
    true
  in
  let args = [
    (file, "json file");
  ]
  in
  Commands.registerCommand "callgraph" [] args [] "Generate json file containing callgraph" (cmd false);
  Commands.registerCommand "dependencies" [] args [] "Generate json file containing code dependencies" (cmd true)

(****************************************************************
 * End
 ****************************************************************)
