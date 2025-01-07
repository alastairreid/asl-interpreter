(****************************************************************
 * Configuration access
 *
 * This is used to manage configuration files
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Yojson

(****************************************************************
 * JSON file reading support
 ****************************************************************)

(* Attempt to get a Json string *)
let get_string (tree : Safe.t) : string option =
  ( match tree with
  | `String s -> Some s
  | _ -> None
  )

(* Attempt to get a Json list *)
let get_list (tree : Safe.t) : Safe.t list option =
  ( match tree with
  | `List s -> Some s
  | _ -> None
  )

(* Attempt to get a Json association list entry by key *)
let get_entry (key : string) (tree : Safe.t) : Safe.t option =
  ( match tree with
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None
  )

(* Read list of strings from Json files by key *)
let get_list_by_key (key : string) (files : Safe.t list) : string list =
  List.concat_map
    (fun json ->
      Option.bind (get_entry key json) (fun e ->
          Option.bind (get_list e) (fun es ->
              Some (List.filter_map get_string es)))
      |> Option.value ~default:[])
    files

let configurations : Yojson.Safe.t list ref = ref []

(** Read a JSON configuration file *)
let read_configuration_file (filename : string) : unit =
  let ops = Yojson.Safe.from_file filename in
  configurations := !configurations @ [ops]

(** Read list of strings from all previously read configuration files *)
let get_strings (key : string) : string list =
  get_list_by_key key !configurations

(****************************************************************
 * End
 ****************************************************************)
