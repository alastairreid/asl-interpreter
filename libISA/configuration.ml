(****************************************************************
 * Configuration access
 *
 * This is used to manage configuration files
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Yojson

(****************************************************************
 * JSON file reading support
 ****************************************************************)

(* Attempt to get a JSON string *)
let get_string (tree : Safe.t) : string option =
  ( match tree with
  | `String s -> Some s
  | _ -> None
  )

(* Attempt to get a JSON list *)
let get_list (tree : Safe.t) : Safe.t list option =
  ( match tree with
  | `List s -> Some s
  | _ -> None
  )

(* Attempt to get a JSON association list entry by key *)
let get_entry (key : string) (tree : Safe.t) : Safe.t option =
  ( match tree with
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None
  )

(* Attempt to get a list and convert all elements of the list *)
let get_element_list (get_element : Safe.t -> 'a option) (tree : Safe.t) : 'a list option =
  ( match tree with
  | `List es -> Utils.flatten_map_option get_element es
  | _ -> None
  )

(* Attempt to get a record and convert all the entries of the record *)
let get_record (get_entry : Safe.t -> 'a option) (tree : Safe.t) : (string * 'a) list option =
  ( match tree with
  | `Assoc kvs ->
      Utils.flatten_map_option
        (fun (key, entry) ->
            Option.bind (get_entry entry) (fun entry' ->
            Some (key, entry')))
        kvs
  | _ -> None
  )

(* Read list of strings from JSON files by key *)
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

(** Read list of records from all previously read configuration files *)
let get_records (key : string) : (string * (string * string list) list) list =
  let trees = List.filter_map (get_entry key) !configurations in
  let get tree = get_record (get_record (get_element_list get_string)) tree in
  let rs = Utils.flatten_map_option get trees |> Option.value ~default:[] in
  List.concat rs

(****************************************************************
 * End
 ****************************************************************)
