(****************************************************************
 * Identifier support
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module IdentInfo = struct
  type t = {
    name : string;
    opt_tag : int option;
  }

  let compare (x : t) (y : t) : int =
      match (x, y) with
      | ({ name = n1; opt_tag = None }, { name = n2; opt_tag = None }) ->
          String.compare n1 n2
      | ({ name = n1; opt_tag = Some i }, { name = n2; opt_tag = Some j }) -> (
          let cx = String.compare n1 n2 in
          if cx <> 0 then cx else Int.compare i j
         )
      | ({ opt_tag = None; _ }, { opt_tag = Some _; _ }) -> -1
      | ({ opt_tag = Some _; _ }, { opt_tag = None; _ }) -> 1
end

module IdentMap = Map.Make (IdentInfo)

type t = {
  (* global unique id. Used for comparison *)
  id : int;
  info : IdentInfo.t;
  root : t option;
}

let hash (t : t) = t.id

let compare (x : t) (y : t) : int =
  Int.compare x.id y.id

let matches (i : t) ~(name : string) : bool =
  String.equal name i.info.name

let equal (a : t) (b : t) : bool =
  compare a b = 0

let root_equal (a : t) ~(root : t) : bool =
  match a.root with
  | None -> false
  | Some root' -> equal root' root

let in_list (x : t) (xs : t list) : bool =
  List.exists (equal x) xs

let to_string (i : t) : string =
  match i.info.opt_tag with
  | None -> i.info.name
  | Some tag -> i.info.name ^ "." ^ string_of_int tag

let pp (fmt : Format.formatter) (x : t) : unit =
  Format.pp_print_string fmt (to_string x)

let name_with_tag (i : t) : string =
  match i.info.opt_tag with
  | None -> i.info.name
  | Some tag -> i.info.name ^ "_" ^ string_of_int tag

let name (i : t) : string =
  i.info.name

let is_function (i : t) : bool =
  Option.is_some i.info.opt_tag

(* The map will store the binding between IdentInfo and Ident.
 * Once an ident is returned it can be compared against other
 * idents fast by using the compare function of the Ident.
 * I.e. creating / retrieving an ident is expensive, but comparing
 * already retrieved idents are cheap. *)
let id  = ref 0
let map = ref IdentMap.empty

let add_ident' (info : IdentInfo.t) (root : t option) : t =
  match IdentMap.find_opt info !map with
  | None ->
    let i = { id = !id; info; root } in
    map := IdentMap.add info i !map;
    id := !id + 1;
    i
  | Some i -> i

let mk_ident (name : string) : t =
  let info : IdentInfo.t = { name; opt_tag = None } in
  add_ident' info None

let mk_idents (names : string list) : t list =
  List.map mk_ident names

let mk_fident (name : string) : t =
  let info : IdentInfo.t = { name; opt_tag = Some 0 } in
  add_ident' info None

let mk_fidents (names : string list) : t list =
  List.map mk_fident names

let mk_fident_with_tag (i : t) ~(tag : int) : t =
  if i.info.opt_tag = Some tag then i
  else add_ident' { i.info with opt_tag = Some tag } (Some i)

let add_suffix (t : t) ~(suffix : string) :  t =
  let name' = t.info.name ^ "_" ^ suffix in
  add_ident' { t.info with name = name' } None

let add_prefix (t : t) ~(prefix : string) : t =
  let name' = prefix ^ "_" ^ t.info.name in
  add_ident' { t.info with name = name' } None

(****************************************************************
 * End
 ****************************************************************)
