(****************************************************************
 * ScopeStack
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type 'a t = 'a Scope.t list

let pp (pp_value : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (ss : 'a t) : unit =
  List.iter (Scope.pp pp_value fmt) ss

let empty () : 'a t =
  let base : 'a Scope.t = Scope.empty () in
  [ base ]

let clone (ss : 'a t) : 'a Scope.t list = List.map Scope.clone ss

let add (ss : 'a t) (x : Ident.t) (v : 'a) : unit =
  match ss with
  | s :: _ -> Scope.set s x v
  | [] -> failwith "ScopeStack.add: broken invariant"

let rec get (ss : 'a t) (x : Ident.t) : 'a option =
  match ss with
  | s :: ss' -> (
      match Scope.get s x with Some v -> Some v | None -> get ss' x)
  | [] -> None

let mem (ss : 'a t) (x : Ident.t) : bool = Option.is_some (get ss x)

let rec set (ss : 'a t) (x : Ident.t) (v : 'a) : bool =
  match ss with
  | s :: ss' ->
      if Scope.mem s x then (
        Scope.set s x v;
        true)
      else
        set ss' x v
  | [] -> false

let map (f : 'a -> 'b) (ss : 'a t) : 'b t = List.map (Scope.map f) ss

let filter_map (f : 'a -> 'b option) (ss : 'a t) : 'b t =
  List.map (Scope.filter_map f) ss

let map_inplace (f : 'a -> 'a) (ss : 'a t) : unit =
  List.iter (Scope.map_inplace f) ss

let map2 (f : 'a -> 'b -> 'c) (ss1 : 'a t) (ss2 : 'b t) : 'c t =
  List.map2 (Scope.map2 f) ss1 ss2

let merge_inplace (f : 'a -> 'b -> 'a) (ss1 : 'a t) (ss2 : 'b t) : unit =
  List.iter2 (Scope.merge_inplace f) ss1 ss2

let add_local_scope (ss : 'a t) : 'a t =
  Scope.empty () :: ss

let nest (ss : 'a t) (k : 'a t -> 'b) : 'b =
  let newscope = Scope.empty () in
  k (newscope :: ss)

let bindings (ss : 'a t) : (Ident.t * 'a) list list =
  List.map Scope.bindings ss

let equal (eq : 'a -> 'a -> bool) (ss1 : 'a t) (ss2 : 'a t) : bool =
  List.equal (Scope.equal eq) ss1 ss2

(****************************************************************
 * End
 ****************************************************************)
