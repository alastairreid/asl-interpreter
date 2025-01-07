(****************************************************************
 * A Scope is a collection of identifiers where each identifier binds to some
 * value.
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Identset

type 'a t = { mutable bs : 'a Bindings.t }

let pp (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (s : 'a t) : unit =
  Format.fprintf fmt "{\n";
  Format_utils.vbox fmt (fun _ -> pp_bindings f fmt s.bs);
  Format.fprintf fmt "}\n"

let empty () : 'a t = { bs = Bindings.empty }

let equal (eq : 'a -> 'a -> bool) (s1 : 'a t) (s2 : 'a t) : bool =
  Bindings.equal eq s1.bs s2.bs

let clone (s : 'a t) : 'a t = { bs = s.bs }

let mem (s : 'a t) (k : Ident.t) : bool = Bindings.mem k s.bs

let get (s : 'a t) (k : Ident.t) : 'a option = Bindings.find_opt k s.bs

let set (s : 'a t) (k : Ident.t) (v : 'a) : unit =
  s.bs <- Bindings.add k v s.bs

let map (f : 'a -> 'b) (s : 'a t) : 'b t = { bs = Bindings.map f s.bs }

let filter_map (f : 'a -> 'b option) (s : 'a t) : 'b t =
  { bs = Bindings.filter_map (fun k a -> f a) s.bs }

let map_inplace (f : 'a -> 'a) (s : 'a t) : unit = s.bs <- Bindings.map f s.bs

let map2 (f : 'a -> 'b -> 'c) (s1 : 'a t) (s2 : 'b t) : 'c t =
  let merge v oa ob =
    match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
  in
  { bs = Bindings.merge merge s1.bs s2.bs }

let merge_inplace (f : 'a -> 'b -> 'a) (s1 : 'a t) (s2 : 'b t) : unit =
  let merge v oa ob =
    match (oa, ob) with Some a, Some b -> Some (f a b) | _ -> None
  in
  s1.bs <- Bindings.merge merge s1.bs s2.bs

let bindings (s : 'a t) : (Ident.t * 'a) list = Bindings.bindings s.bs

(****************************************************************
 * End
 ****************************************************************)
