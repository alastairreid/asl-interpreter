(****************************************************************
 * Identifier set and map support
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(****************************************************************)
(** {2 Bindings and IdentSet}                                   *)
(****************************************************************)

module Bindings = Map.Make (Ident)
(** {2 Bindings: maps indexed by identifiers} *)

module IdentTable = Hashtbl.Make(Ident)

(** add association list to bindings *)
let add_bindings (bs : 'a Bindings.t) (xs : (Ident.t * 'a) list) : 'a Bindings.t =
  List.fold_left (fun a (k, v) -> Bindings.add k v a) bs xs

(** create bindings from association list *)
let mk_bindings (xs : (Ident.t * 'a) list) : 'a Bindings.t =
  add_bindings Bindings.empty xs

(** format bindings *)
let pp_bindings (f : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (bs : 'a Bindings.t) : unit =
  Bindings.iter (fun k v -> Format.fprintf fmt "%a: %a\n" Ident.pp k f v) bs

(** convert a list to bindings *)
let list_to_bindings (merge : 'a -> 'a -> 'a) (null : 'a) (xs : (Ident.t * 'a) list) : 'a Bindings.t =
  let bs : 'a Bindings.t ref = ref Bindings.empty in
  List.iter (fun (k, v) ->
      bs :=
        Bindings.update k
          (fun oprev -> Some (merge v (Option.value oprev ~default:null)))
          !bs
    ) xs;
  !bs

(** keys of bindings - for some reason, this is not a standard function *)
let keys_of_bindings (bs : 'a Bindings.t) : Ident.t list =
  List.map fst (Bindings.bindings bs)

module IdentSet = Set.Make (Ident)
(** {2 Sets of identifiers} *)

(** merge a list of sets *)
let unionSets (idss : IdentSet.t list) : IdentSet.t =
  List.fold_left IdentSet.union IdentSet.empty idss

(** add v to set of identifiers mapped to k *)
let addToBindingSet (k : Ident.t) (v : Ident.t) (bs : IdentSet.t Bindings.t) : IdentSet.t Bindings.t =
  Bindings.update k
    (function
    | None -> Some (IdentSet.singleton v)
    | Some vs -> Some (IdentSet.add v vs)
    )
    bs

(** Add multiple entries to a set *)
let unionToBindingSet (f : Ident.t) (xs : IdentSet.t) (bs : IdentSet.t Bindings.t) : IdentSet.t Bindings.t =
  Bindings.update
    f
    (function
    | None -> Some xs
    | Some s -> Some (IdentSet.union s xs)
    )
    bs

let pp_identset (fmt : Format.formatter) (xs : IdentSet.t) : unit =
    Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt ", ") Ident.pp fmt (IdentSet.elements xs)

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
let to_sorted_list (s : IdentSet.t) : Ident.t list = IdentSet.elements s

(****************************************************************
 * End
 ****************************************************************)
