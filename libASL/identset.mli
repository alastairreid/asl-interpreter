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

module Bindings : Map.S with type key = Ident.t
(** {2 Bindings: maps indexed by identifiers} *)

(** add association list to bindings *)
val add_bindings : 'a Bindings.t -> (Ident.t * 'a) list -> 'a Bindings.t

(** create bindings from association list *)
val mk_bindings : (Ident.t * 'a) list -> 'a Bindings.t

(** print bindings *)
val pp_bindings : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Bindings.t -> unit

(** convert an association list to bindings using merge function to combine duplicates *)
val list_to_bindings : ('a -> 'a -> 'a) -> 'a -> (Ident.t * 'a) list -> 'a Bindings.t

(** keys of bindings - for some reason, this is not a standard function *)
val keys_of_bindings : 'a Bindings.t -> Ident.t list

module IdentTable : Hashtbl.S with type key = Ident.t

(****************************************************************)
(** {2 Scopes}                                                  *)
(****************************************************************)

module IdentSet : Set.S with type elt = Ident.t

(** {2 Sets of identifiers} *)

(** merge a list of sets *)
val unionSets : IdentSet.t list -> IdentSet.t

(** add v to set of identifiers mapped to k *)
val addToBindingSet : Ident.t -> Ident.t -> IdentSet.t Bindings.t -> IdentSet.t Bindings.t

(** Add multiple entries to a set *)
val unionToBindingSet : Ident.t -> IdentSet.t -> IdentSet.t Bindings.t -> IdentSet.t Bindings.t

val pp_identset : Format.formatter -> IdentSet.t -> unit

(** convert identifier set to sorted list of identifiers

    The implementation is trivial and exists mostly to emphasize that the
    resulting list is sorted
 *)
val to_sorted_list : IdentSet.t -> Ident.t list


(****************************************************************
 * End
 ****************************************************************)

