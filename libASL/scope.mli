(****************************************************************
 * A Scope is a collection of identifiers where each identifier binds to some
 * value.
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type 'a t

val empty : unit -> 'a t
(** [empty ()] returns an empty scope *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal cmp s1 s2] tests whether the scopes [s1] and [s2] are equal, that
    is, contain equal keys and associate them with equal data. [cmp] is the
    equality predicate used to compare the data associated with the keys. *)

val clone : 'a t -> 'a t
(** [clone s] creates copy of scope [s] that can be independently mutated. *)

val mem : 'a t -> Ident.t -> bool
(** [mem s key] returns [true] if [s] contains a binding for [key], and [false]
    otherwise. *)

val get : 'a t -> Ident.t -> 'a option
(** [get s key] returns the binding [Some v] for [key] if binding exist, and
    [None] otherwise. *)

val set : 'a t -> Ident.t -> 'a -> unit
(** [set s key v] adds a binding for [key] to [v]. Note that this function does
    not return a new scope. The scope [s] is mutated. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] returns a new scope with [f] applied to each element in [s]. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [map f s] returns a new scope with each element in [s] transformed using
    [f]. *)

val map_inplace : ('a -> 'a) -> 'a t -> unit
(** [map_inplace f s] works the same way as [map] but mutates [s] instead of
    returning a new scope. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f s1 s2] returns a new scope by merging elements from [s1] and [s2]
    using [f]. *)

val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
(** [merge_inplace f s1 s2] works the same way as [map2] but stores the result
    into [s1]. *)

val bindings : 'a t -> (Ident.t * 'a) list
(** [bindings s] returns the bindings of [s] in a list. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(****************************************************************
 * End
 ****************************************************************)
