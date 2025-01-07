(****************************************************************
 * ScopeStack
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type 'a t

val empty : unit -> 'a t
(** [empty ()] returns an empty scope stack *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal cmp ss1 ss2] tests whether the scopes in the scope stacks [ss1] and
    [ss2] are equal, that is, contain equal keys and associate them with equal
    data. [cmp] is the equality predicate used to compare the data associated
    with the keys. *)

val clone : 'a t -> 'a t
(** [clone ss] creates copy of scope stack [ss] that can be independently
    mutated *)

val add : 'a t -> Ident.t -> 'a -> unit
(** [add ss key v] adds a binding for [key] to [v] in the current
    scope in [ss]. Note that this function does not return a new scope stack.
    The scope stack [ss] is mutated. *)

val mem : 'a t -> Ident.t -> bool
(** [mem ss key] returns [true] if a binding for [key] is found in a scope in
    [ss], starting from the current scope, otherwise [false] is returned. *)

val get : 'a t -> Ident.t -> 'a option
(** [get ss key] returns the binding for [key] in the first scope the binding
    is found, starting from the current scope. If a binding is found it is
    returned as [Some v], otherwise [None] is returned. *)

val set : 'a t -> Ident.t -> 'a -> bool
(** [set ss key v] tries to update a binding for [key] to [v] in the first
    scope that the binding is found, starting from the current scope. If the
    binding is not found then it is not updated. This function returns [true]
    if the binding is updated, and [false] if not. Note that [ss] is
    mutated. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f ss] returns a new scope stack with [f] applied to each element in
    [ss]. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f ss] returns a new scope stack with [f] applied to each
    element in [ss]. If [f] returns [None] the element is discarded in the new
    result. *)

val map_inplace : ('a -> 'a) -> 'a t -> unit
(** [map_inplace f ss] works the same way as [map] but mutates [ss] instead of
    returning a new scope stack. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f ss1 ss2] returns a new scope stack by merging elements in each
    scope from [ss1] and [ss2] using [f]. *)

val merge_inplace : ('a -> 'b -> 'a) -> 'a t -> 'b t -> unit
(** [merge_inplace f ss1 ss2] works the same way as [map2] but stores the
    result into [ss1]. *)

val nest : 'a t -> ('a t -> 'b) -> 'b
(** [nest ss f] adds a new empty current scope [s] to [ss] then calls f with
    [s :: ss]. *)

val bindings : 'a t -> (Ident.t * 'a) list list
(** [bindings ss] returns the bindings of all scopes in a list. **)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(****************************************************************
 * End
 ****************************************************************)
