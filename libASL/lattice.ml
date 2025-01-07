(****************************************************************
 * Lattices: abstract interpretation support
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module type Lattice = sig
  type elt
  type t

  val bottom : t
  val top : t
  val lub : t -> t -> t
  val glb : t -> t -> t
  val le : t -> t -> bool
  val equal : t -> t -> bool
  val singleton : elt -> t
  val to_concrete : t -> elt option
  val pp_concrete : Format.formatter -> elt -> unit
  val pp_abstract : Format.formatter -> t -> unit
end

module Const (Concrete : sig
  type t
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end) =
struct
  type elt = Concrete.t
  type t = Bottom | Top | Singleton of elt

  let bottom = Bottom
  let top = Top

  let le (x : t) (y : t) : bool =
    match (x, y) with
    | Bottom, _ -> true
    | Singleton x, Singleton y -> Concrete.equal x y
    | _, Top -> true
    | _ -> false

  let equal (x : t) (y : t) : bool =
    match (x, y) with
    | Bottom, Bottom -> true
    | Singleton x, Singleton y -> Concrete.equal x y
    | Top, Top -> true
    | _ -> false

  let singleton (x : elt) : t = Singleton x

  let to_concrete (x : t) : elt option =
    match x with Singleton y -> Some y | _ -> None

  let lub (x : t) (y : t) : t =
    match (x, y) with
    | Bottom, _ -> y
    | _, Bottom -> x
    | Singleton x', Singleton y' -> if Concrete.equal x' y' then x else Top
    | Top, _ -> x
    | _, Top -> y

  let glb (x : t) (y : t) : t =
    match (x, y) with
    | Bottom, _ -> x
    | _, Bottom -> y
    | Singleton x', Singleton y' -> if Concrete.equal x' y' then x else Bottom
    | Top, _ -> y
    | _, Top -> x

  let pp_concrete = Concrete.pp

  let pp_abstract (fmt : Format.formatter) (x : t) : unit =
    match x with
    | Bottom -> Format.pp_print_string fmt "bottom"
    | Top -> Format.pp_print_string fmt "top"
    | Singleton x -> pp_concrete fmt x
end

module Interval (Concrete : sig
  type t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
end) =
struct
  type elt = Concrete.t
  type t = All | Empty | Range of { lo : elt; hi : elt }

  (* interval size _decreases_ as we move up the domain *)
  let bottom = All
  let top = Empty

  let le (x : t) (y : t) : bool =
    match (x, y) with
    | All, _ -> true
    | Range { lo = xlo; hi = xhi }, Range { lo = ylo; hi = yhi } ->
        Concrete.compare ylo xlo <= 0 && Concrete.compare xhi yhi <= 0
    | _, Empty -> true
    | _ -> false

  let equal (x : t) (y : t) : bool =
    match (x, y) with
    | All, All -> true
    | Range { lo = xlo; hi = xhi }, Range { lo = ylo; hi = yhi } ->
        Concrete.compare ylo xlo = 0 && Concrete.compare xhi yhi = 0
    | Empty, Empty -> true
    | _ -> false

  let singleton (x : elt) : t = Range { lo = x; hi = x }

  let to_concrete (x : t) : elt option =
    match x with
    | Range { lo; hi } -> if Concrete.compare lo hi = 0 then Some lo else None
    | _ -> None

  let lub (x : t) (y : t) : t =
    match (x, y) with
    | All, _ -> y
    | _, All -> x
    | Range { lo = xlo; hi = xhi }, Range { lo = ylo; hi = yhi } ->
        let rlo = if Concrete.compare xlo ylo <= 0 then xlo else ylo in
        (* max *)
        let rhi = if Concrete.compare xhi yhi <= 0 then yhi else xhi in
        (* min *)
        if Concrete.compare rlo rhi <= 0 then Range { lo = rlo; hi = rhi }
        else Empty
    | Empty, _ -> x
    | _, Empty -> y

  let glb (x : t) (y : t) : t =
    match (x, y) with
    | All, _ -> x
    | _, All -> y
    | Range { lo = xlo; hi = xhi }, Range { lo = ylo; hi = yhi } ->
        let rlo = if Concrete.compare xlo ylo <= 0 then xlo else ylo in
        (* min *)
        let rhi = if Concrete.compare xhi yhi <= 0 then yhi else xhi in
        (* max *)
        Range { lo = rlo; hi = rhi }
    | Empty, _ -> y
    | _, Empty -> x

  let pp_concrete = Concrete.pp

  let pp_abstract (fmt : Format.formatter) (x : t) : unit =
    match x with
    | All -> Format.pp_print_string fmt "all"
    | Empty -> Format.pp_print_string fmt "empty"
    | Range { lo; hi } -> Format.fprintf fmt "%a..%a"
                            pp_concrete lo
                            pp_concrete hi
end

(*****************************************
 * End
 *****************************************)
