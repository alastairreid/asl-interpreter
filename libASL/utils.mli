(****************************************************************
 * Generic utility functions
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Exceptions *)

(* Internal invariants have been broken *)
exception InternalError of (Loc.t * string * (Format.formatter -> unit) * string)

(** Generic utility functions *)

(****************************************************************
 * Pretty-printer related
 ****************************************************************)

val to_string2 : (Format.formatter -> unit) -> string

val to_file : string -> (Format.formatter -> unit) -> unit

(****************************************************************
 * List related
 ****************************************************************)

val is_empty : 'a list -> bool

val nub : 'a list -> 'a list

(** init [x1; ... xn] = [x1; ... xn-1] *)
val init : 'a list -> 'a list

(** last [x1; ... xn] = xn *)
val last : 'a list -> 'a

(** drop_left m [x1; ... xn] = [xm+1; ... xn] *)
val drop_left : int -> 'a list -> 'a list

(** drop_right m [x1; ... xn] = [x1; ... xn-m] *)
val drop_right : int -> 'a list -> 'a list

(** split3 [(x1, y1, z1); ... (xn, yn, zn)] = ([x1; ... xn], [y1; ... yn], [z1; ... zn]) *)
val split3 : ('x * 'y * 'z) list -> ('x list * 'y list * 'z list)

(** iter3 f [x1;x2;...] [y1;y2;...] [z1;z2;...] = f x1 y1 z1; f x2 y2 z2; ... *)
val iter3 : ('a -> 'b -> 'c -> unit) -> ('a list -> 'b list -> 'c list -> unit)

(** iter_pairs f [x1;x2;x3;x4;...] = f x1 x2; f x1 x3; f x1 x4; ...; f x2 x3; f x2 x4 *)
val iter_pairs : ('a -> 'a -> unit) -> 'a list -> unit

(** apply 'f' to all combinations of x from xs and y from ys *)
val cross_combine : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** Cartesian product.
  Example:
  # let ls = [[1;2]; [3;4]];;
  val ls : int list list = [[1; 2]; [3; 4]]
  # cartesian_product ls;;
  - : int list list = [[1; 3]; [1; 4]; [2; 3]; [2; 4]]
*)
val cartesian_product : 'a list list -> 'a list list

(****************************************************************
 * Option related
 ****************************************************************)

val from_option : 'a option -> (unit -> 'a) -> 'a

val orelse_option : 'a option -> (unit -> 'a option) -> 'a option

val map2_option : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

(* Examples:
 * concat_option [ Some [1;2]; None; Some [3] ];;
 * None
 * concat_option [ Some [1;2]; Some [3] ];;
 * Some [1; 2; 3]
 *)
val concat_option : 'a list option list -> 'a list option

(* extract all non-None elements from a list *)
val flatten_option : 'a option list -> 'a list

(* Like Option.filter_map for binary operations
 *
 * `filter_map2 f xs ys` applies `f` to every element of `xs` and `ys`,
 * filters out the None elements and returns the list of the arguments
 * of the Some elements.
 * *)
val filter_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

(* If any element of 'map f xs' is None, return None
 * Otherwise, return 'map (Option.get . f) xs'
 *)
(* todo: give this a better name *)
val flatten_map_option : ('a -> 'b option) -> 'a list -> 'b list option

(* find first non-None result from function 'f' on list 'xs' *)
val first_option : ('a -> 'b option) -> 'a list -> 'b option

(****************************************************************
 * String related
 ****************************************************************)

(** Drop first n characters from string *)
val string_drop : int -> string -> string

(** Convert a char list to a string *)
val string_of_chars : char list -> string

(** Delete all characters matching 'c' from string 'x' *)
val drop_chars : string -> char -> string

(****************************************************************
 * Compare related
 ****************************************************************)

(** combine the result of two compare functions *)
val ( <?> ) : int -> (('a -> 'b -> int) * 'a * 'b) -> int

(****************************************************************
 * File related
 ****************************************************************)

(** load a file into a buffer *)
val read_file : string -> bytes

(****************************************************************
 * Int related
 ****************************************************************)

(** 2^i *)
val pow2 : int -> int

(****************************************************************
 * End
 ****************************************************************)
