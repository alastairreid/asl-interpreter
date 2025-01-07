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

let to_string2 (pp : Format.formatter -> unit) : string =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let to_file (filename : string) (pp : Format.formatter -> unit) : unit =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  pp fmt;
  Format.pp_print_flush fmt ();
  close_out chan

(****************************************************************
 * List related
 ****************************************************************)

let is_empty (xs : 'a list) : bool = match xs with [] -> true | _ -> false

let nub (xs : 'a list) : 'a list =
  let rec nub_aux seen xs =
    match xs with
    | [] -> seen
    | y :: ys ->
        if List.mem y seen then nub_aux seen ys else nub_aux (y :: seen) ys
  in
  nub_aux [] xs

(** init [x1; ... xn] = [x1; ... xn-1] *)
let init (xs : 'a list) : 'a list =
  (* tail recursive helper function *)
  let rec aux r xs =
    ( match xs with
    | [x] -> List.rev r
    | (x :: xs') -> aux (x :: r) xs'
    | [] -> failwith "init []"
    )
  in
  aux [] xs

(** last [x1; ... xn] = xn *)
let rec last (xs : 'a list) : 'a =
  ( match xs with
  | [x] -> x
  | (_ :: xs') -> last xs'
  | [] -> failwith "last []"
  )

(** drop_left m [x1; ... xn] = [xm+1; ... xn] *)
let rec drop_left m (xs : 'a list) : 'a list =
  ( match xs with
  | [] -> []
  | (x :: xs') when m > 0 -> drop_left (m-1) xs'
  | _ -> xs
  )

(** drop_right m [x1; ... xn] = [x1; ... xn-m] *)
let drop_right m (xs : 'a list) : 'a list =
  (* tail recursive helper function *)
  let rec aux r xs =
    ( match xs with
    | [] -> List.rev (drop_left m r)
    | (x :: xs') -> aux (x :: r) xs'
    )
  in
  aux [] xs

(** split3 [(x1, y1, z1); ... (xn, yn, zn)] = ([x1; ... xn], [y1; ... yn], [z1; ... zn]) *)
let split3 (xyzs : ('x * 'y * 'z) list) : ('x list * 'y list * 'z list) =
  List.fold_right
    (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs))
    xyzs
    ([], [], [])

(** iter3 f [x1;x2;...] [y1;y2;...] [z1;z2;...] = f x1 y1 z1; f x2 y2 z2; ... *)
let rec iter3 (f : 'a -> 'b -> 'c -> unit) (xs : 'a list) (ys : 'b list) (zs : 'c list) =
  ( match (xs, ys, zs) with
  | ([], [], []) -> ()
  | (x :: xs, y :: ys, z :: zs) -> f x y z; iter3 f xs ys zs
  | _ -> failwith "iter3 with mismatched lengths"
  )

(** iter_pairs f [x1;x2;x3;x4;...] = f x1 x2; f x1 x3; f x1 x4; ...; f x2 x3; f x2 x4 *)
let iter_pairs (f : 'a -> 'a -> unit) (xs : 'a list) : unit =
  ( match xs with
  | (x :: xs) -> List.iter (f x) xs
  | [] -> ()
  )

(** apply 'f' to all combinations of x from xs and y from ys *)
let cross_combine (f : 'a -> 'b -> 'c) (xs : 'a list) (ys : 'b list) : 'c list =
  List.fold_left (fun r x ->
      List.fold_left (fun r y ->
          f x y :: r
      ) r ys
  ) [] xs
  |> List.rev

(** Cartesian product.
  Example:
  # let ls = [[1;2]; [3;4]];;
  val ls : int list list = [[1; 2]; [3; 4]]
  # cartesian_product ls;;
  - : int list list = [[1; 3]; [1; 4]; [2; 3]; [2; 4]]
*)
let cartesian_product (xss : 'a list list) : 'a list list =
  let f (xss' : 'a list list) (ys : 'a list) : 'a list list =
    xss' |> List.concat_map (fun xs' -> ys |> List.map (fun y -> y :: xs'))
  in
  List.fold_left f [[]] xss |> List.map (List.rev)

(****************************************************************
 * Option related
 ****************************************************************)

let from_option (ox : 'a option) (d : unit -> 'a) : 'a =
  match ox with None -> d () | Some x -> x

let orelse_option (ox : 'a option) (f : unit -> 'a option) : 'a option =
  match ox with None -> f () | Some _ -> ox

let map2_option (f : 'a -> 'b -> 'c) (o1 : 'a option) (o2 : 'b option) : 'c option =
  ( match (o1, o2) with
  | (Some x1, Some x2) -> Some (f x1 x2)
  | _ -> None
  )

(* Examples:
 * concat_option [ Some [1;2]; None; Some [3] ];;
 * None
 * concat_option [ Some [1;2]; Some [3] ];;
 * Some [1; 2; 3]
 *)
let rec concat_option (oss : 'a list option list) : 'a list option =
  match oss with
  | [] -> Some []
  | None :: _ -> None
  | Some xs :: xss -> Option.map (List.append xs) (concat_option xss)

(* extract all non-None elements from a list *)
let flatten_option (os : 'a option list) : 'a list =
  let f acc = function Some o -> (o :: acc) | None -> acc in
  List.fold_left f [] os |> List.rev

(* Like Option.filter_map for binary operations
 *
 * `filter_map2 f xs ys` applies `f` to every element of `xs` and `ys`,
 * filters out the None elements and returns the list of the arguments
 * of the Some elements.
 * *)
let filter_map2 (f : 'a -> 'b -> 'c option) (xs : 'a list) (ys : 'b list) : 'c list =
  List.map2 f xs ys |> flatten_option

(* If any element of 'map f xs' is None, return None
 * Otherwise, return 'map (Option.get . f) xs'
 *)
(* todo: give this a better name *)
let flatten_map_option (f : 'a -> 'b option) (xs : 'a list) : 'b list option =
  let rec aux r xs =
    match xs with
    | [] -> Some (List.rev r)
    | x :: xs' -> ( match f x with Some b -> aux (b :: r) xs' | None -> None)
  in
  aux [] xs

(* find first non-None result from function 'f' on list 'xs' *)
let rec first_option (f : 'a -> 'b option) (xs : 'a list) : 'b option =
  match xs with
  | [] -> None
  | x :: xs' -> (
      match f x with Some b -> Some b | None -> first_option f xs')

(****************************************************************
 * String related
 ****************************************************************)

(** Drop first n characters from string *)
let string_drop (n : int) (s : string) : string =
  let l = String.length s in
  if n > l then "" else String.sub s n (l - n)

(** Convert a char list to a string *)
let string_of_chars (cs : char list) : string =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

(** Delete all characters matching 'c' from string 'x' *)
let drop_chars (x : string) (c : char) : string =
  (* First calculate final length *)
  let len = ref 0 in
  String.iter (fun t -> if t <> c then len := !len + 1) x;

  (* search for next character not matching c *)
  let i = ref 0 in
  let rec next_char (_ : int) : char =
    let r = String.get x !i in
    i := !i + 1;
    if r = c then next_char 0 else r
  in

  (* create result *)
  String.init !len next_char

(****************************************************************
 * Compare related
 ****************************************************************)

(** combine the result of two compare functions *)
let ( <?> ) c (cmp, x, y) = if c = 0 then cmp x y else c

(****************************************************************
 * File related
 ****************************************************************)

(** load a file into a buffer *)
let read_file (name : string) : bytes =
  let c = open_in_bin name in
  let inc = 1000000 in
  let b = ref (Bytes.create inc) in
  let rec read (pos : int) : unit =
    b := Bytes.extend !b 0 (pos + inc - Bytes.length !b);
    let r = input c !b pos inc in
    if r <> 0 then read (pos + r)
  in
  read 0;
  close_in c;
  !b

(****************************************************************
 * Int related
 ****************************************************************)

(** 2^i *)
let pow2 (i : int) : int =
    assert (i < 63);
    (Z.to_int (Z.shift_left Z.one i))

(****************************************************************
 * End
 ****************************************************************)
