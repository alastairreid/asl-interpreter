(****************************************************************
 * ASL interpreter values
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type value =
  | VBool of bool (* optimised special case of VEnum *)
  | VEnum of (Ident.t * int)
  | VInt of Primops.bigint
  | VReal of Primops.real
  | VBits of Primops.bitvector
  | VIntN of Primops.sintN
  | VMask of Primops.mask
  | VString of string
  | VTuple of value list
  | VRecord of (Ident.t * value Identset.Bindings.t)
  | VArray of (value Primops.ImmutableArray.t * value)
  | VRAM of Primops.ram
  | VUninitialized

exception Return of value option
exception EvalError of (Loc.t * string)
exception Throw of (Loc.t * value)
exception EndExecution of Loc.t

val pp_value : Format.formatter -> value -> unit
val string_of_value : value -> string
val eq_value : value -> value -> bool

val eval_prim : Ident.t -> value list -> value list -> value option
val impure_prims : string list

(* value constructors and destructors *)

val from_bool : bool -> value
val to_bool : Loc.t -> value -> bool
val int_one : value
val to_integer : Loc.t -> value -> Primops.bigint
val to_int : Loc.t -> value -> int
val to_bits : Loc.t -> value -> Primops.bitvector
val to_mask : Loc.t -> value -> Primops.mask
val to_string : Loc.t -> value -> string
val to_tuple : value list -> value
val of_tuple : Loc.t -> value -> value list
val mk_record : Ident.t -> (Ident.t * value) list -> value
val get_field : Loc.t -> value -> Ident.t -> value
val set_field : Loc.t -> value -> Ident.t -> value -> value
val empty_array : value -> value
val init_array : (int * value) list -> value -> value
val get_array : Loc.t -> value -> value -> value
val set_array : Loc.t -> value -> value -> value -> value


(* convert AST nodes to values *)

val negate_intLit : (int option * Z.t) -> (int option * Z.t)
val from_intLit : (int option * Z.t) -> value option
val from_hexLit : string -> value
val from_realLit : string -> value
val from_bitsLit : string -> value
val from_maskLit : string -> value
val from_stringLit : string -> value

(* bitvector manipulation *)

val extract_bits : Loc.t -> value -> value -> value -> value
val extract_bits' : Loc.t -> value -> int -> int -> value
val extract_bits'' : Loc.t -> value -> value -> value -> value
val insert_int : Loc.t -> value -> value -> value -> value -> value
val insert_bits : Loc.t -> value -> value -> value -> value -> value
val insert_bits' : Loc.t -> value -> int -> int -> value -> value

(* functions used in interpreter *)

val eval_eq : Loc.t -> value -> value -> bool
val eval_leq : Loc.t -> value -> value -> bool
val eval_inc : Loc.t -> value -> value
val eval_dec : Loc.t -> value -> value
val eval_eq_int : Loc.t -> value -> value -> bool
val eval_eq_bits : Loc.t -> value -> value -> bool
val eval_inmask : Loc.t -> value -> value -> bool
val eval_add_int : Loc.t -> value -> value -> value
val eval_mul_int : Loc.t -> value -> value -> value
val eval_sub_int : Loc.t -> value -> value -> value
val eval_concat : Loc.t -> value list -> value

(* unknowns of various types *)

val eval_unknown_bits : Primops.bigint -> value
val eval_unknown_ram : Primops.bigint -> value
val eval_unknown_integer : unit -> value
val eval_unknown_real : unit -> value
val eval_unknown_string : unit -> value

(* tracing support *)

module type Tracer = sig
  val trace_next : unit -> unit

  val trace_physical_memory : is_read:bool -> is_data:bool -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_virtual_memory : is_read:bool -> is_data:bool -> context:Z.t -> virt_addr:Z.t -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_memory_pte : context:Z.t -> level:Z.t -> phys_addr:Z.t -> data:Primops.bitvector -> unit

  val trace_error : kind:string -> string list -> unit

  val trace_event : kind:string -> string list -> unit

  val trace_function : is_prim:bool -> is_return:bool -> Ident.t -> value list -> value list -> unit

  val trace_var : is_local:bool -> is_read:bool -> Ident.t -> value -> unit
end

val tracer : (module Tracer) ref

module TextTracer : Tracer

(****************************************************************
 * End
 ****************************************************************)
