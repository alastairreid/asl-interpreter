(****************************************************************
 * Formatting utilities
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module ColorT = Ocolor_types
open Format

val vbox : formatter -> (unit -> unit) -> unit
val hbox : formatter -> (unit -> unit) -> unit
val hvbox : formatter -> (unit -> unit) -> unit
val indented : formatter -> (unit -> unit) -> unit
val flush : formatter -> unit
val space : formatter -> unit
val cut : formatter -> unit
val nbsp : formatter -> unit
val comma : formatter -> unit
val lbrace : formatter -> unit
val rbrace : formatter -> unit
val lbrack : formatter -> unit
val rbrack : formatter -> unit
val lparen : formatter -> unit
val rparen : formatter -> unit
val sepby : formatter -> (unit -> unit) -> ('a -> unit) -> 'a list -> unit
val cutsep : formatter -> ('a -> unit) -> 'a list -> unit
val map : formatter -> ('a -> unit) -> 'a list -> unit
val hlist : formatter -> (unit -> unit) -> ('a -> unit) -> 'a list -> unit

val surround :
  formatter ->
  (formatter -> unit) ->
  (formatter -> unit) ->
  (unit -> unit) ->
  unit

val with_color : formatter -> ColorT.color4 -> (unit -> unit) -> unit
val commasep : formatter -> ('a -> unit) -> 'a list -> unit
val braces : formatter -> (unit -> unit) -> unit
val brackets : formatter -> (unit -> unit) -> unit
val parens : formatter -> (unit -> unit) -> unit

(*****************************************
 * End
 *****************************************)
