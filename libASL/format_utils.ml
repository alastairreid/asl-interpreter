(****************************************************************
 * Formatting utilities
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Formatting utilities *)

module PP = Format
module Color = Ocolor_format
module ColorT = Ocolor_types

let indentation = 4
let flush (fmt : PP.formatter) = PP.pp_print_flush fmt ()
let space (fmt : PP.formatter) : unit = PP.pp_print_space fmt ()
let cut (fmt : PP.formatter) : unit = PP.pp_print_cut fmt ()
let nbsp (fmt : PP.formatter) : unit = PP.pp_print_string fmt " "
let comma (fmt : PP.formatter) : unit = PP.pp_print_string fmt ","
let lbrace (fmt : PP.formatter) : unit = PP.pp_print_string fmt "{"
let rbrace (fmt : PP.formatter) : unit = PP.pp_print_string fmt "}"
let lbrack (fmt : PP.formatter) : unit = PP.pp_print_string fmt "["
let rbrack (fmt : PP.formatter) : unit = PP.pp_print_string fmt "]"
let lparen (fmt : PP.formatter) : unit = PP.pp_print_string fmt "("
let rparen (fmt : PP.formatter) : unit = PP.pp_print_string fmt ")"

let vbox (fmt : PP.formatter) (pp : unit -> unit) =
  PP.pp_open_vbox fmt 0;
  pp ();
  PP.pp_close_box fmt ()

let indented (fmt : PP.formatter) (pp : unit -> unit) =
  PP.pp_print_break fmt indentation indentation;
  vbox fmt pp

let hbox (fmt : PP.formatter) (pp : unit -> unit) =
  PP.pp_open_hbox fmt ();
  pp ();
  PP.pp_close_box fmt ()

let hvbox (fmt : PP.formatter) (pp : unit -> unit) =
  PP.pp_open_hvbox fmt indentation;
  pp ();
  PP.pp_close_box fmt ()

let sepby (fmt : PP.formatter) (sep : unit -> unit) (pp : 'a -> unit)
    (xs : 'a list) : unit =
  PP.pp_print_list ~pp_sep:(fun _ -> sep) (fun _ -> pp) fmt xs

let with_color (fmt : PP.formatter) (color : ColorT.color4) (pp : unit -> unit)
    : unit =
  Color.pp_open_style fmt (Fg (C4 color));
  pp ();
  Color.pp_close_style fmt ()

let cutsep (fmt : PP.formatter) (pp : 'a -> unit) (xs : 'a list) : unit =
  sepby fmt (fun _ -> cut fmt) pp xs

let map (fmt : PP.formatter) (pp : 'a -> unit) (xs : 'a list) : unit =
  sepby fmt (fun _ -> ()) pp xs

let hlist (fmt : PP.formatter) (sep : unit -> unit) (pp : 'a -> unit)
    (xs : 'a list) : unit =
  sepby fmt (fun _ -> nbsp fmt) pp xs

let surround (fmt : PP.formatter) (l : PP.formatter -> unit)
    (r : PP.formatter -> unit) (m : unit -> unit) =
  l fmt;
  m ();
  r fmt

let commasep (fmt : PP.formatter) (pp : 'a -> unit) (xs : 'a list) : unit =
  sepby fmt
    (fun _ ->
      comma fmt;
      nbsp fmt)
    pp xs

let braces (fmt : PP.formatter) (pp : unit -> unit) =
  surround fmt lbrace rbrace pp

let brackets (fmt : PP.formatter) (pp : unit -> unit) =
  surround fmt lbrack rbrack pp

let parens (fmt : PP.formatter) (pp : unit -> unit) =
  surround fmt lparen rparen pp

(****************************************************************
 * End
 ****************************************************************)
