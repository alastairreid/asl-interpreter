(****************************************************************
 * ISA format
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** ISA format *)

module PP = Format
module AST = Isa_ast
module ColorT = Ocolor_types
open Format_utils
open Builtin_idents

(** Optionally show type parameters when printing ISA code *)
let show_type_params = ref false

(** The typechecker desugars infix syntax to make it absolutely explicit
 *  what it means.  This is good for tools but bad for humans.
 *
 *  This flag causes expressions to be displayed with infix syntax.
 *)
let resugar_operators = ref true

(** This definition of Bindings is repeated from asl_utils.ml to
 *  avoid a circular dependency.
 *)
module Bindings = Map.Make (Ident)

(** Table of binary operators used for resugaring operators *)
let binop_table : AST.binop Bindings.t ref = ref Bindings.empty

let add_binop (op : AST.binop) (x : Ident.t) : unit =
  binop_table := Bindings.add x op !binop_table

(** Table of unary operators used for resugaring operators *)
let unop_table : AST.unop Bindings.t ref = ref Bindings.empty

let add_unop (op : AST.unop) (x : Ident.t) : unit =
  unop_table := Bindings.add x op !unop_table

(** Helper function printing nothing *)
let none (fmt : PP.formatter) : unit = ()

let loc (fmt : PP.formatter) (x : Loc.t) : unit = Loc.pp fmt x

let delimiter (fmt : PP.formatter) (s : string) : unit =
  with_color fmt ColorT.magenta (fun _ -> PP.pp_print_string fmt s)

let keyword (fmt : PP.formatter) (s : string) : unit =
  with_color fmt ColorT.red (fun _ -> PP.pp_print_string fmt s)

let ident (fmt : PP.formatter) (color : ColorT.color4) (x : Ident.t) : unit =
  with_color fmt color (fun _ -> Ident.pp fmt x)

let tycon (fmt : PP.formatter) (x : Ident.t) : unit = ident fmt ColorT.green x

let funname (fmt : PP.formatter) (x : Ident.t) : unit =
  ident fmt ColorT.hi_cyan x

let varname (fmt : PP.formatter) (x : Ident.t) : unit =
  ident fmt ColorT.cyan x

let fieldname (fmt : PP.formatter) (x : Ident.t) : unit =
  ident fmt ColorT.yellow x

(* ISA delimiters *)

let amp (fmt : PP.formatter) : unit = delimiter fmt "&"
let amp_amp (fmt : PP.formatter) : unit = delimiter fmt "&&"
let bang (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar_bar (fmt : PP.formatter) : unit = delimiter fmt "||"
let colon (fmt : PP.formatter) : unit = delimiter fmt ":"
let colon_eq (fmt : PP.formatter) : unit = delimiter fmt ":="
let dot (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot (fmt : PP.formatter) : unit = delimiter fmt ".."
let eq (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq (fmt : PP.formatter) : unit = delimiter fmt "=="
let eq_gt (fmt : PP.formatter) : unit = delimiter fmt "=>"
let gt (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt (fmt : PP.formatter) : unit = delimiter fmt ">>"
let lt (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt (fmt : PP.formatter) : unit = delimiter fmt "<<"
let minus (fmt : PP.formatter) : unit = delimiter fmt "-"
let plus (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_colon (fmt : PP.formatter) : unit = delimiter fmt "+:"
let minus_colon (fmt : PP.formatter) : unit = delimiter fmt "-:"
let star_colon (fmt : PP.formatter) : unit = delimiter fmt "*:"
let plus_plus (fmt : PP.formatter) : unit = delimiter fmt "++"
let semicolon (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash (fmt : PP.formatter) : unit = delimiter fmt "/"
let star (fmt : PP.formatter) : unit = delimiter fmt "*"
let star_star (fmt : PP.formatter) : unit = delimiter fmt "**"
let underscore (fmt : PP.formatter) : unit = delimiter fmt "_"

(* keywords that are used as operators *)
let kw_and (fmt : PP.formatter) : unit = delimiter fmt "and"
let kw_in (fmt : PP.formatter) : unit = delimiter fmt "in"
let kw_not (fmt : PP.formatter) : unit = delimiter fmt "not"
let kw_or (fmt : PP.formatter) : unit = delimiter fmt "or"
let kw_array (fmt : PP.formatter) : unit = keyword fmt "array"
let kw_as (fmt : PP.formatter) : unit = delimiter fmt "as"
let kw_assert (fmt : PP.formatter) : unit = keyword fmt "assert"
let kw_begin (fmt : PP.formatter) : unit = keyword fmt "begin"
let kw_bitfield (fmt : PP.formatter) : unit = keyword fmt "bitfield"
let kw_bits (fmt : PP.formatter) : unit = keyword fmt "bits"
let kw_case (fmt : PP.formatter) : unit = keyword fmt "case"
let kw_catch (fmt : PP.formatter) : unit = keyword fmt "catch"
let kw_config (fmt : PP.formatter) : unit = keyword fmt "config"
let kw_constant (fmt : PP.formatter) : unit = keyword fmt "constant"
let kw_do (fmt : PP.formatter) : unit = keyword fmt "do"
let kw_downto (fmt : PP.formatter) : unit = keyword fmt "downto"
let kw_else (fmt : PP.formatter) : unit = keyword fmt "else"
let kw_elsif (fmt : PP.formatter) : unit = keyword fmt "elsif"
let kw_end (fmt : PP.formatter) : unit = keyword fmt "end"
let kw_endcase (fmt : PP.formatter) : unit = keyword fmt "endcase"
let kw_endfor (fmt : PP.formatter) : unit = keyword fmt "endfor"
let kw_endif (fmt : PP.formatter) : unit = keyword fmt "endif"
let kw_endtry (fmt : PP.formatter) : unit = keyword fmt "endtry"
let kw_endwhile (fmt : PP.formatter) : unit = keyword fmt "endwhile"
let kw_enumeration (fmt : PP.formatter) : unit = keyword fmt "enumeration"
let kw_export (fmt : PP.formatter) : unit = keyword fmt "export"
let kw_for (fmt : PP.formatter) : unit = keyword fmt "for"
let kw_foreign (fmt : PP.formatter) : unit = keyword fmt "foreign"
let kw_function (fmt : PP.formatter) : unit = keyword fmt "function"
let kw_if (fmt : PP.formatter) : unit = keyword fmt "if"
let kw_import (fmt : PP.formatter) : unit = keyword fmt "import"
let kw_let (fmt : PP.formatter) : unit = keyword fmt "let"
let kw_of (fmt : PP.formatter) : unit = keyword fmt "of"
let kw_optimize (fmt : PP.formatter) : unit = keyword fmt "optimize"
let kw_record (fmt : PP.formatter) : unit = keyword fmt "record"
let kw_repeat (fmt : PP.formatter) : unit = keyword fmt "repeat"
let kw_return (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_then (fmt : PP.formatter) : unit = keyword fmt "then"
let kw_throw (fmt : PP.formatter) : unit = keyword fmt "throw"
let kw_to (fmt : PP.formatter) : unit = keyword fmt "to"
let kw_try (fmt : PP.formatter) : unit = keyword fmt "try"
let kw_type (fmt : PP.formatter) : unit = keyword fmt "type"
let kw_typeof (fmt : PP.formatter) : unit = keyword fmt "typeof"
let kw_underscore_array (fmt : PP.formatter) : unit = keyword fmt "__array"
let kw_underscore_builtin (fmt : PP.formatter) : unit = keyword fmt "__builtin"
let kw_underscore_operator1 (fmt : PP.formatter) : unit = keyword fmt "__operator1"
let kw_underscore_operator2 (fmt : PP.formatter) : unit = keyword fmt "__operator2"
let kw_underscore_readwrite (fmt : PP.formatter) : unit = keyword fmt "__readwrite"
let kw_underscore_write (fmt : PP.formatter) : unit = keyword fmt "__write"
let kw_unknown (fmt : PP.formatter) : unit = keyword fmt "UNSPECIFIED"
let kw_until (fmt : PP.formatter) : unit = keyword fmt "until"
let kw_use (fmt : PP.formatter) : unit = keyword fmt "use"
let kw_var (fmt : PP.formatter) : unit = keyword fmt "var"
let kw_when (fmt : PP.formatter) : unit = keyword fmt "when"
let kw_while (fmt : PP.formatter) : unit = keyword fmt "while"
let kw_with (fmt : PP.formatter) : unit = keyword fmt "with"
let kw_xor (fmt : PP.formatter) : unit = delimiter fmt "xor"

let varnames (fmt : PP.formatter) (xs : Ident.t list) : unit =
  commasep fmt (varname fmt) xs

let funnames (fmt : PP.formatter) (xs : Ident.t list) : unit =
  commasep fmt (funname fmt) xs

let binop (fmt : PP.formatter) (x : AST.binop) : unit =
  match x with
  | Binop_Eq -> eq_eq fmt
  | Binop_NtEq -> bang_eq fmt
  | Binop_Gt -> gt fmt
  | Binop_GtEq -> gt_eq fmt
  | Binop_Lt -> lt fmt
  | Binop_LtEq -> lt_eq fmt
  | Binop_Plus -> plus fmt
  | Binop_Minus -> minus fmt
  | Binop_Multiply -> star fmt
  | Binop_Divide -> slash fmt
  | Binop_Remainder -> delimiter fmt "%"
  | Binop_Power -> star_star fmt
  | Binop_ShiftL -> lt_lt fmt
  | Binop_ShiftR -> gt_gt fmt
  | Binop_AndThen -> delimiter fmt "and then"
  | Binop_OrElse -> delimiter fmt "or else"
  | Binop_Or -> kw_or fmt
  | Binop_Xor -> kw_xor fmt
  | Binop_And -> kw_and fmt
  | Binop_Append -> plus_plus fmt
  | Binop_DUMMY ->
      PP.pp_print_string fmt "Binop_DUMMY" (* todo: throw an error? *)
  (* deprecated *)
  | Binop_Quot -> keyword fmt "QUOT"
  | Binop_Rem -> keyword fmt "REM"
  | Binop_Div_exact -> keyword fmt "DIV"
  | Binop_Divrm -> keyword fmt "DIVRM"
  | Binop_Mod -> keyword fmt "MOD"
  | Binop_BoolImplies -> delimiter fmt "-->"


let unop (fmt : PP.formatter) (x : AST.unop) : unit =
  match x with
  | Unop_Negate -> minus fmt
  | Unop_Not -> kw_not fmt

let throws (fmt : PP.formatter) (x : AST.can_throw) : unit =
  ( match x with
  | NoThrow -> ()
  | MayThrow -> PP.pp_print_string fmt "?"
  | AlwaysThrow -> PP.pp_print_string fmt "!"
  )

let rec ty (fmt : PP.formatter) (x : AST.ty) : unit =
  match x with
  | Type_Integer None ->
      tycon fmt integer_ident
  | Type_Integer (Some ranges) ->
      set_ranges fmt ranges
  | Type_Bits (n, fs) ->
      tycon fmt bits_ident;
      parens fmt (fun _ -> expr fmt n);
      if not (Utils.is_empty fs) then begin
        nbsp fmt;
        braces fmt (fun _ ->
            indented fmt (fun _ -> cutsep fmt (regfield fmt) fs);
            cut fmt)
      end
  | Type_Constructor (tc, es) ->
      tycon fmt tc;
      if not (Utils.is_empty es) then parens fmt (fun _ -> exprs fmt es)
  | Type_OfExpr e ->
      kw_typeof fmt;
      parens fmt (fun _ -> expr fmt e)
  | Type_Array (ixty, ety) ->
      kw_array fmt;
      nbsp fmt;
      ixtype fmt ixty;
      nbsp fmt;
      kw_of fmt;
      nbsp fmt;
      ty fmt ety
  | Type_Tuple tys -> parens fmt (fun _ -> types fmt tys)

and types (fmt : PP.formatter) (tys : AST.ty list) : unit =
  commasep fmt (ty fmt) tys

and set_range (fmt : PP.formatter) (x : AST.set_range) : unit =
  match x with
  | Set_Single e -> expr fmt e
  | Set_Range (lo, hi) ->
      PP.pp_print_option expr fmt lo;
      dot_dot fmt;
      PP.pp_print_option expr fmt hi

and set_ranges (fmt : PP.formatter) (x : AST.set_range list) : unit =
  braces fmt (fun _ -> commasep fmt (set_range fmt) x)

and regfield (fmt : PP.formatter) (rf : AST.slice list * Ident.t) : unit =
  PP.fprintf fmt "  %a => [%a]"
    fieldname (snd rf)
    (fun fmt -> commasep fmt (slice fmt)) (fst rf)

and slice (fmt : PP.formatter) (x : AST.slice) : unit =
  match x with
  | Slice_Single e -> expr fmt e
  | Slice_HiLo (hi, lo) ->
      expr fmt hi;
      nbsp fmt;
      colon fmt;
      nbsp fmt;
      expr fmt lo
  | Slice_LoWd (lo, wd) ->
      expr fmt lo;
      nbsp fmt;
      plus_colon fmt;
      nbsp fmt;
      expr fmt wd
  | Slice_HiWd (hi, wd) ->
      expr fmt hi;
      nbsp fmt;
      minus_colon fmt;
      nbsp fmt;
      expr fmt wd
  | Slice_Element (lo, wd) ->
      expr fmt lo;
      nbsp fmt;
      star_colon fmt;
      nbsp fmt;
      expr fmt wd

and slices (fmt : PP.formatter) (ss : AST.slice list) : unit =
  commasep fmt (slice fmt) ss

and changes (fmt : PP.formatter) (cs : (AST.change * AST.expr) list) : unit =
  commasep fmt (fun (c, e) -> PP.fprintf fmt "%a = %a" change c expr e) cs

and change (fmt : PP.formatter) (x : AST.change) : unit =
  ( match x with
  | Change_Field f -> fieldname fmt f
  | Change_Slices ss -> brackets fmt (fun _ -> slices fmt ss)
  )

and ixtype (fmt : PP.formatter) (x : AST.ixtype) : unit =
  match x with
  | Index_Enum tc -> tycon fmt tc
  | Index_Int sz -> brackets fmt (fun _ -> expr fmt sz)

and expr (fmt : PP.formatter) (x : AST.expr) : unit =
  ( match x with
  | Expr_If (els, e) ->
    parens fmt (fun _ ->
      let first = ref true in
      map fmt
        (fun (c, e) ->
          (if !first
           then kw_if fmt
           else begin nbsp fmt; kw_elsif fmt end);
          first := false;
          nbsp fmt;
          expr fmt c;
          nbsp fmt;
          kw_then fmt;
          nbsp fmt;
          expr fmt e
        )
        els;
      nbsp fmt;
      kw_else fmt;
      nbsp fmt;
      expr fmt e
    )
  | Expr_Let (v, t, e, b) ->
      PP.fprintf fmt "(__let %a : %a := %a __in %a)"
        varname v
        ty t
        expr e
        expr b
  | Expr_Assert (e1, e2, loc) ->
      PP.fprintf fmt "(__assert %a __in %a)"
        expr e1
        expr e2
  | Expr_Binop (a, op, b) ->
      PP.fprintf fmt "(%a %a %a)"
        expr a
        binop op
        expr b
  | Expr_Field (e, f) ->
      expr fmt e;
      dot fmt;
      fieldname fmt f
  | Expr_Fields (e, fs) ->
      expr fmt e;
      dot fmt;
      brackets fmt (fun _ -> commasep fmt (fieldname fmt) fs)
  | Expr_Slices (t, e, ss) ->
      if !show_type_params then braces fmt (fun _ -> ty fmt t);
      expr fmt e;
      brackets fmt (fun _ -> slices fmt ss)
  | Expr_WithChanges (t, e, cs) ->
      if !show_type_params then braces fmt (fun _ -> ty fmt t);
      PP.fprintf fmt "(%a %t { %a })"
        expr e
        kw_with
        changes cs
  | Expr_Record (tc, tes, fas) ->
      tycon fmt tc;
      if not (Utils.is_empty tes) then parens fmt (fun _ -> pp_args fmt tes);
      braces fmt (fun _ -> commasep fmt (field_assignment fmt) fas)
  | Expr_ArrayInit es ->
      PP.fprintf fmt "%t (%a)"
        kw_array
        exprs es
  | Expr_In (e, p) ->
      PP.fprintf fmt "(%a IN %a)"
        expr e
        pattern p
  | Expr_Var v -> varname fmt v
  | Expr_TApply (f, tes, [a], throws) when !resugar_operators && Bindings.mem f !unop_table ->
      let op = Bindings.find f !unop_table in
      unop fmt op;
      if !show_type_params then braces fmt (fun _ -> exprs fmt tes);
      nbsp fmt;
      expr fmt a
  | Expr_TApply (f, tes, [a; b], throws) when !resugar_operators && Bindings.mem f !binop_table ->
      let op = Bindings.find f !binop_table in
      expr fmt a;
      nbsp fmt;
      binop fmt op;
      if !show_type_params then braces fmt (fun _ -> exprs fmt tes);
      nbsp fmt;
      expr fmt b
  | Expr_TApply (f, tes, [a; b], throws) when !resugar_operators && Ident.equal f Builtin_idents.lazy_and_bool ->
      PP.fprintf fmt "%a and then %a"
        expr a
        expr b
  | Expr_TApply (f, tes, [a; b], throws) when !resugar_operators && Ident.equal f Builtin_idents.lazy_or_bool ->
      PP.fprintf fmt "%a or else %a"
        expr a
        expr b
  | Expr_TApply (f, tes, es, can_throw) ->
      funname fmt f;
      if !show_type_params then braces fmt (fun _ -> exprs fmt tes);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt es)
  | Expr_UApply (f, use_array_syntax, args, can_throw) ->
      expr fmt f;
      throws fmt can_throw;
      (if use_array_syntax then
          brackets fmt (fun _ -> pp_args fmt args)
      else
          parens fmt (fun _ -> pp_args fmt args)
      )
  | Expr_Tuple es -> parens fmt (fun _ -> exprs fmt es)
  | Expr_Concat (ws, es) ->
      if !show_type_params then braces fmt (fun _ -> exprs fmt ws);
      brackets fmt (fun _ -> exprs fmt es)
  | Expr_Unop (op, e) ->
      PP.fprintf fmt "(%a %a)"
        unop op
        expr e
  | Expr_Unknown t ->
      kw_unknown fmt;
      nbsp fmt;
      colon fmt;
      nbsp fmt;
      ty fmt t
  | Expr_Array (a, e) ->
      expr fmt a;
      brackets fmt (fun _ -> expr fmt e)
  | Expr_Slice s -> slice fmt s
  | Expr_Lit v -> Value.pp_value fmt v
  | Expr_AsType (e, t) ->
      parens fmt (fun _ ->
        expr fmt e;
        nbsp fmt;
        kw_as fmt;
        nbsp fmt;
        ty fmt t
      )
  )

and exprs (fmt : PP.formatter) (es : AST.expr list) : unit =
  commasep fmt (expr fmt) es

and pp_arg (fmt : PP.formatter) (x : Ident.t option * AST.expr) : unit =
  ( match x with
  | (None, e) -> expr fmt e
  | (Some v, e) -> PP.fprintf fmt "%a => %a"
                     varname v
                     expr e
  )

and pp_args (fmt : PP.formatter) (args : (Ident.t option * AST.expr) list) : unit =
  commasep fmt (pp_arg fmt) args

and field_assignment (fmt : PP.formatter) (x : Ident.t * AST.expr) : unit =
  match x with
  | f, e ->
      fieldname fmt f;
      nbsp fmt;
      eq_gt fmt;
      nbsp fmt;
      expr fmt e

and pattern (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_Lit v -> Value.pp_value fmt v
  | Pat_Const v -> varname fmt v
  | Pat_Wildcard -> underscore fmt
  | Pat_Tuple ps -> parens fmt (fun _ -> patterns fmt ps)
  | Pat_Set ps -> braces fmt (fun _ -> patterns fmt ps)
  | Pat_Single e -> expr fmt e
  | Pat_Range (lo, hi) ->
      expr fmt lo;
      nbsp fmt;
      dot_dot fmt;
      nbsp fmt;
      expr fmt hi

and patterns (fmt : PP.formatter) (ps : AST.pattern list) : unit =
  commasep fmt (pattern fmt) ps

let rec lexpr (fmt : PP.formatter) (x : AST.lexpr) : unit =
  match x with
  | LExpr_Wildcard -> minus fmt
  | LExpr_Var v -> varname fmt v
  | LExpr_Field (e, f) ->
      lexpr fmt e;
      dot fmt;
      fieldname fmt f
  | LExpr_Fields (e, fs) ->
      lexpr fmt e;
      dot fmt;
      brackets fmt (fun _ -> commasep fmt (fieldname fmt) fs)
  | LExpr_Slices (t, e, ss) ->
      if !show_type_params then braces fmt (fun _ -> ty fmt t);
      lexpr fmt e;
      brackets fmt (fun _ -> slices fmt ss)
  | LExpr_BitTuple (ws, es) ->
      if !show_type_params then braces fmt (fun _ -> exprs fmt ws);
      brackets fmt (fun _ -> lexprs fmt es)
  | LExpr_Tuple es -> parens fmt (fun _ -> lexprs fmt es)
  | LExpr_Array (a, e) ->
      lexpr fmt a;
      brackets fmt (fun _ -> expr fmt e)
  | LExpr_Write (f, tes, es, can_throw) ->
      kw_underscore_write fmt;
      nbsp fmt;
      funname fmt f;
      if !show_type_params then (
        lbrace fmt;
        exprs fmt tes;
        rbrace fmt);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt es)
  | LExpr_ReadWrite (f, g, tes, es, can_throw) ->
      kw_underscore_readwrite fmt;
      nbsp fmt;
      funname fmt f;
      nbsp fmt;
      funname fmt g;
      if !show_type_params then (
        lbrace fmt;
        exprs fmt tes;
        rbrace fmt);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt es)

and lexprs (fmt : PP.formatter) (ps : AST.lexpr list) : unit =
  commasep fmt (lexpr fmt) ps

let varty (fmt : PP.formatter) (v : Ident.t) (t : AST.ty) : unit =
  varname fmt v;
  nbsp fmt;
  colon fmt;
  nbsp fmt;
  ty fmt t

let varoty (fmt : PP.formatter) (v : Ident.t) (ot : AST.ty option) : unit =
  match ot with None -> varname fmt v | Some t -> varty fmt v t

let direction (fmt : PP.formatter) (x : AST.direction) : unit =
  match x with Direction_Up -> kw_to fmt | Direction_Down -> kw_downto fmt

let decl_bit (fmt : PP.formatter) (x : (Ident.t option * AST.ty)) : unit =
  let (ov, ty) = x in
  varty fmt (Option.value ov ~default:dash_ident) ty

let rec decl_item (fmt : PP.formatter) (x : AST.decl_item) : unit =
  ( match x with
  | DeclItem_Var (v, ot) -> varoty fmt v ot
  | DeclItem_Tuple dis -> parens fmt (fun _ -> commasep fmt (decl_item fmt) dis)
  | DeclItem_BitTuple dbs -> parens fmt (fun _ -> commasep fmt (decl_bit fmt) dbs)
  | DeclItem_Wildcard ot -> varoty fmt dash_ident ot
  )

let rec stmt ?(short=false) (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      kw_var fmt;
      nbsp fmt;
      varnames fmt vs;
      nbsp fmt;
      colon fmt;
      nbsp fmt;
      ty fmt t;
      semicolon fmt
  | Stmt_VarDecl (is_constant, di, i, loc) ->
      (if is_constant then kw_let fmt else kw_var fmt);
      nbsp fmt;
      decl_item fmt di;
      nbsp fmt;
      colon_eq fmt;
      nbsp fmt;
      expr fmt i;
      semicolon fmt
  | Stmt_Assign (l, r, loc) ->
      lexpr fmt l;
      nbsp fmt;
      colon_eq fmt;
      nbsp fmt;
      expr fmt r;
      semicolon fmt
  | Stmt_TCall (f, tes, args, can_throw, loc) ->
      funname fmt f;
      throws fmt can_throw;
      if !show_type_params then (
        lbrace fmt;
        exprs fmt tes;
        rbrace fmt);
      parens fmt (fun _ -> exprs fmt args);
      semicolon fmt
  | Stmt_UCall (f, args, can_throw, loc) ->
      funname fmt f;
      throws fmt can_throw;
      parens fmt (fun _ -> pp_args fmt args);
      semicolon fmt;
  | Stmt_Return (e, loc) ->
      PP.fprintf fmt "%t %a;" kw_return expr e
  | Stmt_Assert (e, loc) ->
      kw_assert fmt;
      nbsp fmt;
      expr fmt e;
      semicolon fmt
  | Stmt_Throw (e, loc) ->
      kw_throw fmt;
      nbsp fmt;
      expr fmt e;
      semicolon fmt
  | Stmt_Block (ss, loc) ->
      PP.fprintf fmt "%t%a@,%t;"
        kw_begin
        (indented_block ~short) ss
        kw_end
  | Stmt_If (els, (e, el), loc) ->
      vbox fmt (fun _ ->
          if short then
              PP.fprintf fmt "if ... then ..."
          else begin
              let first = ref true in
              map fmt
                (fun (c, s, loc) ->
                  (if !first
                   then kw_if fmt
                   else begin cut fmt; kw_elsif fmt end);
                  first := false;
                  nbsp fmt;
                  expr fmt c;
                  nbsp fmt;
                  kw_then fmt;
                  indented_block fmt s)
                els;
              if e <> [] then begin
                cut fmt;
                kw_else fmt;
                indented_block fmt e
              end;
              PP.fprintf fmt "@,%t;" kw_endif
          end
      )
  | Stmt_Case (e, oty, alts, ob, loc) ->
      vbox fmt (fun _ ->
          kw_case fmt;
          nbsp fmt;
          expr fmt e;
          ( match oty with
          | Some t -> nbsp fmt; colon fmt; nbsp fmt; ty fmt t
          | None -> ()
          );
          nbsp fmt;
          kw_of fmt;
          if short then
              PP.fprintf fmt "..."
          else begin
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (AST.Alt_Alt (ps, oc, ss, loc)) ->
                      kw_when fmt;
                      nbsp fmt;
                      patterns fmt ps;
                      PP.pp_print_option
                        (fun _ c ->
                          nbsp fmt;
                          kw_if fmt;
                          nbsp fmt;
                          expr fmt c)
                        fmt oc;
                      nbsp fmt;
                      eq_gt fmt;
                      indented_block fmt ss)
                    alts;
                  PP.pp_print_option
                    (fun _ (b, bl) ->
                      cut fmt;
                      delimiter fmt "when _ =>";
                      indented_block fmt b)
                    fmt ob);
              PP.fprintf fmt "@,%t;" kw_endcase
          end
      )
  | Stmt_For (v, typ, f, dir, t, b, loc) ->
      PP.fprintf fmt "%t %a : %a := %a %a %a %t%a@,%t;"
        kw_for
        varname                 v
        ty                      typ
        expr                    f
        direction               dir
        expr                    t
        kw_do
        (indented_block ~short) b
        kw_endfor
  | Stmt_While (c, b, loc) ->
      PP.fprintf fmt "%t %a %t%a@,%t;"
        kw_while
        expr                    c
        kw_do
        (indented_block ~short) b
        kw_endwhile
  | Stmt_Repeat (b, c, pos, loc) ->
      PP.fprintf fmt "%t%a@,%t %a;"
        kw_repeat
        (indented_block ~short) b
        kw_until
        expr                    c
  | Stmt_Try (b, pos, cs, ob, loc) ->
      kw_try fmt;
      indented_block ~short fmt b;
      cut fmt;
      kw_catch fmt;
      if short then
          PP.fprintf fmt "..."
      else begin
          indented fmt (fun _ ->
              cutsep fmt
                (fun (AST.Catcher_Guarded (v, tc, b, loc)) ->
                  PP.fprintf fmt "%t %a : %a =>"
                    kw_when
                    varname v
                    tycon tc;
                  indented_block fmt b)
                cs;
              PP.pp_print_option
                (fun _ (b, bl) ->
                  cut fmt;
                  delimiter fmt "when _ =>";
                  indented_block fmt b)
                fmt ob);
      end;
      PP.fprintf fmt "@,%t;" kw_endtry
  )

and indented_block ?(short=false) (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if short then
      PP.fprintf fmt "..."
  else
      indented fmt (fun _ -> cutsep fmt (stmt fmt) xs)

let parameter (fmt : PP.formatter) (x : Ident.t * AST.ty option) : unit =
  let v, ot = x in
  varoty fmt v ot

let parameters (fmt : PP.formatter) (xs : (Ident.t * AST.ty option) list) :
    unit =
  commasep fmt (parameter fmt) xs

let formal (fmt : PP.formatter) (x : Ident.t * AST.ty * AST.expr option) : unit =
  let (v, t, od) = x in
  varty fmt v t;
  Option.iter (PP.fprintf fmt " %t %a" colon_eq expr) od

let formals (fmt : PP.formatter) (xs : (Ident.t * AST.ty * AST.expr option) list) : unit =
  commasep fmt (formal fmt) xs

let function_type (fmt : PP.formatter) (fty : AST.function_type) : unit =
  throws fmt fty.throws;
  braces fmt (fun _ -> parameters fmt fty.parameters);
  (if fty.use_array_syntax then
      brackets fmt (fun _ -> formals fmt fty.args)
  else
      parens fmt (fun _ -> formals fmt fty.args)
  );
  PP.pp_print_option
    (fun _ (v, ty) ->
      nbsp fmt;
      colon_eq fmt;
      nbsp fmt;
      varty fmt v ty)
    fmt fty.setter_arg;
  PP.fprintf fmt " -> %a" ty fty.rty

let ffi_direction (fmt : PP.formatter) (is_export : bool): unit =
  PP.fprintf fmt "%t" (if is_export then kw_export else kw_import)

let declaration ?(short=false) (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      ( match x with
      | Decl_BuiltinType (tc, loc) ->
          kw_underscore_builtin fmt;
          nbsp fmt;
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          semicolon fmt
      | Decl_Forward (tc, loc) ->
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          semicolon fmt
      | Decl_Record (tc, ps, fs, loc) ->
          kw_record fmt;
          nbsp fmt;
          tycon fmt tc;
          (if not (Utils.is_empty ps) then parens fmt (fun _ -> commasep fmt (varname fmt) ps));
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (f, t) ->
                      fieldname fmt f;
                      nbsp fmt;
                      colon fmt;
                      nbsp fmt;
                      ty fmt t;
                      comma fmt)
                    fs);
              cut fmt);
          semicolon fmt
      | Decl_Exception (tc, fs, loc) ->
          kw_record fmt;
          nbsp fmt;
          tycon fmt tc;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (f, t) ->
                      fieldname fmt f;
                      nbsp fmt;
                      colon fmt;
                      nbsp fmt;
                      ty fmt t;
                      comma fmt)
                    fs);
              cut fmt);
          semicolon fmt
      | Decl_Typedef (tc, [], Type_Bits(n,fs), loc) when fs <> [] ->
          PP.fprintf fmt "%t %a = {@,"
            kw_bitfield
            tycon tc;
          commasep fmt (regfield fmt) fs;
          PP.fprintf fmt "} : Bits(%a);@,"
            expr n
      | Decl_Typedef (tc, ps, t, loc) ->
          kw_type fmt;
          nbsp fmt;
          tycon fmt tc;
          (if not (Utils.is_empty ps) then parens fmt (fun _ -> commasep fmt (varname fmt) ps));
          nbsp fmt;
          kw_of fmt;
          nbsp fmt;
          ty fmt t;
          semicolon fmt
      | Decl_Enum (tc, es, loc) ->
          kw_enumeration fmt;
          nbsp fmt;
          tycon fmt tc;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          parens fmt (fun _ -> commasep fmt (varname fmt) es);
          semicolon fmt
      | Decl_Var (v, t, loc) ->
          kw_var fmt;
          nbsp fmt;
          varty fmt v t;
          semicolon fmt
      | Decl_Const (v, ot, e, loc) ->
          kw_constant fmt;
          nbsp fmt;
          varoty fmt v ot;
          nbsp fmt;
          colon_eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt
      | Decl_FunType (f, fty, loc) ->
          if fty.is_builtin then PP.fprintf fmt "__builtin ";
          kw_function fmt;
          nbsp fmt;
          funname fmt f;
          function_type fmt fty;
          semicolon fmt
      | Decl_FunDefn (f, fty, b, loc) ->
          if fty.is_builtin then PP.fprintf fmt "__builtin ";
          kw_function fmt;
          nbsp fmt;
          funname fmt f;
          function_type fmt fty;
          if not short then begin
              cut fmt;
              kw_begin fmt;
              indented_block ~short fmt b;
              cut fmt;
              kw_end fmt
          end
      | Decl_FunInstance (f, ps, loc) ->
          PP.fprintf fmt "%t %t %a %t {%a};"
            kw_optimize
            kw_function
            funname f
            kw_with
            (fun fmt -> commasep fmt (fun (p, sz) ->
              ( match sz with
              | None -> PP.fprintf fmt "%a => _" varname p
              | Some v -> PP.fprintf fmt "%a => %a" varname p Value.pp_value v
              ))) ps
      | Decl_FunFFI (nm, is_export, f, ps, loc) ->
          PP.fprintf fmt "%t %a %t \"%s\" = %a %t {%a};"
            kw_foreign
            ffi_direction is_export
            kw_function
            nm
            funname f
            kw_with
            (fun fmt -> commasep fmt (fun (p, v) ->
              PP.fprintf fmt "%a => %a" varname p Value.pp_value v
              )) ps
      | Decl_VarFFI (nm, is_export, v, loc) ->
          PP.fprintf fmt "%t %a %t \"%s\" = %a;"
            kw_foreign
            ffi_direction is_export
            kw_var
            nm
            varname v
      | Decl_TypeFFI (nm, is_export, t, loc) ->
          PP.fprintf fmt "%t %a %t \"%s\" = %a;"
            kw_foreign
            ffi_direction is_export
            kw_type
            nm
            ty t
      | Decl_Operator1 (op, fs, loc) ->
          kw_underscore_operator1 fmt;
          nbsp fmt;
          unop fmt op;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          funnames fmt fs;
          semicolon fmt
      | Decl_Operator2 (op, fs, loc) ->
          kw_underscore_operator2 fmt;
          nbsp fmt;
          binop fmt op;
          nbsp fmt;
          eq fmt;
          nbsp fmt;
          funnames fmt fs;
          semicolon fmt
      | Decl_Config (v, t, e, loc) ->
          kw_config fmt;
          nbsp fmt;
          varty fmt v t;
          nbsp fmt;
          colon_eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt
      | Decl_Use (p1, p2, loc) ->
          PP.fprintf fmt "%t %a %t %a;"
            kw_use
            funname p1
            kw_as
            funname p2
      )
  )

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  vbox fmt (fun _ ->
      map fmt
        (fun d ->
          declaration fmt d;
          cut fmt)
        xs)

(****************************************************************
 * End
 ****************************************************************)
