(****************************************************************
 * ASL format
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL format *)

module PP = Format
module AST = Asl_ast
module ColorT = Ocolor_types
open Format_utils
open Builtin_idents

(** Optionally show type parameters when printing ASL code *)
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

(* ASL delimiters *)

let amp (fmt : PP.formatter) : unit = delimiter fmt "&"
let amp_amp (fmt : PP.formatter) : unit = delimiter fmt "&&"
let bang (fmt : PP.formatter) : unit = delimiter fmt "!"
let bang_eq (fmt : PP.formatter) : unit = delimiter fmt "!="
let bar_bar (fmt : PP.formatter) : unit = delimiter fmt "||"
let caret (fmt : PP.formatter) : unit = delimiter fmt "^"
let colon (fmt : PP.formatter) : unit = delimiter fmt ":"
let dot (fmt : PP.formatter) : unit = delimiter fmt "."
let dot_dot (fmt : PP.formatter) : unit = delimiter fmt ".."
let eq (fmt : PP.formatter) : unit = delimiter fmt "="
let eq_eq (fmt : PP.formatter) : unit = delimiter fmt "=="
let eq_gt (fmt : PP.formatter) : unit = delimiter fmt "=>"
let gt (fmt : PP.formatter) : unit = delimiter fmt ">"
let gt_eq (fmt : PP.formatter) : unit = delimiter fmt ">="
let gt_gt (fmt : PP.formatter) : unit = delimiter fmt ">>"
let lbrace_lbrace (fmt : PP.formatter) : unit = delimiter fmt "{{"
let lt (fmt : PP.formatter) : unit = delimiter fmt "<"
let lt_eq (fmt : PP.formatter) : unit = delimiter fmt "<="
let lt_lt (fmt : PP.formatter) : unit = delimiter fmt "<<"
let lt_minus_gt (fmt : PP.formatter) : unit = delimiter fmt "<->"
let minus (fmt : PP.formatter) : unit = delimiter fmt "-"
let minus_minus_gt (fmt : PP.formatter) : unit = delimiter fmt "-->"
let plus (fmt : PP.formatter) : unit = delimiter fmt "+"
let plus_colon (fmt : PP.formatter) : unit = delimiter fmt "+:"
let minus_colon (fmt : PP.formatter) : unit = delimiter fmt "-:"
let star_colon (fmt : PP.formatter) : unit = delimiter fmt "*:"
let plus_plus (fmt : PP.formatter) : unit = delimiter fmt "++"
let rbrace_rbrace (fmt : PP.formatter) : unit = delimiter fmt "}}"
let semicolon (fmt : PP.formatter) : unit = delimiter fmt ";"
let slash (fmt : PP.formatter) : unit = delimiter fmt "/"
let star (fmt : PP.formatter) : unit = delimiter fmt "*"

(* keywords that are used as operators *)
let kw_and (fmt : PP.formatter) : unit = delimiter fmt "AND"
let kw_div_exact (fmt : PP.formatter) : unit = delimiter fmt "DIV"
let kw_divrm (fmt : PP.formatter) : unit = delimiter fmt "DIVRM"
let kw_in (fmt : PP.formatter) : unit = delimiter fmt "IN"
let kw_mod (fmt : PP.formatter) : unit = delimiter fmt "MOD"
let kw_not (fmt : PP.formatter) : unit = delimiter fmt "NOT"
let kw_or (fmt : PP.formatter) : unit = delimiter fmt "OR"
let kw_quot (fmt : PP.formatter) : unit = delimiter fmt "QUOT"
let kw_rem (fmt : PP.formatter) : unit = delimiter fmt "REM"
let kw_array (fmt : PP.formatter) : unit = keyword fmt "array"
let kw_as (fmt : PP.formatter) : unit = delimiter fmt "as"
let kw_assert (fmt : PP.formatter) : unit = keyword fmt "assert"
let kw_begin (fmt : PP.formatter) : unit = keyword fmt "begin"
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
let kw_enumeration (fmt : PP.formatter) : unit = keyword fmt "enumeration"
let kw_func (fmt : PP.formatter) : unit = keyword fmt "func"
let kw_getter (fmt : PP.formatter) : unit = keyword fmt "getter"
let kw_for (fmt : PP.formatter) : unit = keyword fmt "for"
let kw_if (fmt : PP.formatter) : unit = keyword fmt "if"
let kw_let (fmt : PP.formatter) : unit = keyword fmt "let"
let kw_of (fmt : PP.formatter) : unit = keyword fmt "of"
let kw_otherwise (fmt : PP.formatter) : unit = keyword fmt "otherwise"
let kw_record (fmt : PP.formatter) : unit = keyword fmt "record"
let kw_repeat (fmt : PP.formatter) : unit = keyword fmt "repeat"
let kw_return (fmt : PP.formatter) : unit = keyword fmt "return"
let kw_see (fmt : PP.formatter) : unit = keyword fmt "SEE"
let kw_setter (fmt : PP.formatter) : unit = keyword fmt "setter"
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
let kw_unknown (fmt : PP.formatter) : unit = keyword fmt "UNKNOWN"
let kw_until (fmt : PP.formatter) : unit = keyword fmt "until"
let kw_var (fmt : PP.formatter) : unit = keyword fmt "var"
let kw_when (fmt : PP.formatter) : unit = keyword fmt "when"
let kw_where (fmt : PP.formatter) : unit = keyword fmt "where"
let kw_while (fmt : PP.formatter) : unit = keyword fmt "while"
let kw_xor (fmt : PP.formatter) : unit = delimiter fmt "XOR"

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
  | Binop_Power -> caret fmt
  | Binop_Quot -> kw_quot fmt
  | Binop_Rem -> kw_rem fmt
  | Binop_Div_exact -> kw_div_exact fmt
  | Binop_Divrm -> kw_divrm fmt
  | Binop_Mod -> kw_mod fmt
  | Binop_ShiftL -> lt_lt fmt
  | Binop_ShiftR -> gt_gt fmt
  | Binop_BoolAnd -> amp_amp fmt
  | Binop_BoolOr -> bar_bar fmt
  | Binop_BoolIff -> lt_minus_gt fmt
  | Binop_BoolImplies -> minus_minus_gt fmt
  | Binop_BitOr -> kw_or fmt
  | Binop_BitXor -> kw_xor fmt
  | Binop_BitAnd -> kw_and fmt
  | Binop_Append -> plus_plus fmt
  | Binop_DUMMY ->
      PP.pp_print_string fmt "Binop_DUMMY" (* todo: throw an error? *)

let unop (fmt : PP.formatter) (x : AST.unop) : unit =
  match x with
  | Unop_Negate -> minus fmt
  | Unop_BoolNot -> bang fmt
  | Unop_BitsNot -> kw_not fmt

let throws (fmt : PP.formatter) (x : AST.can_throw) : unit =
  ( match x with
  | NoThrow -> ()
  | MayThrow -> PP.pp_print_string fmt "?"
  | AlwaysThrow -> PP.pp_print_string fmt "!"
  )

let rec ty (fmt : PP.formatter) (x : AST.ty) : unit =
  match x with
  | Type_Integer ocrs ->
      tycon fmt integer_ident;
      (match ocrs with None -> () | Some crs -> constraints fmt crs)
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
      brackets fmt (fun _ -> ixtype fmt ixty);
      nbsp fmt;
      kw_of fmt;
      nbsp fmt;
      ty fmt ety
  | Type_Tuple tys -> parens fmt (fun _ -> types fmt tys)

and types (fmt : PP.formatter) (tys : AST.ty list) : unit =
  commasep fmt (ty fmt) tys

and constraint_range (fmt : PP.formatter) (x : AST.constraint_range) : unit =
  match x with
  | Constraint_Single e -> expr fmt e
  | Constraint_Range (lo, hi) ->
      expr fmt lo;
      dot_dot fmt;
      expr fmt hi

and constraints (fmt : PP.formatter) (x : AST.constraint_range list) : unit =
  braces fmt (fun _ -> commasep fmt (constraint_range fmt) x)

and regfield (fmt : PP.formatter) (rf : AST.slice list * Ident.t) : unit =
  brackets fmt (fun _ -> commasep fmt (slice fmt) (fst rf));
  nbsp fmt;
  fieldname fmt (snd rf)

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
  commasep fmt (fun (c, e) -> Format.fprintf fmt "%a = %a" change c expr e) cs

and change (fmt : PP.formatter) (x : AST.change) : unit =
  ( match x with
  | Change_Field f -> fieldname fmt f
  | Change_Slices ss -> brackets fmt (fun _ -> slices fmt ss)
  )

and ixtype (fmt : PP.formatter) (x : AST.ixtype) : unit =
  match x with
  | Index_Enum tc -> tycon fmt tc
  | Index_Int sz -> expr fmt sz

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
      Format.fprintf fmt "(__let %a : %a = %a __in %a)"
        varname v
        ty t
        expr e
        expr b
  | Expr_Assert (e1, e2, loc) ->
      Format.fprintf fmt "(__assert %a __in %a)"
        expr e1
        expr e2
  | Expr_Binop (a, op, b) ->
      Format.fprintf fmt "(%a %a %a)"
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
      Format.fprintf fmt "(%a with { %a })"
        expr e
        changes cs
  | Expr_Record (tc, tes, fas) ->
      tycon fmt tc;
      if not (Utils.is_empty tes) then parens fmt (fun _ -> pp_args fmt tes);
      braces fmt (fun _ -> commasep fmt (field_assignment fmt) fas)
  | Expr_ArrayInit es ->
      Format.fprintf fmt "array (%a)"
        exprs es
  | Expr_In (e, p) ->
      Format.fprintf fmt "(%a IN %a)"
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
  | Expr_TApply (f, tes, es, can_throw) ->
      funname fmt f;
      if !show_type_params then braces fmt (fun _ -> exprs fmt tes);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt es)
  | Expr_UApply (f, args, can_throw) ->
      funname fmt f;
      throws fmt can_throw;
      parens fmt (fun _ -> pp_args fmt args)
  | Expr_Tuple es -> parens fmt (fun _ -> exprs fmt es)
  | Expr_Concat (ws, es) ->
      if !show_type_params then braces fmt (fun _ -> exprs fmt ws);
      brackets fmt (fun _ -> exprs fmt es)
  | Expr_Unop (op, e) ->
      Format.fprintf fmt "(%a %a)"
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
  | Expr_Lit v -> Value.pp_value fmt v
  | Expr_AsConstraint (e, c) ->
      parens fmt (fun _ ->
        expr fmt e;
        nbsp fmt;
        kw_as fmt;
        nbsp fmt;
        constraints fmt c
      )
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
  | (Some v, e) -> Format.fprintf fmt "%a = %a"
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
      eq fmt;
      nbsp fmt;
      expr fmt e

and pattern (fmt : PP.formatter) (x : AST.pattern) : unit =
  match x with
  | Pat_Lit v -> Value.pp_value fmt v
  | Pat_Const v -> varname fmt v
  | Pat_Wildcard -> minus fmt
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
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt es)
  | LExpr_ReadWrite (f, g, tes, es, can_throw) ->
      kw_underscore_readwrite fmt;
      nbsp fmt;
      funname fmt f;
      nbsp fmt;
      funname fmt g;
      if !show_type_params then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
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
      eq fmt;
      nbsp fmt;
      expr fmt i;
      semicolon fmt
  | Stmt_Assign (l, r, loc) ->
      lexpr fmt l;
      nbsp fmt;
      eq fmt;
      nbsp fmt;
      expr fmt r;
      semicolon fmt
  | Stmt_TCall (f, tes, args, can_throw, loc) ->
      funname fmt f;
      if !show_type_params then (
        lbrace_lbrace fmt;
        exprs fmt tes;
        rbrace_rbrace fmt);
      throws fmt can_throw;
      parens fmt (fun _ -> exprs fmt args);
      semicolon fmt
  | Stmt_UCall (f, args, can_throw, loc) ->
      funname fmt f;
      throws fmt can_throw;
      parens fmt (fun _ -> pp_args fmt args);
      semicolon fmt;
  | Stmt_Return (e, loc) ->
      PP.fprintf fmt "return %a;" expr e
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
      kw_begin fmt;
      indented_block ~short fmt ss;
      cut fmt;
      kw_end fmt
  | Stmt_If (els, (e, el), loc) ->
      vbox fmt (fun _ ->
          if short then
              Format.fprintf fmt "if ... then ..."
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
              cut fmt;
              kw_end fmt
          end
      )
  | Stmt_Case (e, oty, alts, ob, loc) ->
      vbox fmt (fun _ ->
          kw_case fmt;
          nbsp fmt;
          expr fmt e;
          ( match oty with
          | Some t -> nbsp fmt; colon fmt; ty fmt t
          | None -> ()
          );
          nbsp fmt;
          kw_of fmt;
          if short then
              Format.fprintf fmt "..."
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
                          kw_where fmt;
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
                      kw_otherwise fmt;
                      indented_block fmt b)
                    fmt ob);
              cut fmt;
              kw_end fmt
          end
      )
  | Stmt_For (v, typ, f, dir, t, b, loc) ->
      Format.fprintf fmt "for %a : %a = %a %a %a do%a@,end"
        varname                 v
        ty                      typ
        expr                    f
        direction               dir
        expr                    t
        (indented_block ~short) b
  | Stmt_While (c, b, loc) ->
      Format.fprintf fmt "while %a do%a@,end"
        expr                    c
        (indented_block ~short) b
  | Stmt_Repeat (b, c, pos, loc) ->
      Format.fprintf fmt "repeat%a@,until %a;"
        (indented_block ~short) b
        expr                    c
  | Stmt_Try (b, pos, cs, ob, loc) ->
      kw_try fmt;
      indented_block ~short fmt b;
      cut fmt;
      kw_catch fmt;
      if short then
          Format.fprintf fmt "..."
      else begin
          indented fmt (fun _ ->
              cutsep fmt
                (fun (AST.Catcher_Guarded (v, tc, b, loc)) ->
                  Format.fprintf fmt "when %a : %a =>"
                    varname v
                    tycon tc;
                  indented_block fmt b)
                cs;
              PP.pp_print_option
                (fun _ (b, bl) ->
                  cut fmt;
                  kw_otherwise fmt;
                  indented_block fmt b)
                fmt ob);
      end;
      cut fmt;
      kw_end fmt
  )

and indented_block ?(short=false) (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if short then
      Format.fprintf fmt "..."
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
  Option.iter (expr fmt) od

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
      eq fmt;
      nbsp fmt;
      varty fmt v ty)
    fmt fty.setter_arg;
  Format.fprintf fmt " => %a" ty fty.rty

let declaration ?(short=false) (fmt : PP.formatter) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      match x with
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
          braces fmt (fun _ ->
              indented fmt (fun _ ->
                  cutsep fmt
                    (fun (f, t) ->
                      fieldname fmt f;
                      nbsp fmt;
                      colon fmt;
                      nbsp fmt;
                      ty fmt t;
                      semicolon fmt)
                    fs);
              cut fmt);
          semicolon fmt
      | Decl_Exception (tc, fs, loc) ->
          kw_record fmt;
          nbsp fmt;
          tycon fmt tc;
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
                      semicolon fmt)
                    fs);
              cut fmt);
          semicolon fmt
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
          braces fmt (fun _ -> commasep fmt (varname fmt) es);
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
          eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt
      | Decl_BuiltinFunction (f, fty, loc) ->
          kw_underscore_builtin fmt;
          nbsp fmt;
          kw_func fmt;
          nbsp fmt;
          funname fmt f;
          function_type fmt fty;
          semicolon fmt
      | Decl_FunType (f, fty, loc) ->
          ( match (fty.is_getter_setter, fty.setter_arg) with
          | (false, _)      -> kw_func fmt
          | (true,  None)   -> kw_getter fmt
          | (true,  Some _) -> kw_setter fmt
          );
          nbsp fmt;
          funname fmt f;
          function_type fmt fty;
          semicolon fmt
      | Decl_FunDefn (f, fty, b, loc) ->
          ( match (fty.is_getter_setter, fty.setter_arg) with
          | (false, _)      -> kw_func fmt
          | (true,  None)   -> kw_getter fmt
          | (true,  Some _) -> kw_setter fmt
          );
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
          eq fmt;
          nbsp fmt;
          expr fmt e;
          semicolon fmt)

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
