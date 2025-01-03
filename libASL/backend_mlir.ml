(****************************************************************
 * ASL to MLIR backend
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL to MLIR backend *)

module AST = Asl_ast
module FMT = Asl_fmt
module PP = Format
module V = Value
open Asl_utils
open Format_utils
open Utils

let commasep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    ~pp_sep:(fun fmt' _ -> PP.pp_print_string fmt' ", ")
    pp
    fmt
    xs

let cutsep (pp : PP.formatter -> 'a -> unit) (fmt : PP.formatter) (xs : 'a list) : unit =
  PP.pp_print_list
    pp
    fmt
    xs

let ident (fmt : PP.formatter) (x : Ident.t) : unit = Ident.pp fmt x

let locals = new Asl_utils.nameSupply "%"

let valueLit (loc : Loc.t) (fmt : PP.formatter) (x : Value.value) : unit =
  ( match x with
(*
  | VInt v    -> Runtime.int_literal fmt v
  | VIntN v   -> Runtime.sintN_literal fmt v
  | VBits v   -> Runtime.bits_literal fmt v
  | VString v -> PP.pp_print_string fmt ("\"" ^ String.escaped v ^ "\"")
*)
  | _ -> raise (InternalError (loc, "valueLit", (fun fmt -> Value.pp_value fmt x), __LOC__))
  )

let rec apply (loc : Loc.t) (fmt : PP.formatter) (f : Ident.t) (ps : AST.expr list) (args : AST.expr list) : Ident.t =
  let pvs = List.map (expr loc fmt) ps in
  let avs = List.map (expr loc fmt) args in
  let t = locals#fresh in
  PP.fprintf fmt "%a = %a<%a>(%a)@;"
    Ident.pp t
    Ident.pp f
    (commasep Ident.pp) pvs
    (commasep Ident.pp) avs;
  t

and expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : Ident.t =
  ( match x with
  | Expr_Lit v ->
      let t = locals#fresh in
      PP.fprintf fmt "%a = %a@."
        Ident.pp t
        (valueLit loc) v;
      t
  | Expr_Var v ->
      (* todo: if v is a global, it needs to be copied into a local *)
      v
  | Expr_TApply (f, ps, args, throws) ->
      apply loc fmt f ps args
  | _ ->
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, "expression", pp))
  )

(* Todo: the following is a hack that can only cope with a few simple kinds of expression
 * that appear in types but this is definitely not sufficient to express all ASL types.
 *
 * The problem we face is that an expression like "N + 1" would normally
 * be flattened to
 *
 *   %t0 = 1
 *   %t1 = asl.add_int %N %t0
 *
 * and it is not clear where that sequence of statements should go in the places
 * where types can occur
 *)
and simple_expr (loc : Loc.t) (fmt : PP.formatter) (x : AST.expr) : unit =
  ( match x with
  | Expr_Lit v -> valueLit loc fmt v
  | Expr_Var v -> Ident.pp fmt v
  | _ ->
      let msg = Format.asprintf "simple_expr: overly complex expression" in
      let pp fmt = FMT.expr fmt x in
      raise (Error.Unimplemented (loc, msg, pp))
  )

and simple_exprs (loc : Loc.t) (fmt : PP.formatter) (xs : AST.expr list) : unit =
  commasep (simple_expr loc) fmt xs

and constraint_range (loc : Loc.t) (fmt : PP.formatter) (x : AST.constraint_range) : unit =
  ( match x with
  | Constraint_Single e -> simple_expr loc fmt e
  | Constraint_Range (lo, hi) ->
      PP.fprintf fmt "%a:%a"
        (simple_expr loc) lo
        (simple_expr loc) hi
  )

and constraints (loc : Loc.t) (fmt : PP.formatter) (x : AST.constraint_range list) : unit =
  commasep (constraint_range loc) fmt x

and pp_type (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty) : unit =
  ( match x with
  | Type_Bits (e, _) -> PP.fprintf fmt "!asl.bits<%a>" (simple_expr loc) e
  | Type_Constructor (tc, ps) ->
      PP.fprintf fmt "%a<%a>"
        ident tc
        (simple_exprs loc) ps
  | Type_Integer ocrs ->
      PP.fprintf fmt "!asl.int<%a>"
        (PP.pp_print_option (constraints loc)) ocrs
  | _ ->
      let pp fmt = FMT.ty fmt x in
      raise (Error.Unimplemented (loc, "type", pp))
  )

let pp_type_option (loc : Loc.t) (fmt : PP.formatter) (x : AST.ty option) : unit =
  PP.pp_print_option
    ~none:(fun fmt _ -> PP.fprintf fmt "void")
    (fun fmt t -> pp_type loc fmt t)
    fmt
    x

let declitem (loc : Loc.t) (fmt : PP.formatter) (x : AST.decl_item) =
  ( match x with
  | DeclItem_Var (v, Some t) ->
      PP.fprintf fmt "%a : %a;@,"
        Ident.pp v
        (pp_type loc) t
  | _ ->
      let pp fmt = FMT.decl_item fmt x in
      raise (Error.Unimplemented (loc, "declitem", pp))
  )

let decl (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_VarDeclsNoInit (vs, t, loc) ->
      List.iter (fun v ->
        PP.fprintf fmt "%a : %a;@,"
        Ident.pp v
        (pp_type loc) t
      )
      vs
  | Stmt_VarDecl (di, i, loc) | Stmt_ConstDecl (di, i, loc) -> declitem loc fmt di
  | _ -> ()
  )

let return_type = ref (AST.Type_Tuple [])

let rec stmt (fmt : PP.formatter) (x : AST.stmt) : unit =
  ( match x with
  | Stmt_FunReturn (e, loc) ->
      let t = expr loc fmt e in
      PP.fprintf fmt "asl.return %a : %a@."
        Ident.pp t
        (pp_type loc) !return_type
  | Stmt_ProcReturn loc ->
      PP.fprintf fmt "asl.return@."
  | _ ->
      let pp fmt = FMT.stmt fmt x in
      raise (Error.Unimplemented (Loc.Unknown, "statement", pp))
  )

and indented_block (fmt : PP.formatter) (xs : AST.stmt list) : unit =
  if xs <> [] then begin
    indented fmt (fun _ ->
      map fmt (decl fmt) xs;
      cutsep stmt fmt xs)
  end

let formal_param (loc : Loc.t) (fmt : PP.formatter) (x : Ident.t * AST.ty option) : unit =
  let (v, ot) = x in
  PP.fprintf fmt "%a : %a"
    Ident.pp v
    (pp_type loc) (Option.value ~default:(AST.Type_Integer(None)) ot)

let formal_arg (loc : Loc.t) (fmt : PP.formatter) (x : Ident.t * AST.ty) : unit =
  let (v, t) = x in
  PP.fprintf fmt "%a : %a"
    Ident.pp v
    (pp_type loc) t

let declaration (fmt : PP.formatter) ?(is_extern : bool option) (x : AST.declaration) : unit =
  vbox fmt (fun _ ->
      ( match x with
      | Decl_BuiltinType _
      | Decl_BuiltinFunction _
      | Decl_Forward _
      | Decl_Operator1 _
      | Decl_Operator2 _
      | Decl_FunType _
        -> ()
      | Decl_FunDefn (f, fty, b, loc) ->
          PP.fprintf fmt "asl.func @%a<%a>(%a) -> %a {@."
            Ident.pp f
            (commasep (formal_param loc)) fty.parameters
            (commasep (formal_arg loc)) fty.args
            (pp_type_option loc) fty.rty;
          locals#reset;
          return_type := Option.value ~default:(AST.Type_Tuple []) fty.rty;
          indented_block fmt b;
          PP.fprintf fmt "}@.@."
      | _ ->
          ( match Asl_utils.decl_name x with
          | Some nm -> PP.fprintf fmt "# skipping %a\n" Ident.pp nm
          | None -> ()
          )
      ))

let declarations (fmt : PP.formatter) (xs : AST.declaration list) : unit =
  PP.fprintf fmt "builtin.module {@,";
  vbox fmt (fun _ -> map fmt (declaration fmt) xs);
  PP.fprintf fmt "}@,"

(****************************************************************
 * Command: :generate_mlir
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let decls = !Commands.declarations in
    declarations Format.std_formatter decls;
    true
  in

  let flags = Arg.align [
      ]
  in
  Commands.registerCommand "generate_mlir" flags [] [] "Generate MLIR" cmd

(****************************************************************
 * End
 ****************************************************************)
