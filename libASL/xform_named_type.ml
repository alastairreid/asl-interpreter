(****************************************************************
 * ASL named type transform
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Identset

let find_typedefs (ds : AST.declaration list) :
    (Ident.t list * AST.ty) Bindings.t =
  List.fold_left
    (fun map d ->
      match d with
      | AST.Decl_Typedef (tc, ps, ty, loc) -> Bindings.add tc (ps, ty) map
      | _ -> map)
    Bindings.empty ds

class replace_named_types (typedefs : (Ident.t list * AST.ty) Bindings.t) =
  object
    inherit Asl_visitor.nopAslVisitor

    method! vtype x =
      let rec replace (ty : AST.ty) : AST.ty =
        match ty with
        | Type_Constructor (tc, es) -> (
            match Bindings.find_opt tc typedefs with
            | Some (ps, ty') ->
                let bs = mk_bindings (List.combine ps es) in
                replace (subst_type bs ty')
            | None -> ty)
        | _ -> ty
      in
      ChangeTo (replace x)
  end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let typedefs = find_typedefs ds in
  let replacer = new replace_named_types typedefs in
  List.map (Asl_visitor.visit_decl (replacer :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_named_type
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_named_type" [] [] [] "Eliminate typedefs" cmd

(****************************************************************
 * End
 ****************************************************************)
