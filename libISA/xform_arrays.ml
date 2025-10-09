(****************************************************************
 * ISA array wrapping transform
 *
 * Wraps arrays in records
 *
 * This makes code generation easier because it allows
 * us to copy arrays using assignment.
 *
 * Copyright (C) 2025-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Isa_ast

(****************************************************************
 * Wrapper generation code
 ****************************************************************)

(* To simplify code generation for array assignment,
 * we wrap all arrays in records.
 * Doing this correctly requires that we use the same
 * record for every occurence of the same type.
 *)
module Ty = struct
  type t = AST.ty
  let compare (x : t) (y : t) : int = Stdlib.compare x y
end

module TypeMap = Map.Make(Ty) (* cache types *)

let typenames = new Isa_utils.nameSupply "__Type_"

let wrapper_field = Ident.mk_ident "data"

(****************************************************************
 * Transform
 ****************************************************************)

(* It is important that equivalent types are transformed to the same
 * record so we normalize range types like "{1, 2, 7}" to "{1..7}".
 *
 * Returns the input set unchanged if unable to infer the min and max value.
 *)
let simplify_ranges (ranges : AST.set_range list) : AST.set_range list =
  let range_min_max (r : AST.set_range) : (AST.expr option * AST.expr option) =
    ( match r with
    | Set_Single x -> (Some x, Some x)
    | Set_Range (lo, hi) -> (lo, hi)
    )
  in
  let min (x : AST.expr) (y : AST.expr) : AST.expr option =
    ( match (x, y) with
    | (Expr_Lit (VInt x'), Expr_Lit (VInt y')) -> Some (Expr_Lit (VInt (Z.min x' y')))
    | (_, _) -> None
    )
  in
  let max (x : AST.expr) (y : AST.expr) : AST.expr option =
    ( match (x, y) with
    | (Expr_Lit (VInt x'), Expr_Lit (VInt y')) -> Some (Expr_Lit (VInt (Z.max x' y')))
    | (_, _) -> None
    )
  in
  let merge (x : (AST.expr option * AST.expr option)) (y : (AST.expr option * AST.expr option)) : (AST.expr option * AST.expr option) =
    let (xl, xh) = x in
    let (yl, yh) = y in
    let l = Option.join (Utils.map2_option min xl yl) in
    let h = Option.join (Utils.map2_option max xh yh) in
    (l, h)
  in
  ( match ranges with
  | [] -> []
  | (r :: rs) ->
      let lohi = ref (range_min_max r) in
      List.iter (fun r -> lohi := merge !lohi (range_min_max r)) rs;
      let (lo, hi) = !lohi in
      [Set_Range (lo, hi)]
  )

class replaceArrayClass = object (self)
  inherit Isa_visitor.nopIsaVisitor
  val mutable typemap : Ident.t TypeMap.t = TypeMap.empty

  method mkTypeName (x : AST.ty) : Ident.t =
    ( match TypeMap.find_opt x typemap with
    | Some r -> r
    | None ->
          let r = typenames#fresh in
          typemap <- TypeMap.add x r typemap;
          r
    )

  method mkTypeWrappers : AST.declaration list =
    List.map
      (fun (t, nm) -> AST.Decl_Record (nm, [], [(wrapper_field, t)], Loc.Unknown))
      (TypeMap.bindings typemap)

  method! vtype (x : AST.ty) =
    ( match x with
    | Type_Array _ ->
        ChangeDoChildrenPost (x, fun x' ->
          let tc = self#mkTypeName x' in
          AST.Type_Constructor (tc, []))
    | Type_Integer (Some ranges) ->
        ChangeTo (Type_Integer (Some (simplify_ranges ranges)))
    | _ -> DoChildren
    )

  method! vexpr (x : AST.expr) =
    ( match x with
    | Expr_Array (a, ix) ->
        let a' = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) a in
        let ix' = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) ix in
        ChangeTo (AST.Expr_Array (AST.Expr_Field (a', wrapper_field), ix'))
    | Expr_ArrayInit (t, es) ->
        let t' = Isa_visitor.visit_type (self :> Isa_visitor.isaVisitor) t in
        let es' = Isa_visitor.visit_exprs (self :> Isa_visitor.isaVisitor) es in
        let e' = AST.Expr_ArrayInit (t', es') in
        let n = AST.Expr_Lit (VInt (Z.of_int (List.length es'))) in
        let aty = AST.Type_Array (AST.Index_Int n, t') in
        let tc = self#mkTypeName aty in
        ChangeTo (AST.Expr_Record (tc, [], [(wrapper_field, e')]))
    | _ -> DoChildren
    )

  method! vlexpr (x : AST.lexpr) =
    ( match x with
    | LExpr_Array (a, ix) ->
        let a' = Isa_visitor.visit_lexpr (self :> Isa_visitor.isaVisitor) a in
        let ix' = Isa_visitor.visit_expr (self :> Isa_visitor.isaVisitor) ix in
        ChangeTo (AST.LExpr_Array (AST.LExpr_Field (a', wrapper_field), ix'))
    | _ -> DoChildren
    )
end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let replacer = new replaceArrayClass in
  let ds' = List.map (Isa_visitor.visit_decl (replacer :> Isa_visitor.isaVisitor)) ds in
  let decls = replacer#mkTypeWrappers in
  ds' @ decls

(****************************************************************
 * Command: :xform_arrays
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  Commands.registerCommand "xform_arrays" [] [] [] "Wrap array accesses" cmd

(****************************************************************
 * End
 ****************************************************************)
