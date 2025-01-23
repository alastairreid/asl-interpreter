(****************************************************************
 * ASL let-hoisting transform
 *
 * Lifts let-bindings as high as possible out of expressions.
 *
 * The main restrictions on lifting let-bindings are
 *
 * - not lifting the let-binding out of a while guard condition
 *   in case the let-binding depends on a variable that is modified
 *   by the while loop or has side-effects.
 *
 * - not lifting the let-binding out of elsif conditions, case-alternative
 *   guards or catcher-guards in case the let-binding is evaluated
 *   earlier than other conditions/guards in the statement.
 *
 * - not lifting the let-binding out of the body of an if-expression
 *   or out of the second argument of && or || in case the let-binding
 *   contains a side-effect or can throw an exception or can
 *   trigger a runtime error.
 *
 * When these restrictions do not limit how high a binding can be lifted,
 * they normally turn into assignments prior to the statement that
 * contains the expression.
 *
 * Copyright (C) 2025-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_visitor
open Builtin_idents

type binding = (Ident.t * AST.ty * AST.expr)

class hoist_lets (ds : AST.declaration list option) = object (self)
    inherit nopAslVisitor

    val mutable bindings : binding list = []

    (* Hoist let-expressions out of an expression *)
    method hoist_lets_out_of_expression (x : AST.expr) : (binding list * AST.expr) =
      let old = bindings in
      bindings <- [];
      let x' = visit_expr (self :> aslVisitor) x in
      let binds = bindings in
      bindings <- old;
      (binds, x')

    (* Hoist let-expressions to the top of expression *)
    method hoist_lets_to_expression_top (x : AST.expr) : AST.expr =
      let (lets, x') = self#hoist_lets_out_of_expression x in
      Asl_utils.mk_let_exprs lets x'

    method! vexpr x =
      ( match x with
      | Expr_Let (x, ty, e1, e2) ->
          let e1' = visit_expr (self :> aslVisitor) e1 in
          let e2' = visit_expr (self :> aslVisitor) e2 in
          bindings <- (x, ty, e1') :: bindings;
          Visitor.ChangeTo e2'
      | Expr_TApply (f, [], [a; b], NoThrow) when Ident.in_list f [and_bool; or_bool; implies_bool] ->
          let a' = visit_expr (self :> aslVisitor) a in
          let b' = self#hoist_lets_to_expression_top b in
          if a == a' && b == b' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (Expr_TApply (f, [], [a'; b'], NoThrow))
          )
      | Expr_If (c, t, els, e) ->
          let c' = visit_expr (self :> aslVisitor) c in
          let t' = self#hoist_lets_to_expression_top t in
          let els' = Visitor.mapNoCopy (visit_e_elsif (self :> aslVisitor)) els in
          let e' = self#hoist_lets_to_expression_top e in
          if c == c' && t == t' && els == els' && e == e' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (Expr_If (c', t', els', e'))
          )
      | _ ->
          Visitor.DoChildren
      )

    method! vstmt x =
      assert (Utils.is_empty bindings);
      ( match x with
      | Stmt_If (c, t, els, (e, el), loc) ->
          let (lets, c') = self#hoist_lets_out_of_expression c in
          let t' = visit_stmts (self :> aslVisitor) t in
          let els' = Visitor.mapNoCopy (visit_s_elsif (self :> aslVisitor)) els in
          let e' = visit_stmts (self :> aslVisitor) e in
          if Utils.is_empty lets && c == c' && t == t' && els == els' && e == e' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (Asl_utils.mk_assigns loc lets @ [Stmt_If (c', t', els', (e', el), loc)])
          )
      | Stmt_While (c, b, loc) ->
          let c' = self#hoist_lets_to_expression_top c in
          let b' = visit_stmts (self :> aslVisitor) b in
          if c == c' && b == b' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo [Stmt_While (c', b', loc)]
          )
      | Stmt_Repeat (b, c, pos, loc) ->
          let b' = visit_stmts (self :> aslVisitor) b in
          let (lets, c') = self#hoist_lets_out_of_expression c in
          if Utils.is_empty lets && c == c' && b == b' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo [Stmt_Repeat (b' @ Asl_utils.mk_assigns loc lets, c', pos, loc)]
          )
      | _ ->
          let rebuild (xs : AST.stmt list) : AST.stmt list =
            let loc = Loc.Unknown in
            let lets = bindings in
            bindings <- [];
            (Asl_utils.mk_assigns loc lets) @ xs
          in
          Visitor.ChangeDoChildrenPost ([x], rebuild)
      )

    method !vs_elsif x =
      ( match x with
      | S_Elsif_Cond (c, b, loc) ->
          let c' = self#hoist_lets_to_expression_top c in
          let b' = visit_stmts (self :> aslVisitor) b in
          if c == c' && b == b' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (S_Elsif_Cond (c', b', loc))
          )
      )

    method !ve_elsif x =
      ( match x with
      | E_Elsif_Cond (c, e) ->
          let c' = self#hoist_lets_to_expression_top c in
          let e' = self#hoist_lets_to_expression_top e in
          if c == c' && e == e' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (E_Elsif_Cond (c', e'))
          )
      )

    method !valt x =
      ( match x with
      | Alt_Alt (ps, oc, b, loc) ->
          let oc' = Visitor.mapOptionNoCopy self#hoist_lets_to_expression_top oc in
          let b' = visit_stmts (self :> aslVisitor) b in
          if oc == oc' && b == b' then (
            Visitor.SkipChildren
          ) else (
            Visitor.ChangeTo (Alt_Alt (ps, oc', b', loc))
          )
      )
end

let xform_decls (ds : AST.declaration list) : AST.declaration list =
  let transformer = new hoist_lets (Some ds) in
  List.map (Asl_visitor.visit_decl (transformer :> Asl_visitor.aslVisitor)) ds

(****************************************************************
 * Command: :xform_hoist_lets
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := xform_decls !Commands.declarations;
    true
  in
  let options = [] in
  Commands.registerCommand "xform_hoist_lets" options [] [] "Hoist let-expressions" cmd

(****************************************************************
 * End
 ****************************************************************)
