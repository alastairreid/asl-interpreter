(****************************************************************
 * ASL let-hoisting transform
 *
 * Lifts let-bindings and expression-asserts as high as possible
 * out of expressions.
 *
 * The main restrictions on lifting let-bindings/asserts are
 *
 * - not lifting the let-binding/assert out of a while guard condition
 *   in case the let-binding depends on a variable that is modified
 *   by the while loop or has side-effects or the assertion fails.
 *
 * - not lifting the let-binding/assert out of elsif conditions, case-alternative
 *   guards or catcher-guards in case the let-binding is evaluated
 *   earlier than other conditions/guards in the statement.
 *
 * - not lifting the let-binding/assert out of the body of an if-expression
 *   or out of the second argument of && or || in case the let-binding
 *   contains a side-effect or can throw an exception or can
 *   trigger a runtime error.
 *
 * When these restrictions do not limit how high a binding/assert
 * can be lifted, they normally turn into assignments prior to the
 * statement that contains the expression.
 *
 * Copyright (C) 2025-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_visitor
open Builtin_idents

class hoist_lets (ds : AST.declaration list option) = object (self)
    inherit nopAslVisitor

    val mutable bindings : Asl_utils.binding list = []
    val mutable checks : Asl_utils.check list = []

    (* Hoist let-expressions and assertions out of an expression *)
    method hoist_lets_out_of_expression (x : AST.expr) : (Asl_utils.binding list * Asl_utils.check list * AST.expr) =
      let old_bindings = bindings in
      let old_checks = checks in
      bindings <- [];
      checks <- [];
      let x' = visit_expr (self :> aslVisitor) x in
      let lets = bindings in
      let asserts = checks in
      bindings <- old_bindings;
      checks <- old_checks;
      (lets, asserts, x')

    (* Hoist let-expressions to the top of expression *)
    method hoist_lets_to_expression_top (x : AST.expr) : AST.expr =
      let (lets, asserts, x') = self#hoist_lets_out_of_expression x in
      Asl_utils.mk_let_exprs lets (Asl_utils.mk_assert_exprs asserts x')

    method! vexpr x =
      ( match x with
      | Expr_Let (x, ty, e1, e2) ->
          let e1' = visit_expr (self :> aslVisitor) e1 in
          let e2' = visit_expr (self :> aslVisitor) e2 in
          bindings <- (x, ty, e1') :: bindings;
          Visitor.ChangeTo e2'
      | Expr_Assert (e1, e2, loc) ->
          let e1' = visit_expr (self :> aslVisitor) e1 in
          let e2' = visit_expr (self :> aslVisitor) e2 in
          checks <- (e1', loc) :: checks;
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
          let (lets, asserts, c') = self#hoist_lets_out_of_expression c in
          let t' = visit_stmts (self :> aslVisitor) t in
          let els' = Visitor.mapNoCopy (visit_s_elsif (self :> aslVisitor)) els in
          let e' = visit_stmts (self :> aslVisitor) e in
          if Utils.is_empty lets && Utils.is_empty asserts && c == c' && t == t' && els == els' && e == e' then (
            Visitor.SkipChildren
          ) else (
            let lets' = Asl_utils.mk_assigns loc lets in
            let asserts' = Asl_utils.mk_assert_stmts asserts in
            Visitor.ChangeTo (lets' @ asserts' @ [Stmt_If (c', t', els', (e', el), loc)])
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
          let (lets, asserts, c') = self#hoist_lets_out_of_expression c in
          if Utils.is_empty lets && Utils.is_empty asserts && c == c' && b == b' then (
            Visitor.SkipChildren
          ) else (
            let lets' = Asl_utils.mk_assigns loc lets in
            let asserts' = Asl_utils.mk_assert_stmts asserts in
            Visitor.ChangeTo [Stmt_Repeat (b' @ lets' @ asserts', c', pos, loc)]
          )
      | _ ->
          let rebuild (xs : AST.stmt list) : AST.stmt list =
            let loc = Loc.Unknown in
            let lets = bindings in
            let asserts = checks in
            let lets' = Asl_utils.mk_assigns loc lets in
            let asserts' = Asl_utils.mk_assert_stmts asserts in
            bindings <- [];
            checks <- [];
            lets' @ asserts' @ xs
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
