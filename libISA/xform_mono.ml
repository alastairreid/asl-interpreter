(****************************************************************
 * ISA function monomorphization transform
 *
 * Copyright (C) 2022-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** ISA function monomorphization transform *)

module AST = Isa_ast
open Identset
open Visitor
open Isa_visitor
open Utils
open Isa_utils

(* Don't warn about debugging print code *)
[@@@warning "-32"]

let enable_auto_case_split = ref false
let verbose = ref false

let const_expr (x : AST.expr) : Value.value option =
  ( match x with
  | Expr_Lit v -> Some v
  | _ -> None
  )

type param_request = (Ident.t * bool * Value.value option) list
type arg_request = (Ident.t * bool * Value.value option) list
type request = (param_request * arg_request)

let ppRequest1 (fmt : Format.formatter) (rs : (Ident.t * bool * Value.value option) list) : unit =
  let pp' ((var, required, ov) : (Ident.t * bool * Value.value option)) : unit =
    if required then begin
      Ident.pp fmt var;
      ( match ov with
      | None -> ()
      | Some v -> Format.fprintf fmt "=>%a" Value.pp_value v
      )
    end else begin
      Format.fprintf fmt "_"
    end
  in
  Format_utils.commasep fmt pp' rs

let ppRequest (fmt : Format.formatter) (r : request) =
  Format.fprintf fmt "{%a}(%a)"
    ppRequest1 (fst r)
    ppRequest1 (snd r)

module Instance = struct
  type t = Ident.t * (Ident.t * Value.value) list

  let compare' (x : (Ident.t * Value.value)) (y : (Ident.t * Value.value)) : int =
      ( match (snd x, snd y) with
      | (VInt x', VInt y') -> Z.compare x' y'
      | (VIntN x', VIntN y') -> Z.compare x'.v y'.v
      | (VBits x', VBits y') -> Z.compare x'.v y'.v
      | _ -> -1
      )

  let compare (x : t) (y : t) : int =
    Ident.compare (fst x) (fst y) <?> (List.compare compare', snd x, snd y)

  let ppParam (fmt : Format.formatter) (p : (Ident.t * Value.value)) : unit =
    Format.fprintf fmt "%a => %a"
      Ident.pp (fst p)
      Value.pp_value (snd p)

  let ppParams (fmt : Format.formatter) (ps : (Ident.t * Value.value) list) : unit =
    Format_utils.commasep fmt (ppParam fmt) ps

  let pp (fmt : Format.formatter) (x : t) : unit =
    Format.fprintf fmt "Instance{%a, %a}"
      Ident.pp (fst x)
      ppParams (snd x)

  (* Generate a new name for a monomorphic instance of a function or type *)
  let mk_monomorphic_name (loc : Loc.t) (i : t) : Ident.t =
    let (f, sizes) = i in
    assert (List.length sizes > 0);
    (* Escape characters of printed values that are not legal in identifiers *)
    let escape_minus s = Str.global_replace (Str.regexp "-") "minus" s in
    let escape_tick s = Str.global_replace (Str.regexp "'") "_" s in
    let escape s = s |> escape_minus |> escape_tick in
    let suffices =
      List.map
        (fun (nm, sz) -> Ident.to_string nm ^ "_" ^ escape (Value.string_of_value sz))
        sizes
    in
    Ident.add_suffix f ~suffix:(String.concat "_" suffices)
end


module Instances = Map.Make (Instance)

let ( let* ) = Option.bind

(* Class for recursively collecting identifiers from parameters for call
   expressions *)
class param_collector =
  object (self)
    inherit Isa_visitor.nopIsaVisitor

    val mutable params = IdentSet.empty

    method add_param (p : AST.expr) =
      match p with
      | Expr_Var id -> params <- IdentSet.add id params;
      | _           -> ()

    method! vexpr e =
      match e with
      | Expr_TApply (_, params, _, _) ->
          List.iter self#add_param params;
          DoChildren
      | _ -> DoChildren

    method get_params =
      IdentSet.elements params
  end

class monoClass
    (genv : Eval.GlobalEnv.t)
    (global_type_info : AST.ty Bindings.t)
    (type_decl_lookup_table : AST.declaration IdentTable.t)
    (fun_decl_lookup_table : AST.declaration list IdentTable.t)
    (requests : request list IdentTable.t)
    (ds : AST.declaration list) =
  object (self)
    inherit nopIsaVisitor
    val mutable instances : AST.declaration list Instances.t = Instances.empty

    val mutable local_type_info : AST.ty Bindings.t = Bindings.empty

    method update_local_type_info (t : Ident.t) (ty : AST.ty) : unit =
      local_type_info <- Bindings.add t ty local_type_info

    method clear_local_type_info () : unit =
      local_type_info <- Bindings.empty

    method get_type (i : Ident.t) : AST.ty option =
      orelse_option
        (Bindings.find_opt i global_type_info)
        (fun _ -> Bindings.find_opt i local_type_info)

    method getInstances = List.map snd (Instances.bindings instances)

    (* Return the constraints for a given parameter identifier, if constraints
       exist and they are of type integer. *)
    method param_to_constraints (p : Ident.t) : AST.set_range list option =
      match self#get_type p with
      | Some (AST.Type_Integer cs) -> cs
      | _ -> None

    (* Create a 'when' branch with statement [stmt] using the constraints [cs] as
       pattern. *)
    method constraints_to_when_branch (loc : Loc.t) (stmt : AST.stmt) (cs : AST.set_range list)
      : AST.alt option =
      let to_pattern (c : AST.set_range) : AST.pattern option =
        match c with
        | Set_Single (Expr_Lit s) -> Some (Pat_Lit s)
        | _ -> None
      in
      let* ps = flatten_map_option to_pattern cs in
      Some (AST.Alt_Alt ([AST.Pat_Tuple ps], None, [stmt], loc))

    (* Create a 'Stmt_Case' from [stmt], creating when-branches for all
       combinations of parameter constraints using parameters [params]. *)
    method build_case_stmt (loc : Loc.t) (stmt : AST.stmt) (params : Ident.t list)
      : AST.stmt option =
      if !enable_auto_case_split then (
        let* constraints = flatten_map_option self#param_to_constraints params in
        let constraints_combinations = cartesian_product constraints in
        let* when_branches = flatten_map_option (self#constraints_to_when_branch loc stmt) constraints_combinations in
        let params' = List.map (fun p -> AST.Expr_Var p) params in
        let tys = List.map (fun p -> Option.value (self#get_type p) ~default:type_integer) params in
        Some (AST.Stmt_Case ((Expr_Tuple params'), Some (AST.Type_Tuple tys), when_branches, None, loc))
      ) else (
        None
      )

    (* Collect the parameters from a list of expressions [e]. Also add the
       parameters from the list of [params]. *)
    method collect_params (e : AST.expr list) (params : AST.expr list)
      : Ident.t list =
      let collector = new param_collector in
      let _ = Isa_visitor.visit_exprs (collector :> Isa_visitor.isaVisitor) e in
      List.iter collector#add_param params;
      collector#get_params


    method mk_monomorphic_decl (genv : Eval.GlobalEnv.t) (instance : Instance.t)
        (d : AST.declaration) : AST.declaration option =
      let (_, values) = instance in
      let nm = Instance.mk_monomorphic_name Loc.Unknown instance in
      let env = Xform_constprop.mkEnv genv values in
      ( match d with
      | Decl_Typedef (_, _, ty, loc) ->
          let ty' = Xform_constprop.xform_ty env ty in
          let d' = AST.Decl_Typedef (nm, [], ty', loc) in
          let d' = visit_decl (self :> isaVisitor) d' in
          Some d'
      | Decl_Record (_, _, fs, loc) ->
          let fs' = List.map (fun (f, ty) -> (f, Xform_constprop.xform_ty env ty)) fs in
          let d' = AST.Decl_Record (nm, [], fs', loc) in
          let d' = visit_decl (self :> isaVisitor) d' in
          Some d'
      | Decl_FunDefn (_, fty, body, loc) ->
          let rty' = Xform_constprop.xform_ty env fty.rty in
          let pnames = List.map fst values in
          let atys' = List.filter (fun (v, _, _) -> not (List.mem v pnames)) fty.args
                   |> List.map (fun (v, ty, od) -> (v, Xform_constprop.xform_ty env ty, Option.map (Xform_constprop.xform_expr env) od))
          in
          let setter_arg' = Option.map (fun (v, t) -> (v, Xform_constprop.xform_ty env t)) fty.setter_arg in
          let fty' = { fty with parameters=[]; args=atys'; rty=rty'; setter_arg=setter_arg' } in
          let body' = Xform_constprop.xform_stmts env body in
          let d' = AST.Decl_FunDefn (nm, fty', body', loc) in
          let d' = visit_decl (self :> isaVisitor) d' in
          Some d'
      | Decl_FunType (_, fty, loc) ->
          let rty' = Xform_constprop.xform_ty env fty.rty in
          let pnames = List.map fst values in
          let atys' = List.filter (fun (v, _, _) -> not (List.mem v pnames)) fty.args
                   |> List.map (fun (v, ty, od) -> (v, Xform_constprop.xform_ty env ty, Option.map (Xform_constprop.xform_expr env) od))
          in
          let setter_arg' = Option.map (fun (v, t) -> (v, Xform_constprop.xform_ty env t)) fty.setter_arg in
          let fty' = { fty with parameters=[]; args=atys'; rty=rty'; setter_arg=setter_arg' } in
          let d' = AST.Decl_FunType (nm, fty', loc) in
          let d' = visit_decl (self :> isaVisitor) d' in
          Some d'
      | _ ->
          None
      )

    method mk_monomorphic_decls (genv : Eval.GlobalEnv.t) (instance : Instance.t)
        (ds : AST.declaration list) : AST.declaration list option =
          assert (List.length ds > 0);
          let rec aux ds acc =
            match ds with
            | d :: ds' ->
                let* d' = self#mk_monomorphic_decl genv instance d in
                aux ds' (d' :: acc)
            | [] -> Some (List.rev acc)
          in
          aux ds []

    method create_monomorphic_instance (genv : Eval.GlobalEnv.t)
        (instance : Instance.t) (ds : AST.declaration list) : bool =
      if Instances.mem instance instances then (
        true
      ) else (
        ( match self#mk_monomorphic_decls genv instance ds with
        | Some ds' ->
          instances <- Instances.add instance ds' instances;
          true
        | None ->
          false
        )
      )

    method monomorphize_type (genv : Eval.GlobalEnv.t) (tc : Ident.t)
        (szs : Value.value list) : Ident.t option =
      let* d = IdentTable.find_opt type_decl_lookup_table tc in
      let (tc, tvs) =
        ( match d with
        | Decl_Typedef (tc, ps, _, _) -> (tc, ps)
        | Decl_Record (tc, ps, _, _) -> (tc, ps)
        | _ -> failwith "monomorphize_type"
        )
      in
      assert (List.length tvs == List.length szs);
      let instance = (tc, List.combine tvs szs) in
      if self#create_monomorphic_instance genv instance [ d ] then (
        let nm' = Instance.mk_monomorphic_name Loc.Unknown instance in
        Some nm'
      ) else (
        None
      )

    method monomorphize_fun (genv : Eval.GlobalEnv.t) (is_assignment : bool)
        (f : Ident.t) (ps : AST.expr list) (args : AST.expr list)
        : (Ident.t * AST.expr list) option =
      let* instance = self#find_requested_instance is_assignment f ps args in
      let* ds = IdentTable.find_opt fun_decl_lookup_table f in
      if self#create_monomorphic_instance genv instance ds then (
        let nm' = Instance.mk_monomorphic_name Loc.Unknown instance in
        let args' =
            ( match ds with
            | Decl_FunDefn (f, fty, _, _) :: _
            | Decl_FunType (f, fty, _) :: _ ->
               let tvs = List.map fst (snd instance) in
               let arg_names = List.map (fun (x, _, _) -> x) fty.args in
               let args' : AST.expr list = Utils.filter_map2
                   (fun nm arg -> if List.mem nm tvs then None else Some arg)
                   arg_names args
               in
               args'
            | _ -> failwith "monomorphize_fun"
            )
        in
        Some (nm', args')
      ) else (
        None
      )

    method monomorphize_foreign_fun (genv : Eval.GlobalEnv.t)
        (f : Ident.t) (ps : (Ident.t * Value.value) list)
        : Ident.t option =
      let instance = (f, ps) in
      let* ds = IdentTable.find_opt fun_decl_lookup_table f in
      if self#create_monomorphic_instance genv instance ds then (
        let nm' = Instance.mk_monomorphic_name Loc.Unknown instance in
        Some nm'
      ) else (
        None
      )

    method! vtype x =
      ( match x with
      | Type_Constructor (tc, es) ->
          ( match Utils.flatten_map_option const_expr es with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (
                 let* tc' = self#monomorphize_type genv tc sizes in
                 Some (ChangeDoChildrenPost (AST.Type_Constructor (tc', []), Fun.id))
                )
                ~default:DoChildren
          | None -> DoChildren
          )
      | _ -> DoChildren
      )

    method find_requested_instance (is_assignment : bool) (f : Ident.t)
        (ps : AST.expr list) (args : AST.expr list) : Instance.t option =
      let* f_requests = IdentTable.find_opt requests f in
      List.find_map (fun (p_request, a_request) ->
        (* When monomorphizing calls to assignment functions, we don't use the final argument *)
        let a_request' = if is_assignment then Utils.init a_request else a_request in
        if !verbose then
          Format.printf "Monomorphizing %a %d %d %d %d\n" Ident.pp f
            (List.length args) (List.length a_request')
            (List.length ps) (List.length p_request);
        assert (List.length args == List.length a_request');
        assert (List.length ps == List.length p_request);
        let matches = ref [] in (* accumulate matching args *)
        let is_match = List.for_all2 (fun (var, required, ov) arg ->
            if required then
                ( match (ov, const_expr arg) with
                | (Some expected_v, Some v) when Value.eq_value expected_v v -> matches := (var, v) :: !matches; true
                | (None, Some v) -> matches := (var, v) :: !matches; true
                | (_, _) -> false
                )
            else
                true
            )
            (p_request @ a_request')
            (ps @ args)
        in
        if !verbose then
          Format.printf "Matching against %a%a = %a\n" Ident.pp f
            ppRequest (p_request, a_request) Format.pp_print_bool is_match;
        if is_match && not (Utils.is_empty !matches) then
            Some (f, List.rev !matches)
        else
            None
      ) f_requests

    method! vexpr x =
      ( match x with
      | Expr_Record (tc, args, fs) ->
          ( match Utils.flatten_map_option (fun (nm, e) -> const_expr e) args with
          | Some [] -> DoChildren
          | Some sizes ->
              Option.value
                (
                  let* tc' = self#monomorphize_type genv tc sizes in
                  Some (ChangeDoChildrenPost (AST.Expr_Record (tc', [], fs), Fun.id))
                )
                ~default:DoChildren
          | None -> DoChildren
          )
      | Expr_TApply (f, tys, args, throws) ->
          (
          let* (f', args') = self#monomorphize_fun genv false f tys args in
          let e = AST.Expr_TApply (f', [], args', throws) in
          Some (ChangeDoChildrenPost (e, Fun.id))
          )
          |> Option.value ~default:DoChildren
      | _ -> DoChildren
      )

    method! vlexpr e =
      ( match e with
      | LExpr_Write (f, tes, es, throws) ->
          (
          let* (f', es') = self#monomorphize_fun genv true f tes es in
          let e = AST.LExpr_Write (f', [], es', throws) in
          Some (ChangeDoChildrenPost (e, Fun.id))
          )
          |> Option.value ~default:DoChildren
      | _ -> DoChildren
      )

    method! vstmt s =
      ( match s with
      | Stmt_VarDecl (is_constant, d, e, loc) ->

          let rec add_decl (d : AST.decl_item) : unit =
            ( match d with
            | AST.DeclItem_Tuple ds -> List.iter add_decl ds
            | DeclItem_Var (i, Some ty) -> self#update_local_type_info i ty
            | DeclItem_BitTuple ds ->
                List.iter (fun d -> match d with (Some i, ty) -> self#update_local_type_info i ty | _ -> ()) ds
            | _ -> ()
            )
          in

          (* possibly update type info *)
          add_decl d;

          ( match d with
          | DeclItem_Var (i, Some ty) -> (
            match self#collect_params [e] [] with
            | [] -> DoChildren
            | params -> (
                let decl = AST.Stmt_VarDeclsNoInit ([i], ty, loc) in
                let assign_stmt = AST.Stmt_Assign (AST.LExpr_Var i, e, loc) in
                match self#build_case_stmt loc assign_stmt params with
                | None -> DoChildren
                | Some case_stmt ->
                  let env = Xform_constprop.mkEnv genv [] in
                  let case_stmts' = Xform_constprop.xform_stmts env [ case_stmt ] in
                  (* Now monomorphize the calls in each 'when' *)
                  ChangeDoChildrenPost (decl :: case_stmts', Fun.id)
              )
            )
          | _ -> DoChildren
          )
      | Stmt_TCall (f, tys, args, throws, loc) ->
          (
          let* (f', args') = self#monomorphize_fun genv false f tys args in
          let s = AST.Stmt_TCall (f', [], args', throws, loc) in
          Some (ChangeDoChildrenPost ([s], Fun.id))
          )
          |> Option.value ~default:DoChildren
      | Stmt_Assign (LExpr_Var i, e, loc) ->
          ( match self#collect_params [e] [] with
          | [] -> DoChildren
          | params -> (
              match self#build_case_stmt loc s params with
              | None -> DoChildren
              | Some case_stmt ->
                let env = Xform_constprop.mkEnv genv [] in
                let case_stmts' = Xform_constprop.xform_stmts env [ case_stmt ] in
                (* Now monomorphize the calls in each 'when' *)
                ChangeDoChildrenPost (case_stmts', Fun.id)
            )
          )
      | _ -> DoChildren
      )

    method! vdecl d =
      (* Clear type info for each new declaration being processed *)
      self#clear_local_type_info ();

      ( match d with
      (* If declaration is a function, add argument type info, then regardless
         of declaration process it *)
      | Decl_FunType (_, fty, _)
      | Decl_FunDefn (_, fty, _, _)
      ->
          List.iter (fun (i, ty, _) -> self#update_local_type_info i ty) fty.args;
          Option.iter (fun (i, ty) -> self#update_local_type_info i ty) fty.setter_arg;
          DoChildren
      (* In foreign function declaration, replace imported/exported function
         with monomorphized version *)
      | Decl_FunFFI (nm, is_export, f, ps, loc) when List.length ps > 0 ->
          (
            let* f' = self#monomorphize_foreign_fun genv f ps in
            let d' = AST.Decl_FunFFI (nm, is_export, f', [], loc) in
            Some (ChangeDoChildrenPost (d', Fun.id))
          )
          |> Option.value ~default:DoChildren
      | _ -> DoChildren
      )

end (* monoClass *)

(* Add all global variables that has a type to the global type info map which
   is passed to the mono class *)
let build_global_type_info (ds : AST.declaration list) =
  let add_type_info (map : AST.ty Bindings.t) (d : AST.declaration) =
    ( match d with
    | Decl_Var (i, ty, _)
    | Decl_Const (i, Some ty, _, _) -> Bindings.add i ty map
    | _ -> map
    )
  in
  List.fold_left add_type_info Bindings.empty ds

(* Transform a function declaration into an implicit request to monomorphize
 * a function with respect to the function type parameters.
 *)
let mk_implicit_request (d : AST.declaration) : (Ident.t * request) option =
  let* (f, r) = ( match d with
  (* Function width parameters are treated as implicit requests to monomorphize a function.
   * Note that these can be overridden by explicit requests that might add
   * additional arguments.
   *)
  (* Ignoring Decl_FunDefn since it must be accompanied by Decl_FunType *)
  | Decl_FunType (f, fty, _)
    when not fty.is_builtin && not (Utils.is_empty fty.parameters)
    ->
      let ps = List.map (fun (x, _) -> (x, true, None)) fty.parameters in
      let formals = List.map (fun (x, _, _) -> (x, false, None)) fty.args in
      Some (f, (ps, formals))
  | _ ->
      None
  ) in
  if !verbose then
    Format.printf "Implicit request %a%a\n" Ident.pp f ppRequest r;
  Some (f, r)


(* Transform a function instantiation declaration into a request (i.e., a more convenient
 * representation of the request)
 *)
let mk_explicit_request (decl_lookup_table : AST.declaration list IdentTable.t)
    (d : AST.declaration) : (Ident.t * request) option =
  let mk_request f args =
    let* ds = IdentTable.find_opt decl_lookup_table f in
    List.find_map (function
      (* Ignoring Decl_FunDefn since it must be accompanied by Decl_FunType *)
      | AST.Decl_FunType (f, fty, _) when not fty.is_builtin ->
          let mk_arg_request ignore arg : (Ident.t * bool * Value.value option) =
            if List.mem arg ignore then
              (arg, false, None)
            else
              ( match List.assoc_opt arg args with
              | None -> (arg, false, None)
              | Some ov -> (arg, true, ov)
              )
          in
          let p_names = List.map fst fty.parameters in
          let ps = List.map (mk_arg_request []) p_names in
          let formals =
            List.map (fun (x, _, _) -> mk_arg_request p_names x) fty.args
          in
          Some (f, (ps, formals))
      | _ ->
          None
    ) ds
  in
  let* (f, r) = ( match d with
  | Decl_FunFFI (_, _, f, ps, loc) when List.length ps > 0 ->
      mk_request f (List.map (fun (p, v) -> (p, Some v)) ps)
  | Decl_FunInstance (f, ps, loc) when List.length ps > 0 ->
      mk_request f ps
  | _ ->
      None
  ) in
  if !verbose then
    Format.printf "Explicit request %a%a\n" Ident.pp f ppRequest r;
  Some (f, r)

let extend_with (t : request list IdentTable.t)
    (requests : (Ident.t * request) Seq.t) : unit =
  let replace (f, r) =
    IdentTable.find_opt t f
    |> (function Some rs -> r :: rs | None -> [ r ])
    |> IdentTable.replace t f
  in
  Seq.iter replace requests

let monomorphize (ds : AST.declaration list) : AST.declaration list =
  Eval.trace_exceptions := false;
  let genv = Eval.build_constant_environment ds in
  let global_type_info = build_global_type_info ds in
  let type_decl_lookup_table =
    ds
    |> List.to_seq
    |> Seq.filter_map monomorphizable_type_decl_to_ident_and_decl
    |> IdentTable.of_seq
  in
  let fun_decl_lookup_table =
    let table : AST.declaration list IdentTable.t = IdentTable.create 16 in
    ds
    |> List.filter_map monomorphizable_fun_decl_to_ident_and_decl
    |> List.iter (fun (f, d) ->
         let updated =
           match IdentTable.find_opt table f with
           | Some ds -> d :: ds
           | None -> [ d ]
         in
         IdentTable.replace table f (List.rev updated)
       );
    table
  in
  let implicit_requests = Seq.filter_map mk_implicit_request (List.to_seq ds) in
  let explicit_requests = Seq.filter_map (mk_explicit_request fun_decl_lookup_table) (List.to_seq ds) in
  let requests = IdentTable.of_seq (Seq.map (fun (f, r) -> f, [r]) implicit_requests) in
  let () = extend_with requests explicit_requests in
  let mono =
    new monoClass
      genv global_type_info type_decl_lookup_table fun_decl_lookup_table requests ds
  in
  let ds' = List.map (visit_decl (mono :> isaVisitor)) ds in
  let instances = List.concat mono#getInstances in
  Eval.trace_exceptions := true;
  ds' @ instances

(****************************************************************
 * Command: :xform_monomorphize
 ****************************************************************)

let _ =
  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    Commands.declarations := monomorphize !Commands.declarations;
    true
  in
  let flags = Arg.align [
        ("--auto-case-split",    Arg.Set enable_auto_case_split,   " Generate case split code automatically");
        ("--no-auto-case-split", Arg.Clear enable_auto_case_split,   " Do not generate case split code automatically");
        ("--verbose",            Arg.Set verbose,                  " Increase verbosity");
      ]
  in
  Commands.registerCommand "xform_monomorphize" flags [] [] "Monomorphize function calls" cmd

(****************************************************************
 * End
 ****************************************************************)
