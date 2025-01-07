(****************************************************************
 * Detect functions that have not been monomorphized
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
open Asl_utils
open Identset

let is_polymorphic_function (fty : AST.function_type) : bool =
  not (Utils.is_empty fty.parameters)

let is_polymorphic_decl (d : AST.declaration) : bool =
  ( match d with
  | Decl_Record (_, ps, _, _) -> not (Utils.is_empty ps)
  | Decl_FunType (_, fty, _) -> is_polymorphic_function fty
  | Decl_FunDefn (_, fty, _, _) -> is_polymorphic_function fty
  | _ -> false
  )

let show_dependency_graph
  (callers : IdentSet.t Bindings.t)
  (callees : IdentSet.t Bindings.t)
  (names : IdentSet.t)
  : unit
  =
  (* Find callers of the top of each chain of monomorphization failures.
   * This is the first function in the chain that does not have a monomorphization
   * failure.
   *)
  let roots = ref IdentSet.empty in

  let rec find_roots (x : Ident.t) : unit =
    let parents = Option.value ~default:IdentSet.empty (Bindings.find_opt x callers) in
    let poly_functions = IdentSet.inter parents names in
    if IdentSet.is_empty poly_functions then begin
      roots := IdentSet.union !roots parents
    end else begin
      IdentSet.iter find_roots poly_functions
    end
  in
  IdentSet.iter find_roots names;

  (* Display the monomorphization failure trees *)
  let rec display_tree (depth : int) (x : Ident.t) : unit =
    for _ = 1 to 2*depth do
      print_char ' '
    done;
    Printf.printf "%s\n" (Ident.name x);
    let ys = Bindings.find_opt x callees |> Option.value ~default:IdentSet.empty in
    IdentSet.iter (display_tree (depth+1)) (IdentSet.inter names ys)
  in
  IdentSet.iter (display_tree 2) !roots


(****************************************************************
 * Command: :check_monomorphization
 ****************************************************************)

let _ =
  let fatal = ref false in
  let verbose = ref false in

  let cmd (tcenv : Tcheck.Env.t) (cpu : Cpu.cpu) : bool =
    let polymorphic = List.filter is_polymorphic_decl !Commands.declarations in
    if not (Utils.is_empty polymorphic) then (
      let seriousness = if !fatal then "ERROR" else "WARNING" in
      Printf.printf "%s: Monomorphization failed for the following definitions.\n"
        seriousness;
      Printf.printf "This will cause code generation to fail.\n\n";

      let names = List.filter_map decl_name polymorphic |> IdentSet.of_list in
      if !verbose then begin
        (* build callgraph and reverse callgraph *)
        let callees = ref Bindings.empty in
        let callers = ref Bindings.empty in
        List.iter
          (fun d ->
             let x = Option.get (decl_name d) in
             let ys = IdentSet.inter (calls_of_decl d) names in
             let old = Bindings.find_opt x !callees |> Option.value ~default:IdentSet.empty in
             callees := Bindings.add x (IdentSet.union old ys) !callees;
             IdentSet.iter (fun y -> callers := addToBindingSet y x !callers) ys;
          )
          !Commands.declarations;
        show_dependency_graph !callers !callees names
      end else begin
        IdentSet.iter (fun x -> Printf.printf "%s\n" (Ident.name x)) names
      end;
      not !fatal
    ) else (
      true
    )
  in

  let flags = Arg.align [
      ("--fatal", Arg.Set fatal, " Abort build if incompletely monomorphized");
      ("--verbose", Arg.Set verbose, " Show detailed dependency chains");
    ]
  in
  Commands.registerCommand "check_monomorphization" flags [] [] "Check for monomorphization failures" cmd

(****************************************************************
 * End
 ****************************************************************)
