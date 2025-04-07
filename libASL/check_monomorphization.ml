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

let parameters_of_decl (d : AST.declaration) : Ident.t list =
  ( match d with
  | Decl_Record (_, ps, _, _) -> ps
  | Decl_FunType (_, fty, _) -> fty.parameters |> List.map fst
  | Decl_FunDefn (_, fty, _, _) -> fty.parameters |> List.map fst
  | _ -> []
  )

let show_dependency_graph
  (callers : IdentSet.t Bindings.t)
  (callees : IdentSet.t Bindings.t)
  (parameters : IdentSet.t Bindings.t)
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
      Format.printf " "
    done;
    let ps = Bindings.find_opt x parameters |> Option.value ~default:IdentSet.empty in
    Format.printf "%s with bitwidth parameters {%a}\n" (Ident.name x) pp_identset ps;
    let ys = Bindings.find_opt x callees |> Option.value ~default:IdentSet.empty in
    IdentSet.iter (display_tree (depth+1)) (IdentSet.inter names ys)
  in
  IdentSet.iter (fun root ->
      Format.printf "The bitwidth parameters in this call-graph are not statically known\n";
      display_tree 2 root
    ) !roots


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
      Format.printf "%s: Bitwidth parameters are not statically known in the following definitions.\n"
        seriousness;
      Format.printf "This will cause code generation to fail.\n\n";

      let names = List.filter_map decl_name polymorphic |> IdentSet.of_list in
      if !verbose then begin
        (* build callgraph and reverse callgraph *)
        let callees = ref Bindings.empty in
        let callers = ref Bindings.empty in
        let parameters = ref Bindings.empty in
        List.iter
          (fun d ->
             let x = Option.get (decl_name d) in
             let ys = IdentSet.inter (calls_of_decl d) names in
             let ps = IdentSet.of_list (parameters_of_decl d) in
             parameters := Bindings.add x ps !parameters;
             let old = Bindings.find_opt x !callees |> Option.value ~default:IdentSet.empty in
             callees := Bindings.add x (IdentSet.union old ys) !callees;
             IdentSet.iter (fun y -> callers := addToBindingSet y x !callers) ys;
          )
          !Commands.declarations;
        show_dependency_graph !callers !callees !parameters names
      end else begin
        IdentSet.iter (fun x -> Format.printf "%s\n" (Ident.name x)) names
      end;
      Format.printf "\nThe simplest way to fix monomorphization issues is usually\n";
      Format.printf "to change the topmost function in the call-graph so that\n";
      Format.printf "the parameters to the function it calls are statically known.\n";
      Format.printf "\nFor example, you might use an if-statement or case-statement to separate\n";
      Format.printf "the cases for different parameter sizes.\n";
      Format.printf "This is often easiest to do in machine generated code such as instruction decoders.\n";

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
