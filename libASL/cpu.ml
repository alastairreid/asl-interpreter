(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast

type cpu = {
  env : Eval.Env.t;
  setConfig : Ident.t -> Value.value -> unit;
  reset : unit -> unit;
  step : unit -> unit;
  getPC : unit -> Primops.bigint;
  setPC : Int64.t -> unit;
  elfwrite8 : Int64.t -> char -> unit;
}

let mkCPU (env : Eval.Env.t) : cpu =
  let genv = Eval.Env.globals env in
  let loc = Loc.Unknown in

  let setConfig (key : Ident.t) (v : Value.value) : unit =
    Eval.GlobalEnv.set_config genv key v
  and reset () : unit =
    Eval.eval_proccall loc env Builtin_idents.asl_reset [] []
  and step () : unit =
    Eval.eval_proccall loc env Builtin_idents.asl_step [] []
  and getPC () : Primops.bigint =
    let r = Eval.eval_funcall loc env Builtin_idents.asl_get_pc [] [] in
    Value.to_integer loc r
  and setPC (x : Int64.t) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 x)) in
    Eval.eval_proccall loc env Builtin_idents.asl_set_pc [] [a]
  and elfwrite8 (addr : Int64.t) (b : char) : unit =
    let a = Value.VBits (Primops.mkBits 64 (Z.of_int64 addr)) in
    let b = Value.VBits (Primops.mkBits 8 (Z.of_int (Char.code b))) in
    Eval.eval_proccall loc env Builtin_idents.asl_elf_write_memory8 [] [a; b]
  in
  { env; setConfig; reset; step; getPC; setPC; elfwrite8 }

(****************************************************************
 * End
 ****************************************************************)
