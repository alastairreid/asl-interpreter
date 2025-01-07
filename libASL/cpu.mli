(****************************************************************
 * CPU interface
 *
 * Copyright Arm Limited (c) 2017-2019
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type cpu = {
  env : Eval.Env.t;
  setConfig : Ident.t -> Value.value -> unit;
  reset : unit -> unit;
  step : unit -> unit;
  getPC : unit -> Primops.bigint;
  setPC : Int64.t -> unit;
  elfwrite8 : Int64.t -> char -> unit;
}

val mkCPU : Eval.Env.t -> cpu

(****************************************************************
 * End
 ****************************************************************)
