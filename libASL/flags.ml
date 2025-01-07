(****************************************************************
 * Control flag registry
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module FlagMap = Map.Make(String)

let flags : (bool ref * string) FlagMap.t ref = ref FlagMap.empty

let registerFlag (name : string) (flag : bool ref) (description : string) : unit =
   flags := FlagMap.add name (flag, description) !flags

(****************************************************************
 * End
 ****************************************************************)
