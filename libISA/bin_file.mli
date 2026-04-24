(****************************************************************
 * Binary file loader
 *
 * Simplistic binary file loader
 *
 * Copyright (C) 2023-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

(** load binary file *)
val load_file : string -> (Int64.t -> char -> unit) -> Int64.t -> unit

(****************************************************************
 * End
 ****************************************************************)
