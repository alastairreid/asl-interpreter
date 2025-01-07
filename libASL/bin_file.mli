(****************************************************************
 * Binary file loader
 *
 * Simplistic binary file loader
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** load binary file *)
val load_file : string -> (Int64.t -> char -> unit) -> Int64.t -> unit

(****************************************************************
 * End
 ****************************************************************)
