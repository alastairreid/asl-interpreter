(****************************************************************
 * Metadata generator
 *
 * Copyright (C) 2023-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Write metadata to a json file *)
val generate_callgraph : string -> Asl_ast.declaration list -> unit

(****************************************************************
 * End
 ****************************************************************)
