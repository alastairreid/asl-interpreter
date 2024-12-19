(****************************************************************
 * Metadata generator
 *
 * Copyright Intel (c) 2023-2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Write metadata to a json file *)
val generate_callgraph : string -> Asl_ast.declaration list -> unit

(****************************************************************
 * End
 ****************************************************************)
