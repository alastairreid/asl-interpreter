(****************************************************************
 * Commands registry
 *
 * Copyright (C) 2024-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

type command = Tcheck.Env.t -> Cpu.cpu -> bool
type flag = (Stdlib.Arg.key * Stdlib.Arg.spec * Stdlib.Arg.doc)
type arg = (string ref * Stdlib.Arg.doc)

(** [registerCommand name args description cmd] registers a command
    @param name        Command name
    @param flags       Stdlib.Arg flag list
    @param args        Mandatory argument list
    @param opt_args    Optional argument list
    @param description Short description (used for :help command)
    @param cmd         Function to parse the arguments and execute the command
 *)
val registerCommand : string -> flag list -> arg list -> arg list -> string -> command -> unit

val execute_command : string -> string list -> command

val print_help : unit -> unit

(** List of all declarations.
 *  This list can be accessed and updated by commands
 *)
val declarations : Asl_ast.declaration list ref

(****************************************************************
 * End
 ****************************************************************)
