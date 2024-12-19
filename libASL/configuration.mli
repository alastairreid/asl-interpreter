(****************************************************************
 * Configuration access
 *
 * This is used to manage configuration files
 *
 * Copyright Intel Inc (c) 2023-2024
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** Read a JSON configuration file *)
val read_configuration_file : string -> unit

(** Read list of strings from all previously read configuration files *)
val get_strings : string -> string list

(****************************************************************
 * End
 ****************************************************************)
