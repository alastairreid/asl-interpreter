(****************************************************************
 * Error
 *
 * Copyright (C) 2022-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

exception Unimplemented of (Loc.t * string * (Format.formatter -> unit))
exception UnknownObject of (Loc.t * string * string)
exception DoesNotMatch of (Loc.t * string * string * string)
exception IsNotA of (Loc.t * string * string)
exception Ambiguous of (Loc.t * string * string)
exception TypeError of (Loc.t * string)
exception ParseError of Loc.t

val print_exception : exn -> unit

(****************************************************************
 * End
 ****************************************************************)
