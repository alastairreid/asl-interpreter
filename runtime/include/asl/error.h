////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_ERROR_H
#define ASL_ERROR_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ASL_NORETURN __attribute__((__noreturn__))

ASL_NORETURN void ASL_error(const char *loc, const char *msg);

#define ASL_error_unmatched_case(loc) ASL_error(loc, "unmatched case statement")

ASL_NORETURN void ASL_runtime_error(const char *msg);

#define ASL_runtime_error_if(cond, msg) if (cond) ASL_runtime_error(msg);

void ASL_assert(const char* loc, const char* expr, bool c);

#ifdef __cplusplus
}
#endif

#endif  // ASL_ERROR_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
