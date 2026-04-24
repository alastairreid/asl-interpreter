////////////////////////////////////////////////////////////////
// Runtime info support for ASL's C backend
//
// Note: these functions can (and usually are) overridden in the linker
// command line by providing .o files that override both functions.
//
// Copyright (C) 2026-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/info.h"

#include <stdarg.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void
ASL_info(int n_states, int64_t level, const char *fmt, ...)
{
        (void)level;  // not used in this default implementation

        va_list args;
        va_start(args, fmt);

        /* Skip first n_states arguments. These are passed in case the info
         * message needs to access them, but this default implementation
         * doesn't. */
        for (int i = 0; i < n_states; ++i) {
                (void)va_arg(args, void *);
        }

        vprintf(fmt, args);
        printf("\n");

        va_end(args);
}

#ifdef __cplusplus
}
#endif
