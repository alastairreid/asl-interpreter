////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Note: these functions can (and usually are) overridden in the linker
// command line by providing .o files that override both functions.
//
// Copyright (C) 2022-2024 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/error.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void
ASL_error(const char* loc, const char* msg)
{
        printf("%s: ASL error: %s\n\n", loc, msg);
        exit(1);
}

void
ASL_runtime_error(const char *msg)
{
        printf("Runtime error: %s\n", msg);
        exit(1);
}

void
ASL_assert(const char* loc, const char* expr, bool c)
{
        if (!c) {
                printf("%s: Evaluation error: assertion failure: %s\n\n", loc, expr);
                exit(1);
        }
}

#ifdef __cplusplus
}
#endif

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
