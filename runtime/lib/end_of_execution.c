////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Note: these functions can (and usually are) overridden in the linker
// command line by providing .o files that override both functions.
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/error.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void
ASL_end_execution(bool success)
{
        exit(success ? 0 : 1);
}

#ifdef __cplusplus
}
#endif

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
