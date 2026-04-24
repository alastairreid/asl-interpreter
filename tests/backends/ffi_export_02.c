// Copyright (C) 2025-2026 Intel Corporation

#include <stdint.h>

#include "isa_ffi.h"

/* The helper function is imported into ISA to test the calls to exported
   functions, which allow reading/writing the variable from C. */
void Helper(void)
{
        int x = v_Read();
        v_Write(x + 1);
}
