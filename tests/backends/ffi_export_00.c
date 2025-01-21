// FFI testing support functions to be imported/exported into ASL test programs
// Copyright (C) 2025-2025 Intel Corporation

#include <stdint.h>
#include "asl_ffi.h"

uint32_t FFI_Call128(uint64_t x[2])
{
        return FFI_Extract128_32(x, 12);
}
