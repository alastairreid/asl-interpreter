// FFI testing support functions to be imported into ASL test programs
// Copyright (C) 2025-2025 Intel Corporation
#include <stdint.h>
#include "asl_ffi.h"

// 8, 16, 32 and 64-bit bitvectors are represented using uint*_t
uint32_t FFI_Invert32(uint32_t x)
{
        return ~x;
}

// Wide bitvectors are represented as arrays of uint64_t
// If returning a wide bitvector, an extra array argument is
// added as the last argument
void FFI_Invert128(uint64_t x[2], uint64_t y[2])
{
        for(int i = 0; i < 2; ++i) {
                y[i] = ~x[i];
        }
}
