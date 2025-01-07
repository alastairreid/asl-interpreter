////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS512_H
#define ASL_BITS512_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
        uint64_t u64[8];
        uint32_t u32[16];
} ASL_bits512_t;

static inline ASL_bits512_t
ASL_bits_512(uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4, uint64_t v3,
             uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_bits512_t){ { v0, v1, v2, v3, v4, v5, v6, v7 } };
}

static inline ASL_bits512_t
ASL_bits_max_512()
{
        return ASL_bits_512(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                            UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits512_t
ASL_bits_zero_512()
{
        return ASL_bits_512(0, 0, 0, 0, 0, 0, 0, 0);
}

#define N 512
#include "asl/bits_template.h"
#undef N

static inline ASL_bits64_t
ASL_cast_bits_512_64(ASL_bits512_t x)
{
        return x.u64[0];
}

static inline ASL_bits128_t
ASL_cast_bits_512_128(ASL_bits512_t x)
{
        return ASL_bits_128(x.u64[1], x.u64[0]);
}

static inline ASL_bits256_t
ASL_cast_bits_512_256(ASL_bits512_t x)
{
        return ASL_bits_256(x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits512_t ASL_lsl_bits_512(int width, ASL_bits512_t x, ASL_int_t d);
ASL_bits512_t ASL_lsr_bits_512(int width, ASL_bits512_t x, ASL_int_t d);
ASL_bits256_t ASL_slice_lowd_512_256(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits128_t ASL_slice_lowd_512_128(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits64_t ASL_slice_lowd_512_64(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits512_t ASL_zero_extend_bits_64_512(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits512_t ASL_zero_extend_bits_128_512(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits512_t ASL_zero_extend_bits_256_512(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits512_t ASL_sign_extend_bits_64_512(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits512_t ASL_sign_extend_bits_128_512(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits512_t ASL_sign_extend_bits_256_512(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits512_t ASL_zeros_bits_512(ASL_int_t width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS512_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
