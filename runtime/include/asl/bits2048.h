////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS2048_H
#define ASL_BITS2048_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
        uint64_t u64[32];
        uint32_t u32[64];
} ASL_bits2048_t;

static inline ASL_bits2048_t
ASL_bits_2048(
              uint64_t v31, uint64_t v30, uint64_t v29, uint64_t v28,
              uint64_t v27, uint64_t v26, uint64_t v25, uint64_t v24,
              uint64_t v23, uint64_t v22, uint64_t v21, uint64_t v20,
              uint64_t v19, uint64_t v18, uint64_t v17, uint64_t v16,
              uint64_t v15, uint64_t v14, uint64_t v13, uint64_t v12,
              uint64_t v11, uint64_t v10, uint64_t v9, uint64_t v8,
              uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4,
              uint64_t v3, uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_bits2048_t){ { v0, v1, v2, v3, v4, v5, v6, v7,
                                   v8, v9, v10, v11, v12, v13, v14, v15,
                                   v16, v17, v18, v19, v20, v21, v22, v23,
                                   v24, v25, v26, v27, v28, v29, v30, v31 } };
}

static inline ASL_bits2048_t
ASL_bits_max_2048()
{
        return ASL_bits_2048(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits2048_t
ASL_bits_zero_2048()
{
        return ASL_bits_2048(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#define N 2048
#include "asl/bits_template.h"
#undef N

static inline ASL_bits64_t
ASL_cast_bits_2048_64(ASL_bits2048_t x)
{
        return x.u64[0];
}

static inline ASL_bits128_t
ASL_cast_bits_2048_128(ASL_bits2048_t x)
{
        return ASL_bits_128(x.u64[1], x.u64[0]);
}

static inline ASL_bits256_t
ASL_cast_bits_2048_256(ASL_bits2048_t x)
{
        return ASL_bits_256(x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits512_t
ASL_cast_bits_2048_512(ASL_bits2048_t x)
{
        return ASL_bits_512(x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                            x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits1024_t
ASL_cast_bits_2048_1024(ASL_bits2048_t x)
{
        return ASL_bits_1024(x.u64[15], x.u64[14], x.u64[13], x.u64[12],
                             x.u64[11], x.u64[10], x.u64[9], x.u64[8],
                             x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                             x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits2048_t ASL_lsl_bits_2048(int width, ASL_bits2048_t x, ASL_int_t d);
ASL_bits2048_t ASL_lsr_bits_2048(int width, ASL_bits2048_t x, ASL_int_t d);
ASL_bits1024_t ASL_slice_lowd_2048_1024(ASL_bits2048_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits512_t ASL_slice_lowd_2048_512(ASL_bits2048_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits256_t ASL_slice_lowd_2048_256(ASL_bits2048_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits128_t ASL_slice_lowd_2048_128(ASL_bits2048_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits64_t ASL_slice_lowd_2048_64(ASL_bits2048_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits2048_t ASL_zero_extend_bits_64_2048(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits2048_t ASL_zero_extend_bits_128_2048(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits2048_t ASL_zero_extend_bits_256_2048(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits2048_t ASL_zero_extend_bits_512_2048(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits2048_t ASL_zero_extend_bits_1024_2048(int width, ASL_bits1024_t x, ASL_int_t n);
ASL_bits2048_t ASL_sign_extend_bits_64_2048(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits2048_t ASL_sign_extend_bits_128_2048(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits2048_t ASL_sign_extend_bits_256_2048(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits2048_t ASL_sign_extend_bits_512_2048(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits2048_t ASL_sign_extend_bits_1024_2048(int width, ASL_bits1024_t x, ASL_int_t n);
ASL_bits2048_t ASL_zeros_bits_2048(ASL_int_t width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS2048_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
