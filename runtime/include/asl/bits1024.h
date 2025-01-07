////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS1024_H
#define ASL_BITS1024_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
        uint64_t u64[16];
        uint32_t u32[32];
} ASL_bits1024_t;

static inline ASL_bits1024_t
ASL_bits_1024(uint64_t v15, uint64_t v14, uint64_t v13, uint64_t v12,
              uint64_t v11, uint64_t v10, uint64_t v9, uint64_t v8,
              uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4,
              uint64_t v3, uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_bits1024_t){ { v0, v1, v2, v3, v4, v5, v6, v7,
                                   v8, v9, v10, v11, v12, v13, v14, v15 } };
}

static inline ASL_bits1024_t
ASL_bits_max_1024()
{
        return ASL_bits_1024(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits1024_t
ASL_bits_zero_1024()
{
        return ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#define N 1024
#include "asl/bits_template.h"
#undef N

static inline ASL_bits64_t
ASL_cast_bits_1024_64(ASL_bits1024_t x)
{
        return x.u64[0];
}

static inline ASL_bits128_t
ASL_cast_bits_1024_128(ASL_bits1024_t x)
{
        return ASL_bits_128(x.u64[1], x.u64[0]);
}

static inline ASL_bits256_t
ASL_cast_bits_1024_256(ASL_bits1024_t x)
{
        return ASL_bits_256(x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits512_t
ASL_cast_bits_1024_512(ASL_bits1024_t x)
{
        return ASL_bits_512(x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                            x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits1024_t ASL_lsl_bits_1024(int width, ASL_bits1024_t x, ASL_int_t d);
ASL_bits1024_t ASL_lsr_bits_1024(int width, ASL_bits1024_t x, ASL_int_t d);
ASL_bits512_t ASL_slice_lowd_1024_512(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits256_t ASL_slice_lowd_1024_256(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits128_t ASL_slice_lowd_1024_128(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits64_t ASL_slice_lowd_1024_64(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits1024_t ASL_zero_extend_bits_64_1024(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits1024_t ASL_zero_extend_bits_128_1024(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits1024_t ASL_zero_extend_bits_256_1024(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits1024_t ASL_zero_extend_bits_512_1024(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits1024_t ASL_sign_extend_bits_64_1024(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits1024_t ASL_sign_extend_bits_128_1024(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits1024_t ASL_sign_extend_bits_256_1024(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits1024_t ASL_sign_extend_bits_512_1024(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits1024_t ASL_zeros_bits_1024(ASL_int_t width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS1024_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
