////////////////////////////////////////////////////////////////
// Runtime integer support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_INTEGER_H
#define ASL_INTEGER_H

#include <stdint.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int64_t ASL_int64_t;
typedef __int128 ASL_int128_t;

#define ASL_INT64_MAX INT64_MAX
#define ASL_INT64_MIN INT64_MIN
#define ASL_INT128_MAX ASL_int_128(INT64_MAX, UINT64_MAX)
#define ASL_INT128_MIN (-ASL_INT128_MAX - 1)

#define ASL_INT128

#ifdef ASL_INT128
typedef ASL_int128_t ASL_int_t;
#else
typedef ASL_int64_t ASL_int_t;
#endif

#define ASL_int_zero(sizeof_x) \
        ASL_CC(ASL_int_zero_, sizeof_x)()

static inline ASL_int64_t
ASL_int_zero_64()
{
        return 0;
}

#define ASL_int_max(sizeof_x) \
        ASL_CC(ASL_int_max_, sizeof_x)()

static inline ASL_int64_t
ASL_int_max_64()
{
        return INT64_MAX;
}

static inline ASL_int128_t
ASL_int_128(uint64_t v1, uint64_t v0)
{
        return ((__int128)v1 << 64) | (__int128)v0;
}

static inline ASL_int128_t
ASL_int_zero_128()
{
        return 0;
}

static inline ASL_int128_t
ASL_int_max_128()
{
        return ASL_int_128(INT64_MAX, UINT64_MAX);
}

static inline bool
ASL_is_pow2_int(ASL_int_t x)
{
        return x != 0 && (x & (x - 1)) == 0;
}

static inline ASL_int_t
ASL_exact_div_int(ASL_int_t x, ASL_int_t y)
{
        assert(y != 0);
#if 0 // disable this check for now until the spec is fixed
        assert(x % y == 0);
#endif
        return x / y;
}

static inline ASL_int_t
ASL_fdiv_int(ASL_int_t x, ASL_int_t y)
{
        assert(y != 0);
        const ASL_int_t quot = x / y;
        const ASL_int_t rem = x % y;
        return quot - (rem != 0 && quot < 0);
}

static inline ASL_int_t
ASL_frem_int(ASL_int_t x, ASL_int_t y)
{
        assert(y != 0);
        return x - ASL_fdiv_int(x, y) * y;
}

static inline ASL_int_t
ASL_mask_int(ASL_int_t w)
{
#ifdef ASL_INT128
        return (ASL_int_t)((unsigned __int128)(-1LL) >> (128 - w));
#else
        return (ASL_int_t)(UINT64_MAX >> (64 - w));
#endif
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_INTEGER_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
