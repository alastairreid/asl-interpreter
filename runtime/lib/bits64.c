////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits64.h"

#include <stdbool.h>
#include <stdint.h>

#include "asl/integer.h"

#define M 64
#define N 64
#include "set_slice_template_c.h"
#undef N
#undef M

ASL_bits64_t
ASL_add_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x + y) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_and_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x & y;
}

ASL_bits64_t
ASL_append_bits_64(int x_width, int y_width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x << y_width) | y;
}

ASL_bits64_t
ASL_asr_bits_64(int width, ASL_bits64_t x, ASL_int_t d)
{
        bool sign_bit = x >> (width - 1);
        if (sign_bit) {
                x = ASL_not_bits_64(width, x);
                x = ASL_lsr_bits_64(width, x, d);
                x = ASL_not_bits_64(width, x);
        } else {
                x = ASL_lsr_bits_64(width, x, d);
        }
        return x;
}

ASL_int_t
ASL_cvt_bits_sint_64(int width, ASL_bits64_t x)
{
#ifdef ASL_INT128
        const unsigned __int128 mask = (unsigned __int128)1ULL << (width - 1);
#else
        const uint64_t mask = (uint64_t)1ULL << (width - 1);
#endif
        /* If the sign bit is 1 then, after XOR-ing,
           the subtraction borrows from higher bits making them 111..1 */
        return (ASL_int_t)((x ^ mask) - mask);
}

ASL_int_t
ASL_cvt_bits_uint_64(int width, ASL_bits64_t x)
{
        return (ASL_int_t)x;
}

ASL_bits64_t
ASL_cvt_int_bits_64(int width, ASL_int_t x)
{
        return (ASL_bits64_t)x & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_xor_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x ^ y;
}

bool
ASL_eq_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x == y;
}

ASL_bits64_t
ASL_lsl_bits_64(int width, ASL_bits64_t x, ASL_int_t d)
{
        return (x << d) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_lsr_bits_64(int width, ASL_bits64_t x, ASL_int_t d)
{
        return x >> d;
}

ASL_bits64_t
ASL_mk_mask_64(ASL_int_t w)
{
        if (w == 0) {
                return 0;
        } else {
                return UINT64_MAX >> (64 - w);
        }
}

ASL_bits64_t
ASL_mul_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x * y) & ASL_mk_mask_64(width);
}

bool
ASL_ne_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x != y;
}

ASL_bits64_t
ASL_not_bits_64(int width, ASL_bits64_t x)
{
        return ~x & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_ones_bits_64(ASL_int_t width)
{
        return ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_or_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x | y;
}

ASL_bits64_t
ASL_replicate_bits_64(int width, ASL_bits64_t x, ASL_int_t n)
{
        ASL_bits64_t r = 0;
        while (n-- > 0)
                r = (r << width) | x;
        return r;
}

ASL_bits64_t
ASL_sub_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x - y) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_zero_extend_bits_64_64(int width, ASL_bits64_t x, ASL_int_t n)
{
        return x;
}

ASL_bits64_t
ASL_sign_extend_bits_64_64(int width, ASL_bits64_t x, ASL_int_t n)
{
        if (x & (((uint64_t)1) << (width - 1))) {
            ASL_bits64_t ext = ASL_mk_mask_64(n) & ~ASL_mk_mask_64(width);
            return x | ext;
        } else {
            return x;
        }
}

ASL_bits64_t
ASL_zeros_bits_64(ASL_int_t width)
{
        return 0;
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
