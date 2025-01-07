////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS64_H
#define ASL_BITS64_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/integer.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef uint64_t ASL_bits64_t;

#define ASL_CC(x, y) x##y
#define ASL_CC4(a, b, c, d) a##b##c##d
#define ASL_CC_INDIR(x, y) ASL_CC(x, y)

#define ASL_bits(sizeof_x, ...) \
        ASL_CC(ASL_bits_, sizeof_x)(__VA_ARGS__)

static inline ASL_bits64_t
ASL_bits_64(uint64_t x)
{
        return x;
}

#define ASL_bits_max(sizeof_x) \
        ASL_CC(ASL_bits_max_, sizeof_x)()

static inline ASL_bits64_t
ASL_bits_max_64()
{
        return UINT64_MAX;
}

#define ASL_bits_zero(sizeof_x) \
        ASL_CC(ASL_bits_zero_, sizeof_x)()

static inline ASL_bits64_t
ASL_bits_zero_64()
{
        return 0;
}

#define ASL_add_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_add_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_add_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_and_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_and_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_and_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_append_bits(sizeof_res, m, n, x, y) \
        ASL_CC(ASL_append_bits_, sizeof_res)(m, n, x, y)

ASL_bits64_t ASL_append_bits_64(int x_width, int y_width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_asr_bits(sizeof_x, n, x, d) \
        ASL_CC(ASL_asr_bits_, sizeof_x)(n, x, d)

ASL_bits64_t ASL_asr_bits_64(int width, ASL_bits64_t x, ASL_int_t d);

#define ASL_cvt_bits_sint(sizeof_x, n, x) \
        ASL_CC(ASL_cvt_bits_sint_, sizeof_x)(n, x)

ASL_int_t ASL_cvt_bits_sint_64(int width, ASL_bits64_t x);

#define ASL_cvt_bits_uint(sizeof_x, n, x) \
        ASL_CC(ASL_cvt_bits_uint_, sizeof_x)(n, x)

ASL_int_t ASL_cvt_bits_uint_64(int width, ASL_bits64_t x);

#define ASL_cvt_int_bits(sizeof_x, n, x) \
        ASL_CC(ASL_cvt_int_bits_, sizeof_x)(n, x)

ASL_bits64_t ASL_cvt_int_bits_64(int width, ASL_int_t x);

#define ASL_eor_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_eor_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_eor_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_eq_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_eq_bits_, sizeof_x)(n, x, y)

bool ASL_eq_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_lsl_bits(sizeof_x, n, x, d) \
        ASL_CC(ASL_lsl_bits_, sizeof_x)(n, x, d)

ASL_bits64_t ASL_lsl_bits_64(int width, ASL_bits64_t x, ASL_int_t d);

#define ASL_lsr_bits(sizeof_x, n, x, d) \
        ASL_CC(ASL_lsr_bits_, sizeof_x)(n, x, d)

ASL_bits64_t ASL_lsr_bits_64(int width, ASL_bits64_t x, ASL_int_t d);

#define ASL_replicate_bits(sizeof_res, m, x, n) \
        ASL_CC(ASL_replicate_bits_, sizeof_res)(m, x, n)

ASL_bits64_t ASL_replicate_bits_64(int width, ASL_bits64_t x, ASL_int_t n);

#define ASL_mk_mask(n, w) \
        ASL_CC(ASL_mk_mask_, n)(w)

ASL_bits64_t ASL_mk_mask_64(ASL_int_t w);

#define ASL_mul_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_mul_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_mul_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_ne_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_ne_bits_, sizeof_x)(n, x, y)

bool ASL_ne_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_not_bits(sizeof_x, n, x) \
        ASL_CC(ASL_not_bits_, sizeof_x)(n, x)

ASL_bits64_t ASL_not_bits_64(int width, ASL_bits64_t x);

#define ASL_ones_bits(sizeof_x, n) \
        ASL_CC(ASL_ones_bits_, sizeof_x)(n)

ASL_bits64_t ASL_ones_bits_64(ASL_int_t width);

#define ASL_or_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_or_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_or_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_slice_lowd(sizeof_x, sizeof_res, x, lo, width) \
        ASL_CC4(ASL_slice_lowd_, sizeof_x, _, sizeof_res)(x, lo, width)

static inline ASL_bits64_t
ASL_slice_lowd_64_64(ASL_bits64_t x, ASL_int_t lo, ASL_int_t width)
{
        return (x >> lo) & ASL_mk_mask_64(width);
}

#define ASL_set_slice(sizeof_l, sizeof_r, l_width, l, i, r_width, r) \
        ASL_CC4(ASL_set_slice_, sizeof_l, _, sizeof_r)(l_width, l, i, r_width, r)

static inline void
ASL_set_slice(64, 64, int l_width, ASL_bits64_t *l, ASL_int_t i, int r_width, ASL_bits64_t r)
{
        ASL_bits64_t mask = ASL_mk_mask_64(r_width);
        *l = (*l & ~(mask << i)) | (r << i);
}

#define ASL_sub_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_sub_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_sub_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_zero_extend_bits(sizeof_x, sizeof_res, m, x, n) \
        ASL_CC4(ASL_zero_extend_bits_, sizeof_x, _, sizeof_res)(m, x, n)

ASL_bits64_t ASL_zero_extend_bits_64_64(int width, ASL_bits64_t x, ASL_int_t n);

#define ASL_sign_extend_bits(sizeof_x, sizeof_res, m, x, n) \
        ASL_CC4(ASL_sign_extend_bits_, sizeof_x, _, sizeof_res)(m, x, n)

ASL_bits64_t ASL_sign_extend_bits_64_64(int width, ASL_bits64_t x, ASL_int_t n);

ASL_bits64_t ASL_sign64(ASL_bits64_t x, ASL_int_t n);

#define ASL_zeros_bits(sizeof_x, n) \
        ASL_CC(ASL_zeros_bits_, sizeof_x)(n)

ASL_bits64_t ASL_zeros_bits_64(ASL_int_t width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS64_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
