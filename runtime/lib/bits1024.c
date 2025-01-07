////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits1024.h"

#include <stdbool.h>

#define N 1024
#include "bits_template_c.h"
#undef N

ASL_bits1024_t
ASL_lsl_bits_1024(int width, ASL_bits1024_t x, ASL_int_t d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[15] = (x.u64[14] >> (64 - d)) | (x.u64[15] << d);
                x.u64[14] = (x.u64[13] >> (64 - d)) | (x.u64[14] << d);
                x.u64[13] = (x.u64[12] >> (64 - d)) | (x.u64[13] << d);
                x.u64[12] = (x.u64[11] >> (64 - d)) | (x.u64[12] << d);
                x.u64[11] = (x.u64[10] >> (64 - d)) | (x.u64[11] << d);
                x.u64[10] = (x.u64[9]  >> (64 - d)) | (x.u64[10] << d);
                x.u64[9]  = (x.u64[8]  >> (64 - d)) | (x.u64[9] << d);
                x.u64[8]  = (x.u64[7]  >> (64 - d)) | (x.u64[8] << d);
                x.u64[7]  = (x.u64[6]  >> (64 - d)) | (x.u64[7] << d);
                x.u64[6]  = (x.u64[5]  >> (64 - d)) | (x.u64[6] << d);
                x.u64[5]  = (x.u64[4]  >> (64 - d)) | (x.u64[5] << d);
                x.u64[4]  = (x.u64[3]  >> (64 - d)) | (x.u64[4] << d);
                x.u64[3]  = (x.u64[2]  >> (64 - d)) | (x.u64[3] << d);
                x.u64[2]  = (x.u64[1]  >> (64 - d)) | (x.u64[2] << d);
                x.u64[1]  = (x.u64[0]  >> (64 - d)) | (x.u64[1] << d);
                x.u64[0]  = x.u64[0] << d;
        } else {
                x.u64[15] = x.u64[14];
                x.u64[14] = x.u64[13];
                x.u64[13] = x.u64[12];
                x.u64[12] = x.u64[11];
                x.u64[11] = x.u64[10];
                x.u64[10] = x.u64[9];
                x.u64[9]  = x.u64[8];
                x.u64[8]  = x.u64[7];
                x.u64[7]  = x.u64[6];
                x.u64[6]  = x.u64[5];
                x.u64[5]  = x.u64[4];
                x.u64[4]  = x.u64[3];
                x.u64[3]  = x.u64[2];
                x.u64[2]  = x.u64[1];
                x.u64[1]  = x.u64[0];
                x.u64[0]  = 0;
                x = ASL_lsl_bits_1024(width, x, d - 64);
        }
        return ASL_and_bits_1024(width, x, ASL_mk_mask_1024(width));
}

ASL_bits1024_t
ASL_lsr_bits_1024(int width, ASL_bits1024_t x, ASL_int_t d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[0]  = (x.u64[1]  << (64 - d)) | (x.u64[0] >> d);
                x.u64[1]  = (x.u64[2]  << (64 - d)) | (x.u64[1] >> d);
                x.u64[2]  = (x.u64[3]  << (64 - d)) | (x.u64[2] >> d);
                x.u64[3]  = (x.u64[4]  << (64 - d)) | (x.u64[3] >> d);
                x.u64[4]  = (x.u64[5]  << (64 - d)) | (x.u64[4] >> d);
                x.u64[5]  = (x.u64[6]  << (64 - d)) | (x.u64[5] >> d);
                x.u64[6]  = (x.u64[7]  << (64 - d)) | (x.u64[6] >> d);
                x.u64[7]  = (x.u64[8]  << (64 - d)) | (x.u64[7] >> d);
                x.u64[8]  = (x.u64[9]  << (64 - d)) | (x.u64[8] >> d);
                x.u64[9]  = (x.u64[10] << (64 - d)) | (x.u64[9] >> d);
                x.u64[10] = (x.u64[11] << (64 - d)) | (x.u64[10] >> d);
                x.u64[11] = (x.u64[12] << (64 - d)) | (x.u64[11] >> d);
                x.u64[12] = (x.u64[13] << (64 - d)) | (x.u64[12] >> d);
                x.u64[13] = (x.u64[14] << (64 - d)) | (x.u64[13] >> d);
                x.u64[14] = (x.u64[15] << (64 - d)) | (x.u64[14] >> d);
                x.u64[15] = x.u64[15] >> d;
        } else {
                x.u64[0]  = x.u64[1];
                x.u64[1]  = x.u64[2];
                x.u64[2]  = x.u64[3];
                x.u64[3]  = x.u64[4];
                x.u64[4]  = x.u64[5];
                x.u64[5]  = x.u64[6];
                x.u64[6]  = x.u64[7];
                x.u64[7]  = x.u64[8];
                x.u64[8]  = x.u64[9];
                x.u64[9]  = x.u64[10];
                x.u64[10] = x.u64[11];
                x.u64[11] = x.u64[12];
                x.u64[12] = x.u64[13];
                x.u64[13] = x.u64[14];
                x.u64[14] = x.u64[15];
                x.u64[15] = 0;
                x = ASL_lsr_bits_1024(width, x, d - 64);
        }
        return x;
}

ASL_bits512_t
ASL_slice_lowd_1024_512(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_1024_512(ASL_slice_lowd_1024_1024(x, lo, width));
}

ASL_bits256_t
ASL_slice_lowd_1024_256(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_1024_256(ASL_slice_lowd_1024_1024(x, lo, width));
}

ASL_bits128_t
ASL_slice_lowd_1024_128(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_1024_128(ASL_slice_lowd_1024_1024(x, lo, width));
}

ASL_bits64_t
ASL_slice_lowd_1024_64(ASL_bits1024_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_1024_64(ASL_slice_lowd_1024_1024(x, lo, width));
}

ASL_bits1024_t
ASL_zero_extend_bits_64_1024(int width, ASL_bits64_t x, ASL_int_t n)
{
        return ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, x);
}

ASL_bits1024_t
ASL_zero_extend_bits_128_1024(int width, ASL_bits128_t x, ASL_int_t n)
{
        return ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, x.u64[1], x.u64[0]);
}

ASL_bits1024_t
ASL_zero_extend_bits_256_1024(int width, ASL_bits256_t x, ASL_int_t n)
{
        return ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits1024_t
ASL_zero_extend_bits_512_1024(int width, ASL_bits512_t x, ASL_int_t n)
{
        return ASL_bits_1024(0, 0, 0, 0,
                             0, 0, 0, 0,
                             x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                             x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits1024_t
ASL_zeros_bits_1024(ASL_int_t width)
{
        return ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0);
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
