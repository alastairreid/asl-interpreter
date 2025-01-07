////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits512.h"

#include <stdbool.h>

#define N 512
#include "bits_template_c.h"
#undef N

ASL_bits512_t
ASL_lsl_bits_512(int width, ASL_bits512_t x, ASL_int_t d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[7] = (x.u64[6] >> (64 - d)) | (x.u64[7] << d);
                x.u64[6] = (x.u64[5] >> (64 - d)) | (x.u64[6] << d);
                x.u64[5] = (x.u64[4] >> (64 - d)) | (x.u64[5] << d);
                x.u64[4] = (x.u64[3] >> (64 - d)) | (x.u64[4] << d);
                x.u64[3] = (x.u64[2] >> (64 - d)) | (x.u64[3] << d);
                x.u64[2] = (x.u64[1] >> (64 - d)) | (x.u64[2] << d);
                x.u64[1] = (x.u64[0] >> (64 - d)) | (x.u64[1] << d);
                x.u64[0] = x.u64[0] << d;
        } else {
                x.u64[7] = x.u64[6];
                x.u64[6] = x.u64[5];
                x.u64[5] = x.u64[4];
                x.u64[4] = x.u64[3];
                x.u64[3] = x.u64[2];
                x.u64[2] = x.u64[1];
                x.u64[1] = x.u64[0];
                x.u64[0] = 0;
                x = ASL_lsl_bits_512(width, x, d - 64);
        }
        return ASL_and_bits_512(width, x, ASL_mk_mask_512(width));
}

ASL_bits512_t
ASL_lsr_bits_512(int width, ASL_bits512_t x, ASL_int_t d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[0] = (x.u64[1] << (64 - d)) | (x.u64[0] >> d);
                x.u64[1] = (x.u64[2] << (64 - d)) | (x.u64[1] >> d);
                x.u64[2] = (x.u64[3] << (64 - d)) | (x.u64[2] >> d);
                x.u64[3] = (x.u64[4] << (64 - d)) | (x.u64[3] >> d);
                x.u64[4] = (x.u64[5] << (64 - d)) | (x.u64[4] >> d);
                x.u64[5] = (x.u64[6] << (64 - d)) | (x.u64[5] >> d);
                x.u64[6] = (x.u64[7] << (64 - d)) | (x.u64[6] >> d);
                x.u64[7] = x.u64[7] >> d;
        } else {
                x.u64[0] = x.u64[1];
                x.u64[1] = x.u64[2];
                x.u64[2] = x.u64[3];
                x.u64[3] = x.u64[4];
                x.u64[4] = x.u64[5];
                x.u64[5] = x.u64[6];
                x.u64[6] = x.u64[7];
                x.u64[7] = 0;
                x = ASL_lsr_bits_512(width, x, d - 64);
        }
        return x;
}

ASL_bits256_t
ASL_slice_lowd_512_256(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_512_256(ASL_slice_lowd_512_512(x, lo, width));
}

ASL_bits128_t
ASL_slice_lowd_512_128(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_512_128(ASL_slice_lowd_512_512(x, lo, width));
}

ASL_bits64_t
ASL_slice_lowd_512_64(ASL_bits512_t x, ASL_int_t lo, ASL_int_t width)
{
        return ASL_cast_bits_512_64(ASL_slice_lowd_512_512(x, lo, width));
}

ASL_bits512_t
ASL_zero_extend_bits_64_512(int width, ASL_bits64_t x, ASL_int_t n)
{
        return ASL_bits_512(0, 0, 0, 0, 0, 0, 0, x);
}

ASL_bits512_t
ASL_zero_extend_bits_128_512(int width, ASL_bits128_t x, ASL_int_t n)
{
        return ASL_bits_512(0, 0, 0, 0, 0, 0, x.u64[1], x.u64[0]);
}

ASL_bits512_t
ASL_zero_extend_bits_256_512(int width, ASL_bits256_t x, ASL_int_t n)
{
        return ASL_bits_512(0, 0, 0, 0, x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits512_t
ASL_zeros_bits_512(ASL_int_t width)
{
        return ASL_bits_512(0, 0, 0, 0, 0, 0, 0, 0);
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
