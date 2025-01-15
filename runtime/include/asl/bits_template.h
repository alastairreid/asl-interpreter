////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

ASL_BITS_TYPE ASL_add_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_and_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_append_bits(N, int x_width, int y_width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_asr_bits(N, int width, ASL_BITS_TYPE x, ASL_int_t d);
ASL_int_t ASL_cvt_bits_sint(N, int width, ASL_BITS_TYPE x);
ASL_int_t ASL_cvt_bits_uint(N, int width, ASL_BITS_TYPE x);
ASL_BITS_TYPE ASL_cvt_int_bits(N, int width, ASL_int_t x);
ASL_BITS_TYPE ASL_xor_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
bool ASL_eq_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_mk_mask(N, ASL_int_t width);
bool ASL_ne_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_mul_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_not_bits(N, int width, ASL_BITS_TYPE x);
ASL_BITS_TYPE ASL_ones_bits(N, ASL_int_t width);
ASL_BITS_TYPE ASL_or_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_replicate_bits(N, int width, ASL_BITS_TYPE x, ASL_int_t n);
ASL_BITS_TYPE ASL_slice_lowd(N, N, ASL_BITS_TYPE x, ASL_int_t lo, ASL_int_t width);
ASL_BITS_TYPE ASL_sub_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_zero_extend_bits(N, N, int width, ASL_BITS_TYPE x, ASL_int_t n);
ASL_BITS_TYPE ASL_sign_extend_bits(N, N, int width, ASL_BITS_TYPE x, ASL_int_t n);

#define M 64
#include "asl/set_slice_template.h"
#undef M
#define M 128
#include "asl/set_slice_template.h"
#undef M
#define M 256
#include "asl/set_slice_template.h"
#undef M
#define M 512
#include "asl/set_slice_template.h"
#undef M
#define M 1024
#include "asl/set_slice_template.h"
#undef M

#undef ASL_BITS_TYPE
