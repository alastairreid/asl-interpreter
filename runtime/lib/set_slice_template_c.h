////////////////////////////////////////////////////////////////
// Slice assignment support for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#if M <= N && N != 64

#define ASL_BITS_TYPE_N ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)
#define ASL_BITS_TYPE_M ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, M), _t)

// Set bits i..i+w-1 of *l to r
void
ASL_set_slice(N, M, int l_width, ASL_BITS_TYPE_N *l, ASL_int_t i, int r_width, ASL_BITS_TYPE_M r)
{
        ASL_BITS_TYPE_N lsl    = ASL_lsl_bits(N, l_width, ASL_zero_extend_bits(M, N, r_width, r, N), i);
        ASL_BITS_TYPE_N mask   = ASL_lsl_bits(N, l_width, ASL_mk_mask(N, r_width), i);
        ASL_BITS_TYPE_N masked = ASL_and_bits(N, l_width, *l, ASL_not_bits(N, l_width, mask));
        *l = ASL_or_bits(N, l_width, masked, lsl);
}

#undef LIMB_M
#undef LIMB_N

#endif
