////////////////////////////////////////////////////////////////
// Sign extend support for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#if M <= N

#define ASL_BITS_TYPE_M ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, M), _t)

#if M == 64
#define LIMB_M(x, i) x
#else
#define LIMB_M(x, i) x.u64[i]
#endif

#if N == 64
#define LIMB_N(x, i) x
#else
#define LIMB_N(x, i) x.u64[i]
#endif

ASL_BITS_TYPE
ASL_sign_extend_bits(M, N, int width, ASL_BITS_TYPE_M x, ASL_int_t n)
{
        ASL_BITS_TYPE r = ASL_zero_extend_bits(M, N, width, x, n);

        int lead_limb = (width - 1) >> 6;
        int lead_msb = (width - 1) & 63;
        uint64_t lead = LIMB_M(x, lead_limb);
        bool is_negative = (lead & ((uint64_t)1 << lead_msb)) != 0;

        if (is_negative) {
                int lead_res_limb = ((int)n - 1) >> 6;
                // set lead bits to 1
                ASL_bits64_t ext = ~ASL_mk_mask_64(width - (lead_limb << 6));
                LIMB_N(r, lead_limb) |= ext;
                // fill in higher order sign bits
                for (int i = lead_limb + 1; i <= lead_res_limb; ++i) {
                        LIMB_N(r, i) = UINT64_MAX;
                }
                // zero any top bits
                LIMB_N(r, lead_res_limb) &=
                        ASL_mk_mask_64(n - (lead_res_limb << 6));
        }
        return r;
}

#undef ASL_BITS_TYPE_M
#undef LIMB_M
#undef LIMB_N

#endif
