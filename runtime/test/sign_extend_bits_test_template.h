////////////////////////////////////////////////////////////////
// Tests for C runtime bitvector support library
//
// Copyright (C) 2024-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#if M <= N

#define ASL_BITS_TYPE_M ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, M), _t)

TEST_F(ASL_CC_INDIR(Bits, N), ASL_CC_INDIR(SignExtend, M))
{
    int m = M - 1;
    int n = N - 1;

    // x = 0b010..00
    //        ^ sign at bit m-1
    ASL_BITS_TYPE_M x = ASL_lsl_bits(M, m, ASL_mk_mask(M, 1), m - 1);

    EXPECT_EQ(ASL_sign_extend_bits(M, N, m, x, n),
              ASL_lsl_bits(N, n, ASL_mk_mask(N, 1 + n - m), m - 1));
}

TEST_F(ASL_CC_INDIR(Bits, N), ASL_CC_INDIR(SignExtendMax, M))
{
    int m = M;
    int n = N;

    // x = 0b10..00
    //       ^ sign at bit m-1
    ASL_BITS_TYPE_M x = ASL_lsl_bits(M, m, ASL_mk_mask(M, 1), m - 1);

    EXPECT_EQ(ASL_sign_extend_bits(M, N, m, x, n),
              ASL_lsl_bits(N, n, ASL_mk_mask(N, 1 + n - m), m - 1));
}

#if N - M > 64

TEST_F(ASL_CC_INDIR(Bits, N), ASL_CC_INDIR(SignExtendPrevLimb, M))
{
    int m = M - 1;
    int n = N - 64 - 1;

    ASL_BITS_TYPE_M x = ASL_lsl_bits(M, m, ASL_mk_mask(M, 1), m - 1);

    EXPECT_EQ(ASL_sign_extend_bits(M, N, m, x, n),
              ASL_lsl_bits(N, n, ASL_mk_mask(N, 1 + n - m), m - 1));
}

#endif

#undef ASL_BITS_TYPE_M

#endif
