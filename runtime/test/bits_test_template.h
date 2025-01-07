////////////////////////////////////////////////////////////////
// Tests for C runtime bitvector support library
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#define ASL_BITS_LIMBS_64 (N >> 6)
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

class ASL_CC_INDIR(Bits, N) : public ::testing::Test {
 protected:
    ASL_BITS_TYPE zeros = ASL_zeros_bits(N, N);
    ASL_BITS_TYPE ones = ASL_bits_max(N);
};

bool operator==(const ASL_BITS_TYPE &x, const ASL_BITS_TYPE &y) {
    return Equal(x, y);
}

void PrintTo(const ASL_BITS_TYPE &x, std::ostream *os) {
    Print(x, os);
}

TEST_F(ASL_CC_INDIR(Bits, N), Add)
{
    int width = N - 1;
    ASL_BITS_TYPE one = ASL_mk_mask(N, 1);
    ASL_BITS_TYPE minus_one = ASL_mk_mask(N, width);
    ASL_BITS_TYPE two = ASL_lsl_bits(N, width, one, 1);
    ASL_BITS_TYPE minus_two = ASL_not_bits(N, width, one);

    EXPECT_EQ(two, ASL_add_bits(N, width, one, one));
    EXPECT_EQ(minus_two, ASL_add_bits(N, width, minus_one, minus_one));
    EXPECT_EQ(zeros, ASL_add_bits(N, width, minus_one, one));
}

TEST_F(ASL_CC_INDIR(Bits, N), And)
{
    EXPECT_EQ(zeros, ASL_and_bits(N, N, zeros, zeros));
    EXPECT_EQ( ones, ASL_and_bits(N, N,  ones,  ones));
    EXPECT_EQ(zeros, ASL_and_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Append)
{
    EXPECT_EQ(ones,
              ASL_append_bits(N, N - 1, 1,
                              ASL_mk_mask(N, N - 1), ASL_mk_mask(N, 1)));
}

TEST_F(ASL_CC_INDIR(Bits, N), Asr)
{
    ASL_int_t shift = 1;
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);

    EXPECT_EQ(zeros, ASL_asr_bits(N, width, zeros, shift));
    EXPECT_EQ(x, ASL_asr_bits(N, width, x, shift));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtBitsSInt)
{
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);

    EXPECT_EQ(-1LL, ASL_cvt_bits_sint(N, width, x));
    EXPECT_EQ(0LL, ASL_cvt_bits_sint(N, width, zeros));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtBitsUInt)
{
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);
#ifdef ASL_INT128
    ASL_int_t r = ASL_int_128(x.u64[1], x.u64[0]);
#else
    ASL_int_t r = x.u64[0];
#endif

    EXPECT_EQ(r, ASL_cvt_bits_uint(N, width, x));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtIntBits)
{
    int width = N - 1;
    ASL_BITS_TYPE r = ASL_mk_mask(N, width);

    EXPECT_EQ(r, ASL_cvt_int_bits(N, width, -1LL));
}

TEST_F(ASL_CC_INDIR(Bits, N), Eor)
{
    EXPECT_EQ(zeros, ASL_eor_bits(N, N, zeros, zeros));
    EXPECT_EQ(zeros, ASL_eor_bits(N, N,  ones,  ones));
    EXPECT_EQ( ones, ASL_eor_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Eq)
{
    EXPECT_FALSE(ASL_eq_bits(N, N, zeros, ones));
    EXPECT_TRUE(ASL_eq_bits(N, N, ones, ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Mul)
{
    int width = N - 2;
    ASL_BITS_TYPE one = ASL_mk_mask(N, 1);
    ASL_BITS_TYPE minus_one = ASL_mk_mask(N, width);

    EXPECT_EQ(one, ASL_mul_bits(N, width, one, one));
    EXPECT_EQ(one, ASL_mul_bits(N, width, minus_one, minus_one));
    EXPECT_EQ(minus_one, ASL_mul_bits(N, width, one, minus_one));
    EXPECT_EQ(zeros, ASL_mul_bits(N, width, one, zeros));
}

TEST_F(ASL_CC_INDIR(Bits, N), Ne)
{
    EXPECT_TRUE(ASL_ne_bits(N, N, zeros, ones));
    EXPECT_FALSE(ASL_ne_bits(N, N, ones, ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Not)
{
    int width = N - 1;
    ASL_BITS_TYPE mask = ASL_mk_mask(N, width);

    EXPECT_EQ(zeros, ASL_not_bits(N, width, mask));
    EXPECT_EQ(mask, ASL_not_bits(N, width, zeros));
}

TEST_F(ASL_CC_INDIR(Bits, N), Ones)
{
    ASL_int_t width = N - 1;
    ASL_BITS_TYPE mask = ASL_mk_mask(N, width);

    EXPECT_EQ(mask, ASL_ones_bits(N, width));
}

TEST_F(ASL_CC_INDIR(Bits, N), Or)
{
    EXPECT_EQ(zeros, ASL_or_bits(N, N, zeros, zeros));
    EXPECT_EQ( ones, ASL_or_bits(N, N,  ones,  ones));
    EXPECT_EQ( ones, ASL_or_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Replicate)
{
    EXPECT_EQ(ASL_mk_mask(N, 32 * ((N >> 5) - 1)),
              ASL_replicate_bits(N, 32, ASL_mk_mask(N, 32), (N >> 5) - 1));
}

TEST_F(ASL_CC_INDIR(Bits, N), SliceLoWd)
{
    ASL_int_t width = N - 1;

    EXPECT_EQ(ASL_mk_mask(N, width - 1),
              ASL_slice_lowd(N, N, ASL_mk_mask(N, width), 1, width));
}

TEST_F(ASL_CC_INDIR(Bits, N), Sub)
{
    int width = N - 1;
    ASL_BITS_TYPE one = ASL_mk_mask(N, 1);
    ASL_BITS_TYPE minus_one = ASL_mk_mask(N, width);

    EXPECT_EQ(zeros, ASL_sub_bits(N, width, one, one));
    EXPECT_EQ(minus_one, ASL_sub_bits(N, width, zeros, one));
    EXPECT_EQ(one, ASL_sub_bits(N, width, zeros, minus_one));
}

#define M 64
#include "sign_extend_bits_test_template.h"
#undef M

#define M 128
#include "sign_extend_bits_test_template.h"
#undef M

#define M 256
#include "sign_extend_bits_test_template.h"
#undef M

#define M 512
#include "sign_extend_bits_test_template.h"
#undef M

#define M 1024
#include "sign_extend_bits_test_template.h"
#undef M

#undef ASL_BITS_LIMBS_64
#undef ASL_BITS_TYPE
