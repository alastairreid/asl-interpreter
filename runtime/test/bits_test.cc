////////////////////////////////////////////////////////////////
// Tests for C runtime bitvector support library
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"

#include "gtest/gtest.h"

class Bits64 : public ::testing::Test {
 protected:
    ASL_bits64_t zeros = 0;
    ASL_bits64_t ones = UINT64_MAX;
};

TEST_F(Bits64, Add)
{
    int width = 3;

    EXPECT_EQ(2ULL, ASL_add_bits_64(width, 1ULL, 1ULL));
    EXPECT_EQ(6ULL, ASL_add_bits_64(width, 7ULL, 7ULL));
    EXPECT_EQ(0ULL, ASL_add_bits_64(width, 1ULL, 7ULL));
}

TEST_F(Bits64, And)
{
    EXPECT_EQ(zeros, ASL_and_bits_64(64, zeros, zeros));
    EXPECT_EQ( ones, ASL_and_bits_64(64,  ones,  ones));
    EXPECT_EQ(zeros, ASL_and_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, Append)
{
    EXPECT_EQ(5ULL, ASL_append_bits_64(2, 1, 2ULL, 1ULL));
}

TEST_F(Bits64, Asr)
{
    ASL_int_t shift = 1;
    int width = 2;

    EXPECT_EQ(0ULL, ASL_asr_bits_64(width, 1ULL, shift));
    EXPECT_EQ(3ULL, ASL_asr_bits_64(width, 2ULL, shift));
}

TEST_F(Bits64, CvtBitsSInt)
{
    int width = 3;

    EXPECT_EQ(2LL, ASL_cvt_bits_sint_64(width, 2ULL));
    EXPECT_EQ(-4LL, ASL_cvt_bits_sint_64(width, 4ULL));
}

TEST_F(Bits64, CvtBitsUInt)
{
    int width = 3;

    EXPECT_EQ(2LL, ASL_cvt_bits_uint_64(width, 2ULL));
    EXPECT_EQ(4LL, ASL_cvt_bits_uint_64(width, 4ULL));
}

TEST_F(Bits64, CvtIntBits)
{
    int width = 3;

    EXPECT_EQ(6ULL, ASL_cvt_int_bits_64(width, 14LL));
    EXPECT_EQ(6ULL, ASL_cvt_int_bits_64(width, -2LL));
}

TEST_F(Bits64, Xor)
{
    EXPECT_EQ(zeros, ASL_xor_bits_64(64, zeros, zeros));
    EXPECT_EQ(zeros, ASL_xor_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_xor_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, Eq)
{
    EXPECT_FALSE(ASL_eq_bits_64(64, zeros, ones));
    EXPECT_TRUE(ASL_eq_bits_64(64, ones, ones));
}

TEST_F(Bits64, Lsl)
{
    ASL_int_t shift = 1;
    int width = 2;

    EXPECT_EQ(2ULL, ASL_lsl_bits_64(width, 3ULL, shift));
}

TEST_F(Bits64, Lsr)
{
    ASL_int_t shift = 1;
    int width = 64;

    EXPECT_EQ(1ULL, ASL_lsr_bits_64(width, 2ULL, shift));
}

TEST_F(Bits64, MkMask)
{
    EXPECT_EQ(1ULL, ASL_mk_mask_64(1));
    EXPECT_EQ(ones, ASL_mk_mask_64(64));
}

TEST_F(Bits64, Mul)
{
    int width = 3;

    EXPECT_EQ(1ULL, ASL_mul_bits_64(width, 1ULL, 1ULL));
    EXPECT_EQ(1ULL, ASL_mul_bits_64(width, 7ULL, 7ULL));
    EXPECT_EQ(7ULL, ASL_mul_bits_64(width, 1ULL, 7ULL));
    EXPECT_EQ(0ULL, ASL_mul_bits_64(width, 1ULL, 0ULL));
}

TEST_F(Bits64, Ne)
{
    EXPECT_TRUE(ASL_ne_bits_64(64, zeros, ones));
    EXPECT_FALSE(ASL_ne_bits_64(64, ones, ones));
}

TEST_F(Bits64, Not)
{
    int width = 63;
    ASL_bits64_t mask = ASL_mk_mask_64(width);

    EXPECT_EQ(zeros, ASL_not_bits_64(width, mask));
    EXPECT_EQ(mask, ASL_not_bits_64(width, zeros));
}

TEST_F(Bits64, Ones)
{
    EXPECT_EQ(7ULL, ASL_ones_bits_64(3));
}

TEST_F(Bits64, Or)
{
    EXPECT_EQ(zeros, ASL_or_bits_64(64, zeros, zeros));
    EXPECT_EQ( ones, ASL_or_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_or_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, Replicate)
{
    EXPECT_EQ(10ULL, ASL_replicate_bits_64(2, 2ULL, 2));
}

TEST_F(Bits64, SignExtend)
{
    int m = 32;
    int n = 63;
    ASL_bits64_t x = ASL_lsl_bits_64(m, 1ULL, m - 1);

    EXPECT_EQ(ASL_sign_extend_bits_64_64(m, x, n),
              ASL_lsl_bits_64(n, ASL_mk_mask_64(1 + n - m), m - 1));
}

TEST_F(Bits64, SignExtendMax)
{
    int m = 32;
    int n = 64;
    ASL_bits64_t x = ASL_lsl_bits_64(m, 1ULL, m - 1);

    EXPECT_EQ(ASL_sign_extend_bits_64_64(m, x, n),
              ASL_lsl_bits_64(n, ASL_mk_mask_64(1 + n - m), m - 1));
}

TEST_F(Bits64, SliceLoWd)
{
    EXPECT_EQ(2ULL, ASL_slice_lowd_64_64(11ULL, 2, 2));
}

TEST_F(Bits64, Sub)
{
    int width = 3;

    EXPECT_EQ(0ULL, ASL_sub_bits_64(width, 1ULL, 1ULL));
    EXPECT_EQ(7ULL, ASL_sub_bits_64(width, 0ULL, 1ULL));
    EXPECT_EQ(1ULL, ASL_sub_bits_64(width, 0ULL, 7ULL));
}

TEST_F(Bits64, Zeros)
{
    EXPECT_EQ(0ULL, ASL_zeros_bits_64(3));
}

template <class T>
static bool Equal(const T &x, const T &y) {
    for (unsigned i = 0; i < sizeof(x.u64) / sizeof(x.u64[0]); ++i)
        if (x.u64[i] != y.u64[i]) return false;
    return true;
}

template <class T>
static void Print(const T &x, std::ostream *os) {
    *os << std::hex << "(hex)" << std::setfill('0');
    for (int i = sizeof(x.u64) / sizeof(x.u64[0]) - 1; i >= 0; --i)
        *os << " " << std::setw(16) << x.u64[i];
}

#define N 128
#include "bits_test_template.h"
#undef N

#define N 256
#include "bits_test_template.h"
#undef N

#define N 512
#include "bits_test_template.h"
#undef N

#define N 1024
#include "bits_test_template.h"
#undef N

TEST_F(Bits128, Lsl)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    ASL_int_t shift = 1;
    int width = 64 + 2;

    EXPECT_EQ(ASL_bits_128(2, 6),
              ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, Lsl)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    ASL_int_t shift = 1;
    int width = 64 * 3 + 2;

    EXPECT_EQ(ASL_bits_256(2, 6, 6, 6),
              ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, Lsl)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 1;
    int width = 64 * 7 + 2;

    EXPECT_EQ(ASL_bits_512(2, 6, 6, 6, 6, 6, 6, 6),
              ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits1024, Lsl)
{
    ASL_bits1024_t x = ASL_bits_1024(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 1;
    int width = 64 * 15 + 2;

    EXPECT_EQ(ASL_bits_1024(2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
              ASL_lsl_bits_1024(width, x, shift));
}

TEST_F(Bits128, LslBy64)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    ASL_int_t shift = 64;
    int width = 64 + 1;

    EXPECT_EQ(ASL_bits_128(1, 0),
              ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, LslBy64)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    ASL_int_t shift = 64;
    int width = 64 * 3 + 1;

    EXPECT_EQ(ASL_bits_256(1, 3, 3, 0),
              ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, LslBy64)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 64;
    int width = 64 * 7 + 1;

    EXPECT_EQ(ASL_bits_512(1, 3, 3, 3, 3, 3, 3, 0),
              ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits1024, LslBy64)
{
    ASL_bits1024_t x = ASL_bits_1024(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 64;
    int width = 64 * 15 + 1;

    EXPECT_EQ(ASL_bits_1024(1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0),
              ASL_lsl_bits_1024(width, x, shift));
}

TEST_F(Bits128, LslGt64)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    ASL_int_t shift = 64 + 1;
    int width = 64 + 2;

    EXPECT_EQ(ASL_bits_128(2, 0),
              ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, LslGt64)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    ASL_int_t shift = 64 + 1;
    int width = 64 * 3 + 2;

    EXPECT_EQ(ASL_bits_256(2, 6, 6, 0),
              ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, LslGt64)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 64 + 1;
    int width = 64 * 7 + 2;

    EXPECT_EQ(ASL_bits_512(2, 6, 6, 6, 6, 6, 6, 0),
              ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits1024, LslGt64)
{
    ASL_bits1024_t x = ASL_bits_1024(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
    ASL_int_t shift = 64 + 1;
    int width = 64 * 15 + 2;

    EXPECT_EQ(ASL_bits_1024(2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 0),
              ASL_lsl_bits_1024(width, x, shift));
}

TEST_F(Bits128, Lsr)
{
    ASL_bits128_t x = ASL_bits_128(2, 2);
    ASL_int_t shift = 1;
    int width = 128;

    EXPECT_EQ(ASL_bits_128(1, 1),
              ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, Lsr)
{
    ASL_bits256_t x = ASL_bits_256(2, 2, 2, 2);
    ASL_int_t shift = 1;
    int width = 256;

    EXPECT_EQ(ASL_bits_256(1, 1, 1, 1),
              ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, Lsr)
{
    ASL_bits512_t x = ASL_bits_512(2, 2, 2, 2, 2, 2, 2, 2);
    ASL_int_t shift = 1;
    int width = 512;

    EXPECT_EQ(ASL_bits_512(1, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits1024, Lsr)
{
    ASL_bits1024_t x = ASL_bits_1024(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2);
    ASL_int_t shift = 1;
    int width = 1024;

    EXPECT_EQ(ASL_bits_1024(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_1024(width, x, shift));
}

TEST_F(Bits128, LsrBy64)
{
    ASL_bits128_t x = ASL_bits_128(1, 1);
    ASL_int_t shift = 64;
    int width = 128;

    EXPECT_EQ(ASL_bits_128(0, 1),
              ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, LsrBy64)
{
    ASL_bits256_t x = ASL_bits_256(1, 1, 1, 1);
    ASL_int_t shift = 64;
    int width = 256;

    EXPECT_EQ(ASL_bits_256(0, 1, 1, 1),
              ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, LsrBy64)
{
    ASL_bits512_t x = ASL_bits_512(1, 1, 1, 1, 1, 1, 1, 1);
    ASL_int_t shift = 64;
    int width = 512;

    EXPECT_EQ(ASL_bits_512(0, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits1024, LsrBy64)
{
    ASL_bits1024_t x = ASL_bits_1024(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    ASL_int_t shift = 64;
    int width = 1024;

    EXPECT_EQ(ASL_bits_1024(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_1024(width, x, shift));
}

TEST_F(Bits128, LsrGt64)
{
    ASL_bits128_t x = ASL_bits_128(2, 2);
    ASL_int_t shift = 64 + 1;
    int width = 128;

    EXPECT_EQ(ASL_bits_128(0, 1),
              ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, LsrGt64)
{
    ASL_bits256_t x = ASL_bits_256(2, 2, 2, 2);
    ASL_int_t shift = 64 + 1;
    int width = 256;

    EXPECT_EQ(ASL_bits_256(0, 1, 1, 1),
              ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, LsrGt64)
{
    ASL_bits512_t x = ASL_bits_512(2, 2, 2, 2, 2, 2, 2, 2);
    ASL_int_t shift = 64 + 1;
    int width = 512;

    EXPECT_EQ(ASL_bits_512(0, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits1024, LsrGt64)
{
    ASL_bits1024_t x = ASL_bits_1024(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2);
    ASL_int_t shift = 64 + 1;
    int width = 1024;

    EXPECT_EQ(ASL_bits_1024(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
              ASL_lsr_bits_1024(width, x, shift));
}

TEST_F(Bits128, MkMask)
{
    EXPECT_EQ(ASL_bits_128(1, UINT64_MAX),
              ASL_mk_mask_128(1 + 64));
    EXPECT_EQ(ASL_bits_max_128(),
              ASL_mk_mask_128(128));
}

TEST_F(Bits256, MkMask)
{
    EXPECT_EQ(ASL_bits_256(0, 1, UINT64_MAX, UINT64_MAX),
              ASL_mk_mask_256(1 + 128));
    EXPECT_EQ(ASL_bits_max_256(),
              ASL_mk_mask_256(256));
}

TEST_F(Bits512, MkMask)
{
    EXPECT_EQ(ASL_bits_512(0, 0, 0, 1, UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX),
              ASL_mk_mask_512(1 + 256));
    EXPECT_EQ(ASL_bits_max_512(),
              ASL_mk_mask_512(512));
}

TEST_F(Bits1024, MkMask)
{
    EXPECT_EQ(ASL_bits_1024(0, 0, 0, 0,
                            0, 0, 0, 1,
                            UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                            UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX),
              ASL_mk_mask_1024(1 + 512));
    EXPECT_EQ(ASL_bits_max_1024(),
              ASL_mk_mask_1024(1024));
}

TEST_F(Bits128, Zeros)
{
    EXPECT_EQ(ASL_bits_128(0, 0),
              ASL_zeros_bits_128(1 + 64));
}

TEST_F(Bits256, Zeros)
{
    EXPECT_EQ(ASL_bits_256(0, 0, 0, 0),
              ASL_zeros_bits_256(1 + 128));
}

TEST_F(Bits512, Zeros)
{
    EXPECT_EQ(ASL_bits_512(0, 0, 0, 0, 0, 0, 0, 0),
              ASL_zeros_bits_512(1 + 256));
}

TEST_F(Bits1024, Zeros)
{
    EXPECT_EQ(ASL_bits_1024(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              ASL_zeros_bits_1024(1 + 512));
}
