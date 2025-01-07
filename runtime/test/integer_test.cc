////////////////////////////////////////////////////////////////
// Tests for C runtime integer support library
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include <stdint.h>

#include "asl/integer.h"

#include "gtest/gtest.h"

class Integer : public ::testing::Test {
 protected:
};

TEST_F(Integer, MaskInt)
{
    EXPECT_EQ(1LL, ASL_mask_int(1LL));
#ifdef ASL_INT128
    EXPECT_EQ(ASL_int_128(0, UINT64_MAX), ASL_mask_int(64LL));
    EXPECT_EQ(ASL_int_128(1LL, UINT64_MAX), ASL_mask_int(65LL));
    EXPECT_EQ(-1LL, ASL_mask_int(128LL));
#else
    EXPECT_EQ(-1LL, ASL_mask_int(64LL));
#endif
}
