////////////////////////////////////////////////////////////////
// Tests for C runtime memory support library
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/ram.h"

#include "gtest/gtest.h"

class Memory : public ::testing::Test {
 protected:
    ASL_ram_t memory = ASL_ram_alloc();
    int64_t address_size = 0;  // unused

    void TearDown() override { ASL_ram_free(&memory); }
};

TEST_F(Memory, Init)
{
    ASL_ram_init(address_size, memory, 0);
    ASL_ram_init(address_size, memory, 0xff);
}

TEST_F(Memory, Read)
{
    uint64_t val = 0xf0ccac1adeadbeefUL;
    ASL_ram_init(address_size, memory, val);
    EXPECT_EQ(val & 0xff, ASL_ram_read(address_size, 1, memory, 0));
}

TEST_F(Memory, Write)
{
    int64_t size = 1;
    uint64_t address = 0;
    uint64_t val = 0xab;
    ASL_ram_write(address_size, size, memory, address, val);
    EXPECT_EQ(val, ASL_ram_read(address_size, size, memory, address));
}
