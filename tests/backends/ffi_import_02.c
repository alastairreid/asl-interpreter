// FFI testing support functions to be imported into ISA test programs
// Copyright (C) 2025-2026 Intel Corporation

#include <assert.h>
#include <stdint.h>

#include "isa_ffi.h"

static int64_t x = 0;

int64_t v1_Read(void)
{
        return x + 1;
}

void v1_Write(int64_t value)
{
        x = value + 1;
}

#define SIZE 1
static int64_t y[SIZE] = { 0 };

int64_t v2_Read(int64_t i)
{
        assert(i >= 0 && i < SIZE);
        return y[i] + 1;
}

void v2_Write(int64_t i, int64_t value)
{
        assert(i >= 0 && i < SIZE);
        y[i] = value + 1;
}
