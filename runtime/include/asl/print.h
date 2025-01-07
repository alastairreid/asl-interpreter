////////////////////////////////////////////////////////////////
// Runtime print support for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_PRINT_H
#define ASL_PRINT_H

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"
#include "asl/integer.h"

#ifdef __cplusplus
extern "C" {
#endif

#define ASL_print_bits_hex(sizeof_x, n, x) \
        ASL_CC(ASL_print_bits_hex_, sizeof_x)(n, x)

static inline void
ASL_print_bits_hex_64(int width, ASL_bits64_t x)
{
        // as a special case, we don't zero pad small bitvectors
        printf("%d'x", width);
        printf("%llx", (long long)x);
}

#define N 128
#include "asl/print_template.h"
#undef N

#define N 256
#include "asl/print_template.h"
#undef N

#define N 512
#include "asl/print_template.h"
#undef N

#define N 1024
#include "asl/print_template.h"
#undef N

static inline void
ASL_print_char(ASL_int_t x)
{
        putchar(x);
}

static inline void
ASL_print_int_hex(int n, bool add_size, ASL_int_t x)
{
#ifdef ASL_INT128
        if (x < 0) {
                printf("-");
        }
        if (add_size) {
                printf("i%d'x", n);
        } else {
                printf("0x");
        }
        int64_t top = (int64_t)(x >> 64);
        int64_t bottom = (int64_t)x;
        if (top == 0 && bottom >= 0) {
            printf("%" PRIx64, bottom);
        } else if (top == -1 && bottom < 0 && bottom != INT64_MIN) {
            printf("%" PRIx64, -bottom);
        } else {
            printf("%08" PRIx64 "_%08" PRIx64, top, bottom);
        }
#else
        if (x < 0) {
                printf("-");
        }
        if (add_size) {
                printf("i%d'x", n);
        } else {
                printf("0x");
        }
        if (x == INT64_MIN) {
                printf("8000000000000000");
        } else if (x < 0) {
                printf("%" PRIx64, (long long)-x);
        } else {
                printf("%" PRIx64, (long long)x);
        }
#endif
}

static inline void
ASL_print_int_dec(int n, bool add_size, ASL_int_t x)
{
#ifdef ASL_INT128
        if (x < 0) {
                printf("-");
        }
        if (add_size) {
                printf("i%d'd", n);
        }
        int64_t top = (int64_t)(x >> 64);
        int64_t bottom = (int64_t)x;
        if (top == 0 && bottom >= 0) {
                printf("%" PRId64, bottom);
        } else if (top == -1 && bottom < 0 && bottom != INT64_MIN) {
                printf("%" PRId64, -bottom);
        } else {
                // despite the name, large numbers are printed in hex
                printf("x%08" PRIx64 "_%08" PRIx64, top, bottom);
        }
#else
        if (x < 0) {
                printf("-");
        }
        if (add_size) {
                printf("i%d'", n);
        }
        if (x == INT64_MIN) {
                printf("9223372036854775808");
        } else {
                printf("%" PRId64, (long long)x);
        }
#endif
}

static inline void
ASL_print_str(const char* x)
{
        printf("%s", x);
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_PRINT_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
