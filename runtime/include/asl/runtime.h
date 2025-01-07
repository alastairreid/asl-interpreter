////////////////////////////////////////////////////////////////
// Runtime library for ASL's C backend
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_RUNTIME_H
#define ASL_RUNTIME_H

#if defined(ASL_C23)

#include <stdio.h>
#include <stdint.h>
#include <limits.h>

// The following types are used in the foreign function interface
typedef __int128 ASL_int_t;
typedef uint8_t ASL_bits8_t;
typedef uint16_t ASL_bits16_t;
typedef uint32_t ASL_bits32_t;
typedef uint64_t ASL_bits64_t;
typedef unsigned __int128 ASL_bits128_t;
#if BITINT_MAXWIDTH >= 256
typedef unsigned _BitInt(256) ASL_bits256_t;
#endif
#if BITINT_MAXWIDTH >= 256
typedef unsigned _BitInt(512) ASL_bits512_t;
#endif

#define ASL_cast_bits_512_64(x) ((ASL_bits64_t)(x))
#define ASL_cast_bits_256_64(x) ((ASL_bits64_t)(x))
#define ASL_cast_bits_128_64(x) ((ASL_bits64_t)(x))

#else // defined(ASL_FALLBACK)

// The following types are used in the foreign function interface
// Note that they are all 64 bits wide in this runtime.
#include <stdint.h>
typedef uint64_t ASL_bits8_t;
typedef uint64_t ASL_bits16_t;
typedef uint64_t ASL_bits32_t;

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"
#include "asl/integer.h"
#include "asl/print.h"

#endif // ASL_FALLBACK

#include "asl/error.h"
#include "asl/ram.h"
#include "asl/track_valid.h"

#endif  // ASL_RUNTIME_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
