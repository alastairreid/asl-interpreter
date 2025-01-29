////////////////////////////////////////////////////////////////
// Runtime library for ASL's CPP backend
//
// Copyright (C) 2024-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#pragma once

#include "asl/error.h"
#include "asl/ram.h"
#include <sstream>

#if defined(ASL_AC)
#include <ac_int.h>

namespace asl
{
    template <int T, int SIZE_IN=1, bool S = false>
    inline ac_int<T*SIZE_IN, S> replicate(int n, const ac_int<SIZE_IN, S> &x)
    {
        ac_int<T*SIZE_IN, S> r = 0;
        while (n-- > 0)
                r = (r << SIZE_IN) | x;
        return r;
    }
}

typedef ac_int<128,true> ASL_int_t;
#endif // ASL_AC

#if defined(ASL_SC)
#include <systemc.h>

typedef sc_bigint<128> ASL_int_t;
#endif // ASL_SC

static inline void
ASL_print_int_dec(ASL_int_t x)
{
    std::cout << x;
}

static inline void
ASL_print_int_hex(ASL_int_t x)
{
    // Most of the work here is converting to lower case
    // because ac_int does not support std::nouppercase
    std::stringstream buffer;
    if (x >= 0) {
        buffer << std::hex << x;
    } else {
        buffer << "-" << std::hex << -x;
    }
    std::string s = buffer.str();
    for (auto &c : s) {
        std::cout << (char)tolower(c);
    }
}

#if defined(ASL_AC)
template <int N>
static inline void
ASL_print_bits_hex(ac_int<N,false> x)
{
    // Most of the work here is converting to lower case
    // because ac_int does not support std::nouppercase
    std::stringstream buffer;
    buffer << std::hex << x;
    std::string s = buffer.str().substr(2);

    std::cout << N << "'x";
    bool leading_zero_suppress = true;
    for (auto &c : s) {
        if (!leading_zero_suppress || c != '0') {
            leading_zero_suppress = false;
            std::cout << (char)tolower(c);
        }
    }
    if (leading_zero_suppress) {
        std::cout << '0';
    }
}
#endif // ASL_AC

#if defined(ASL_SC)
namespace asl
{
    template <typename T, int num_bits>
    inline T sc_bit_fill(const int (&x)[(num_bits + 31)/32])
    {
        T r = 0;
        for (int i = 0; i+31 < num_bits; i += 32) {
            r.range(i+31, i) = x[i/32];
        }
        if (num_bits % 32) {
            r.range(num_bits-1, 32*(num_bits / 32)) = x[num_bits/32];
        }
        return r;
    }
}
#endif // ASL_SC

static inline void
ASL_print_char(ASL_int_t x)
{
    std::cout << (char)(x.to_int());
}

static inline void
ASL_print_str(const char* x)
{
    std::cout << x;
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
