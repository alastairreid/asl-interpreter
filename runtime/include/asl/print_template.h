////////////////////////////////////////////////////////////////
// Runtime print support for ASL's C backend
//
// Copyright (C) 2023-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#define ASL_BITS_LIMBS_64 (N >> 6)
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

static inline void
ASL_print_bits_hex(N, int width, ASL_BITS_TYPE x)
{
        printf("%d'x", width);
        bool leading = true; // suppress leading zeros
        for (int i = ASL_BITS_LIMBS_64 - 1; i >= 0; --i) {
                if (leading) {
                        if (i == 0 || x.u64[i]) {
                                printf("%llx", (long long)x.u64[i]);
                                leading = false;
                        }
                } else {
                    printf("%08llx", (long long)x.u64[i]);
                }
        }
}

#undef ASL_BITS_LIMBS_64
#undef ASL_BITS_TYPE
