////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright (C) 2023-2026 Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS8192_H
#define ASL_BITS8192_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"
#include "asl/bits2048.h"
#include "asl/bits4096.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef union {
        uint64_t u64[128];
        uint32_t u32[256];
} ASL_bits8192_t;

static inline ASL_bits8192_t
ASL_bits_8192(
              uint64_t v127, uint64_t v126, uint64_t v125, uint64_t v124,
              uint64_t v123, uint64_t v122, uint64_t v121, uint64_t v120,
              uint64_t v119, uint64_t v118, uint64_t v117, uint64_t v116,
              uint64_t v115, uint64_t v114, uint64_t v113, uint64_t v112,
              uint64_t v111, uint64_t v110, uint64_t v109, uint64_t v108,
              uint64_t v107, uint64_t v106, uint64_t v105, uint64_t v104,
              uint64_t v103, uint64_t v102, uint64_t v101, uint64_t v100,
              uint64_t v99, uint64_t v98, uint64_t v97, uint64_t v96,
              uint64_t v95, uint64_t v94, uint64_t v93, uint64_t v92,
              uint64_t v91, uint64_t v90, uint64_t v89, uint64_t v88,
              uint64_t v87, uint64_t v86, uint64_t v85, uint64_t v84,
              uint64_t v83, uint64_t v82, uint64_t v81, uint64_t v80,
              uint64_t v79, uint64_t v78, uint64_t v77, uint64_t v76,
              uint64_t v75, uint64_t v74, uint64_t v73, uint64_t v72,
              uint64_t v71, uint64_t v70, uint64_t v69, uint64_t v68,
              uint64_t v67, uint64_t v66, uint64_t v65, uint64_t v64,
              uint64_t v63, uint64_t v62, uint64_t v61, uint64_t v60,
              uint64_t v59, uint64_t v58, uint64_t v57, uint64_t v56,
              uint64_t v55, uint64_t v54, uint64_t v53, uint64_t v52,
              uint64_t v51, uint64_t v50, uint64_t v49, uint64_t v48,
              uint64_t v47, uint64_t v46, uint64_t v45, uint64_t v44,
              uint64_t v43, uint64_t v42, uint64_t v41, uint64_t v40,
              uint64_t v39, uint64_t v38, uint64_t v37, uint64_t v36,
              uint64_t v35, uint64_t v34, uint64_t v33, uint64_t v32,
              uint64_t v31, uint64_t v30, uint64_t v29, uint64_t v28,
              uint64_t v27, uint64_t v26, uint64_t v25, uint64_t v24,
              uint64_t v23, uint64_t v22, uint64_t v21, uint64_t v20,
              uint64_t v19, uint64_t v18, uint64_t v17, uint64_t v16,
              uint64_t v15, uint64_t v14, uint64_t v13, uint64_t v12,
              uint64_t v11, uint64_t v10, uint64_t v9, uint64_t v8,
              uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4,
              uint64_t v3, uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_bits8192_t){ { v0, v1, v2, v3, v4, v5, v6, v7,
                                   v8, v9, v10, v11, v12, v13, v14, v15,
                                   v16, v17, v18, v19, v20, v21, v22, v23,
                                   v24, v25, v26, v27, v28, v29, v30, v31,
                                   v32, v33, v34, v35, v36, v37, v38, v39,
                                   v40, v41, v42, v43, v44, v45, v46, v47,
                                   v48, v49, v50, v51, v52, v53, v54, v55,
                                   v56, v57, v58, v59, v60, v61, v62, v63,
                                   v64, v65, v66, v67, v68, v69, v70, v71,
                                   v72, v73, v74, v75, v76, v77, v78, v79,
                                   v80, v81, v82, v83, v84, v85, v86, v87,
                                   v88, v89, v90, v91, v92, v93, v94, v95,
                                   v96, v97, v98, v99, v100, v101, v102, v103,
                                   v104, v105, v106, v107, v108, v109, v110, v111,
                                   v112, v113, v114, v115, v116, v117, v118, v119,
                                   v120, v121, v122, v123, v124, v125, v126, v127 } };
}

static inline ASL_bits8192_t
ASL_bits_max_8192()
{
        return ASL_bits_8192(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                             UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits8192_t
ASL_bits_zero_8192()
{
        return ASL_bits_8192(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#define N 8192
#include "asl/bits_template.h"
#undef N

static inline ASL_bits64_t
ASL_cast_bits_8192_64(ASL_bits8192_t x)
{
        return x.u64[0];
}

static inline ASL_bits128_t
ASL_cast_bits_8192_128(ASL_bits8192_t x)
{
        return ASL_bits_128(x.u64[1], x.u64[0]);
}

static inline ASL_bits256_t
ASL_cast_bits_8192_256(ASL_bits8192_t x)
{
        return ASL_bits_256(x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits512_t
ASL_cast_bits_8192_512(ASL_bits8192_t x)
{
        return ASL_bits_512(x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                            x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits1024_t
ASL_cast_bits_8192_1024(ASL_bits8192_t x)
{
        return ASL_bits_1024(x.u64[15], x.u64[14], x.u64[13], x.u64[12],
                             x.u64[11], x.u64[10], x.u64[9], x.u64[8],
                             x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                             x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits2048_t
ASL_cast_bits_8192_2048(ASL_bits8192_t x)
{
        return ASL_bits_2048(x.u64[31], x.u64[30], x.u64[29], x.u64[28],
                             x.u64[27], x.u64[26], x.u64[25], x.u64[24],
                             x.u64[23], x.u64[22], x.u64[21], x.u64[20],
                             x.u64[19], x.u64[18], x.u64[17], x.u64[16],
                             x.u64[15], x.u64[14], x.u64[13], x.u64[12],
                             x.u64[11], x.u64[10], x.u64[9], x.u64[8],
                             x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                             x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

static inline ASL_bits4096_t
ASL_cast_bits_8192_4096(ASL_bits8192_t x)
{
        return ASL_bits_4096(
                             x.u64[63], x.u64[62], x.u64[61], x.u64[60],
                             x.u64[59], x.u64[58], x.u64[57], x.u64[56],
                             x.u64[55], x.u64[54], x.u64[53], x.u64[52],
                             x.u64[51], x.u64[50], x.u64[49], x.u64[48],
                             x.u64[47], x.u64[46], x.u64[45], x.u64[44],
                             x.u64[43], x.u64[42], x.u64[41], x.u64[40],
                             x.u64[39], x.u64[38], x.u64[37], x.u64[36],
                             x.u64[35], x.u64[34], x.u64[33], x.u64[32],
                             x.u64[31], x.u64[30], x.u64[29], x.u64[28],
                             x.u64[27], x.u64[26], x.u64[25], x.u64[24],
                             x.u64[23], x.u64[22], x.u64[21], x.u64[20],
                             x.u64[19], x.u64[18], x.u64[17], x.u64[16],
                             x.u64[15], x.u64[14], x.u64[13], x.u64[12],
                             x.u64[11], x.u64[10], x.u64[9], x.u64[8],
                             x.u64[7], x.u64[6], x.u64[5], x.u64[4],
                             x.u64[3], x.u64[2], x.u64[1], x.u64[0]);
}

ASL_bits8192_t ASL_lsl_bits_8192(int width, ASL_bits8192_t x, ASL_int_t d);
ASL_bits8192_t ASL_lsr_bits_8192(int width, ASL_bits8192_t x, ASL_int_t d);
ASL_bits2048_t ASL_slice_lowd_8192_2048(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits1024_t ASL_slice_lowd_8192_1024(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits512_t ASL_slice_lowd_8192_512(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits256_t ASL_slice_lowd_8192_256(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits128_t ASL_slice_lowd_8192_128(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits64_t ASL_slice_lowd_8192_64(ASL_bits8192_t x, ASL_int_t lo, ASL_int_t width);
ASL_bits8192_t ASL_zero_extend_bits_64_8192(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_128_8192(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_256_8192(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_512_8192(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_1024_8192(int width, ASL_bits1024_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_2048_8192(int width, ASL_bits2048_t x, ASL_int_t n);
ASL_bits8192_t ASL_zero_extend_bits_4096_8192(int width, ASL_bits4096_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_64_8192(int width, ASL_bits64_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_128_8192(int width, ASL_bits128_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_256_8192(int width, ASL_bits256_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_512_8192(int width, ASL_bits512_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_1024_8192(int width, ASL_bits1024_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_2048_8192(int width, ASL_bits2048_t x, ASL_int_t n);
ASL_bits8192_t ASL_sign_extend_bits_4096_8192(int width, ASL_bits4096_t x, ASL_int_t n);
ASL_bits8192_t ASL_zeros_bits_8192(ASL_int_t width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS8192_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
