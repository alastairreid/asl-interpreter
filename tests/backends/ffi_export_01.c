// FFI testing support functions used by ffi_export_01.asl
//
// Exports a function FFI_test_exports that, for many
// different ASL types "T":
//
// - calls an ASL function "FFI_{T}(x : {T}) => {T}"
// - and prints the result
//
// Copyright (C) 2025-2025 Intel Corporation

#include "asl_ffi.h"
#include <stdio.h>
#include <string.h>

void FFI_test_exports() {
        printf("8'x%x\n", FFI_bits8(8));
        printf("16'x%x\n", FFI_bits16(16));
        printf("32'x%x\n", FFI_bits32(32));
        printf("64'x%lx\n", FFI_bits64(64));

        uint64_t inbuf1[1];
        uint64_t inbuf2[2];

        uint64_t outbuf1[1];
        uint64_t outbuf2[2];

        outbuf1[0] = 17;
        FFI_bits17(outbuf1, inbuf1);
        printf("17'x%lx\n", inbuf1[0]);

        outbuf2[0] = 65;
        outbuf2[1] = 0;
        FFI_bits65(outbuf2, inbuf2);
        printf("65'x%lx%08lx\n", inbuf2[1], inbuf2[0]);

        outbuf2[0] = 127;
        outbuf2[1] = 0;
        FFI_bits65(outbuf2, inbuf2);
        printf("127'x%lx%08lx\n", inbuf2[1], inbuf2[0]);

        outbuf2[0] = 128;
        outbuf2[1] = 0;
        FFI_bits65(outbuf2, inbuf2);
        printf("128'x%lx%08lx\n", inbuf2[1], inbuf2[0]);

        printf("%s\n", FFI_string("abcd"));

        enum E eret = FFI_E(C);
        printf("%s\n", (eret == C) ? "TRUE" : "FALSE");

        bool bret = FFI_boolean(true);
        printf("%s\n", bret ? "TRUE" : "FALSE");

        printf("%d\n", FFI_integer(42));
        printf("i17'd%d\n", FFI_sint17(42));

        int intret2;
        bool boolret2;
        FFI_int_bool(1, &intret2, &boolret2);
        printf("(%d, %s)\n", intret2, boolret2 ? "TRUE" : "FALSE");
        FFI_int_bool(4, &intret2, &boolret2);
        printf("(%d, %s)\n", intret2, boolret2 ? "TRUE" : "FALSE");

}
