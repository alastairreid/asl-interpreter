// FFI testing support functions used by ffi_export_01.asl
//
// Exports a function FFI_test_exports that, for many
// different ASL types "T":
//
// - calls an ASL function "FFI_{T}(x : {T}) -> {T}"
// - and prints the result
//
// Copyright (C) 2025-2026 Intel Corporation

#include "isa_ffi.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

void FFI_test_exports() {
        printf("8'x%x\n", FFI_bits8(8));
        printf("16'x%x\n", FFI_bits16(16));
        printf("32'x%x\n", FFI_bits32(32));
        printf("64'x%lx\n", FFI_bits64(64));

        printf("2'x%x\n", FFI_bits2(2));
        printf("17'x%x\n", FFI_bits17(17));

        uint64_t inbuf[2];
        uint64_t outbuf[2];

        outbuf[0] = 65;
        outbuf[1] = 0;
        FFI_bits65(outbuf, inbuf);
        printf("65'x%" PRIx64 "_%016" PRIx64 "\n", inbuf[1], inbuf[0]);

        outbuf[0] = 127;
        outbuf[1] = 0;
        FFI_bits65(outbuf, inbuf);
        printf("127'x%" PRIx64 "_%016" PRIx64 "\n", inbuf[1], inbuf[0]);

        outbuf[0] = 128;
        outbuf[1] = 0;
        FFI_bits65(outbuf, inbuf);
        printf("128'x%" PRIx64 "_%016" PRIx64 "\n", inbuf[1], inbuf[0]);

        printf("%s\n", FFI_string("abcd"));

        enum E eret = FFI_E(C);
        printf("%s\n", (eret == C) ? "True" : "False");

        bool bret = FFI_Boolean(true);
        printf("%s\n", bret ? "True" : "False");

        printf("%ld\n", FFI_integer(42));
        printf("i17'd%d\n", FFI_sint17(42));

        int64_t intret2;
        bool boolret2;
        FFI_int_bool(1, &intret2, &boolret2);
        printf("(%ld, %s)\n", intret2, boolret2 ? "True" : "False");
        FFI_int_bool(4, &intret2, &boolret2);
        printf("(%ld, %s)\n", intret2, boolret2 ? "True" : "False");

}
