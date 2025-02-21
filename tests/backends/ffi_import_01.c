// FFI testing support functions to be imported into ASL test programs
// These functions are "null" functions (or "identity" functions):
// they all return their argument.
//
// Copyright (C) 2025-2025 Intel Corporation

#include "asl_ffi.h"
#include <string.h>

uint8_t FFI_null_bits8(uint8_t x) { return x; }
uint16_t FFI_null_bits16(uint16_t x) { return x; }
uint32_t FFI_null_bits32(uint32_t x) { return x; }
uint64_t FFI_null_bits64(uint64_t x) { return x; }

void FFI_null_bits17(uint64_t x[1], uint64_t r[1]) { memcpy(r, x, 1*sizeof(uint64_t)); }
void FFI_null_bits65(uint64_t x[2], uint64_t r[2]) { memcpy(r, x, 2*sizeof(uint64_t)); }
void FFI_null_bits127(uint64_t x[2], uint64_t r[2]) { memcpy(r, x, 2*sizeof(uint64_t)); }
void FFI_null_bits128(uint64_t x[2], uint64_t r[2]) { memcpy(r, x, 2*sizeof(uint64_t)); }

const char* FFI_null_string(const char *x) { return x; }
enum E FFI_null_E(enum E x) { return x; }
bool FFI_null_boolean(bool x) { return x; }
int FFI_null_integer(int x) { return x; }
int FFI_null_sint17(int x) { return x; }

void FFI_int_bool(int x, int* ret1, bool* ret2)
{
    *ret1 = x;
    *ret2 = x > 3;
}
