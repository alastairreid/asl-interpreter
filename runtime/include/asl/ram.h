////////////////////////////////////////////////////////////////
// Runtime memory support library for ASL's C backend
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_RAM_H
#define ASL_RAM_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ASL_ram {
        uint8_t  *p;
        uint64_t init_val;
} *ASL_ram_t;

ASL_ram_t ASL_ram_alloc();

void ASL_ram_free(struct ASL_ram **p);

void ASL_ram_init(int64_t address_size, ASL_ram_t ram, uint64_t val);

uint64_t ASL_ram_read(int64_t address_size, int64_t size, ASL_ram_t ram,
                      uint64_t address);

void ASL_ram_write(int64_t address_size, int64_t size, ASL_ram_t ram,
                   uint64_t address, uint64_t val);

#ifdef __cplusplus
}
#endif

#endif  // ASL_RAM_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
