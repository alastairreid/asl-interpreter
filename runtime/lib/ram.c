////////////////////////////////////////////////////////////////
// Runtime memory support library for ASL's C backend
//
// Copyright (C) 2022-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/ram.h"

#include <string.h>
#include <stdlib.h>

#include "asl/error.h"

enum {
        PAGE_ENTRY_SZ = 1,
        TABLE_ENTRY_SZ = sizeof(uint8_t *),

        ENTRIES_LOG2 = 16,  // both tables and pages have 64k entries
        ENTRIES = 1 << ENTRIES_LOG2,

        PAGE_SZ = PAGE_ENTRY_SZ * ENTRIES,
        LEVEL_OF_PAGE = 4,
};

static inline uint64_t
entry_num(uint64_t address)
{
        return address & (ENTRIES - 1);
}

static inline uint64_t
page_offset(uint64_t address)
{
        return PAGE_ENTRY_SZ * entry_num(address);
}

static inline uint64_t
table_offset(uint64_t address)
{
        return TABLE_ENTRY_SZ * entry_num(address);
}

static uint8_t *
page_alloc(uint64_t init_val)
{
        void *p = malloc(PAGE_SZ);
        ASL_runtime_error_if(!p, "cannot allocate a page");
        for (int i = 0; i < PAGE_SZ / 8; ++i) {
            ((uint64_t*)p)[i] = init_val;
        }
        return p;
}

static uint8_t *
table_alloc()
{
        void *p = calloc(ENTRIES, TABLE_ENTRY_SZ);
        ASL_runtime_error_if(!p, "cannot allocate a table");
        return p;
}

static void
table_free(uint8_t *p, int level)
{
        if (!p)
                return;

        if (level < LEVEL_OF_PAGE) {
                for (int i = 0; i < ENTRIES; ++i) {
                        uint8_t *e;
                        memcpy(&e, p + TABLE_ENTRY_SZ * i, sizeof(uint8_t *));
                        table_free(e, level + 1);
                }
        }

        free(p);
}

/* When looking for a page, page table structures (tables and pages) are
   allocated on demand. */
static uint8_t *
page_lookup(ASL_ram_t ram, uint64_t address)
{
        uint8_t *p = ram->p;
        uint8_t **p_addr = &ram->p;

        for (int level = 1; level < LEVEL_OF_PAGE; ++level) {
                if (!p) {  // no table
                        p = table_alloc();
                        *p_addr = p;
                }

                address >>= ENTRIES_LOG2;
                p_addr = (uint8_t **)(p + table_offset(address));
                p = *p_addr;
        }

        if (!p) {  // no page
                p = page_alloc(ram->init_val);
                *p_addr = p;
        }

        return p;
}

ASL_ram_t
ASL_ram_alloc()
{
        return calloc(1, sizeof(struct ASL_ram));
}

void
ASL_ram_free(struct ASL_ram **p)
{
        ASL_ram_t ram = *p;
        ASL_runtime_error_if(!ram, "cannot deallocate RAM");

        table_free(ram->p, 1);
        free(ram);
}

void
ASL_ram_init(int64_t address_size, ASL_ram_t ram, uint64_t val)
{
        ASL_runtime_error_if(!ram, "cannot initialize RAM");

        table_free(ram->p, 1);
        ram->p = 0;
        ram->init_val = val;
}

uint64_t
ASL_ram_read(int64_t address_size, int64_t size, ASL_ram_t ram,
             uint64_t address)
{
        ASL_runtime_error_if(!ram, "cannot read RAM");
        ASL_runtime_error_if(size > 8, "unsupported read access size");

        uint64_t val = 0;
        for (uint64_t i = 0; i < size; ++i) {
                uint8_t *page = page_lookup(ram, address + i);
                *((uint8_t *)&val + i) = *(page + page_offset(address + i));
        }
        return val;
}

void
ASL_ram_write(int64_t address_size, int64_t size, ASL_ram_t ram,
              uint64_t address, uint64_t val)
{
        ASL_runtime_error_if(!ram, "cannot write RAM");
        ASL_runtime_error_if(size > 8, "unsupported write access size");

        for (uint64_t i = 0; i < size; ++i) {
                uint8_t *page = page_lookup(ram, address + i);
                *(page + page_offset(address + i)) = *((uint8_t *)&val + i);
        }
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
