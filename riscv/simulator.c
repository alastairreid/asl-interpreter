/****************************************************************
 * An example simulator harness for use when compiling an ISA
 * specification to C code.
 *
 * Copyright (C) 2023-2024 Intel Corporation
 ****************************************************************/

#include <assert.h>
#include <elf.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "asl/runtime.h"

#include "sim_ffi.h"

// File that error messages are sent to
FILE* SIM_error_file = NULL;

/****************************************************************
 * ELF loader
 *
 * Currently limited to ELF32 files
 *
 * todo: replace all assert failures with proper error handling
 ****************************************************************/

// Load data into simulator memory and pad with zeros as needed
void load_block(char* data, Elf32_Addr addr, Elf32_Xword file_size, Elf32_Xword mem_size) {
        for(uint64_t i = 0; i < file_size; ++i) {
                uint8_t value = *((uint8_t*)(data + i));
                // printf("Setting %lx = %x\n", addr + i, value);
                ISA_Write_Memory8(addr + i, value);
        }
        for(uint64_t i = file_size; i < mem_size; ++i) {
                ISA_Write_Memory8(addr + i, 0);
        }
}

void load_Phdr(char* elf, Elf32_Phdr* ph) {
        if (ph->p_type == PT_LOAD) {
                char* data = elf + ph->p_offset;
                load_block(data, ph->p_paddr, ph->p_filesz, ph->p_memsz);
        }
}

uint64_t load_elf32(const char* filename) {
        FILE *f = fopen(filename, "rb");
        if (!f) {
                perror("Error while reading ELF file: ");
                fprintf(SIM_error_file, "%s\n", filename);
                exit(1);
        }
        fseek(f, 0L, SEEK_END);
        long size = ftell(f);
        fseek(f, 0L, SEEK_SET);

        char *elf = malloc(size);
        assert(elf);
        fread(elf, size, 1, f);
        fclose(f);

        Elf32_Ehdr *hdr = (Elf32_Ehdr*)elf;

        // Check ELF magic number
        if (  hdr->e_ident[EI_MAG0]  != 0x7f
           || hdr->e_ident[EI_MAG1]  != 'E'
           || hdr->e_ident[EI_MAG2]  != 'L'
           || hdr->e_ident[EI_MAG3]  != 'F'
           || hdr->e_ident[EI_CLASS] != ELFCLASS32
           || hdr->e_ident[EI_DATA]  != ELFDATA2LSB
           ) {
                fprintf(SIM_error_file, "File %s is not an ELF64 lsb file\n", filename);
                exit(1);
        }

        Elf32_Off  ph_off     = hdr->e_phoff;
        Elf32_Half ph_num     = hdr->e_phnum;
        Elf32_Half ph_entsize = hdr->e_phentsize;
        for(int i = 0; i < ph_num; ++i) {
                Elf32_Phdr* ph = (Elf32_Phdr*)(((char*) elf) + ph_off + i * ph_entsize);
                load_Phdr(elf, ph);
        }
        return hdr->e_entry;
}

/****************************************************************
 * ISA error handling
 *
 * These three definitions override the default definitions in the
 * isa-tools runtime library.
 * Note that it is necessary to either define none of these functions
 * or all three of these functions. If you define just some of
 * the functions, you will get a linking error.
 ****************************************************************/

void
ASL_error(const char* loc, const char* msg)
{
        fprintf(SIM_error_file, "%s: ISA error %s\n\n", loc, msg);
        fprintf(SIM_error_file, "This error indicates an error in the specification and should\n");
        fprintf(SIM_error_file, "be reported to the specification authors as a bug.\n");

        exit(1);
}

void
ASL_assert(const char* loc, const char* expr, bool c)
{
        if (!c) {
                fprintf(SIM_error_file, "%s: ISA assertion failure %s\n\n", loc, expr);
                fprintf(SIM_error_file, "This error indicates an error in the specification and should\n");
                fprintf(SIM_error_file, "be reported to the specification authors as a bug.\n");

                exit(1);
        }
}

void
ASL_runtime_error(const char *msg)
{
        fprintf(SIM_error_file, "Runtime error: %s\n", msg);
        fprintf(SIM_error_file, "This error indicates an error in the isa-tools runtime and should\n");
        fprintf(SIM_error_file, "be reported to the isa-tools authors as a bug.\n");

        exit(1);
}

/****************************************************************
 * Include the specification
 ****************************************************************/

#include "sim_exceptions.h"
#include "sim_vars.h"

// These could be compiled separately and linked in - but this is easier
#include "sim_exceptions.c"
#include "sim_vars.c"
#include "sim_funs.c"

/****************************************************************
 * Exception support code
 ****************************************************************/

void exception_clear()
{
        ASL_exception = (ASL_exception_t){ ._exc={.ASL_tag = ASL_no_exception} };
}

enum ASL_exception_tag exception_tag()
{
        return ASL_exception._exc.ASL_tag;
}

void exception_check(const char *what)
{
        if (ASL_exception._exc.ASL_tag != ASL_no_exception) {
                fprintf(SIM_error_file, "Error: uncaught exception in %s\n", what);
                exit(1);
        }
}

/****************************************************************
 * Register access by name
 *
 * This builds on the ISA_Read_Register64/ISA_Write_Register64 .isa functions
 * to provide access to registers by their name.
 ****************************************************************/

typedef int SIM_regid; // number must match the number in demo.md

typedef struct {
        const char* name;
        SIM_regid asl_id;
} reg_entry;

#define REG_ENTRY(id, nm) { .asl_id=id, .name=#nm }
#define LAST_REG_ENTRY    { .asl_id=-1, .name=NULL }

// This table maps register names (strings) to unique register
// identifiers (ints).
//
// The identifier "-1" is reserved but, otherwise, the number
// allocation is arbitrary but any change to this table
// requires a matching change to the .isa functions
// ISA_ReadReg64 / ISA_WriteReg64.
static reg_entry reg_table[] = {
    REG_ENTRY(  0, PC),
    REG_ENTRY(  1, halted),
    REG_ENTRY(256 +  0, R0),
    REG_ENTRY(256 +  1, R1),
    REG_ENTRY(256 +  2, R2),
    REG_ENTRY(256 +  3, R3),
    REG_ENTRY(256 +  4, R4),
    REG_ENTRY(256 +  5, R5),
    REG_ENTRY(256 +  6, R6),
    REG_ENTRY(256 +  7, R7),
    REG_ENTRY(256 +  8, R8),
    REG_ENTRY(256 +  9, R9),
    REG_ENTRY(256 + 10, R10),
    REG_ENTRY(256 + 11, R11),
    REG_ENTRY(256 + 12, R12),
    REG_ENTRY(256 + 13, R13),
    REG_ENTRY(256 + 14, R14),
    REG_ENTRY(256 + 15, R15),
    REG_ENTRY(256 + 16, R16),
    REG_ENTRY(256 + 17, R17),
    REG_ENTRY(256 + 18, R18),
    REG_ENTRY(256 + 19, R19),
    REG_ENTRY(256 + 20, R20),
    REG_ENTRY(256 + 21, R21),
    REG_ENTRY(256 + 22, R22),
    REG_ENTRY(256 + 23, R23),
    REG_ENTRY(256 + 24, R24),
    REG_ENTRY(256 + 25, R25),
    REG_ENTRY(256 + 26, R26),
    REG_ENTRY(256 + 27, R27),
    REG_ENTRY(256 + 28, R28),
    REG_ENTRY(256 + 29, R29),
    REG_ENTRY(256 + 30, R30),
    REG_ENTRY(256 + 31, R31),

    LAST_REG_ENTRY
};

static int lookup_regname(const char* name)
{
        for(int i = 0; reg_table[i].asl_id >= 0; ++i) {
                if (strcasecmp(reg_table[i].name, name) == 0) {
                        return reg_table[i].asl_id;
                }
        }
        return -1;
}

static uint64_t get_register(const char* name)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring get of unknown register '%s'\n", name);
                return 0;
        }
        uint64_t r = ISA_Read_Register64(index);
        exception_check("ISA::Read_Register64");
        return r;
}

static void set_register(const char* name, uint64_t val)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring set of unknown register '%s'\n", name);
                return;
        }
        printf("Setting %s to %lx\n", name, val);
        ISA_Write_Register64(index, val);
        exception_check("ISA::Write_Register64");
}

/****************************************************************
 * Simulator
 ****************************************************************/

int main(int argc, const char* argv[])
{
        SIM_error_file = stderr;
        if (argc < 2) {
                fprintf(SIM_error_file, "Usage: simulator --steps=<n> <.elf files>\n");
                exit(1);
        }
        exception_clear();
        ISA_Reset();
        exception_check("ISA::Reset");

        long steps = 10; // default number of steps to run
        for(int i = 1; i < argc; ++i) {
                const char* suffix = strrchr(argv[i], '.');
                if (suffix && 0 == strcmp(suffix, ".elf")) {
                        printf("Loading ELF file %s.\n", argv[i]);
                        uint64_t entry = load_elf32(argv[i]);
                        printf("Entry point = 0x%lx\n", entry);
                        set_register("PC", entry);
                } else if (strncmp(argv[i], "--steps=", 8) == 0) {
                        steps = strtol(argv[i]+8, NULL, 10);
                } else {
                        printf("Error: unrecognized argument '%s'\n", argv[i]);
                        exit(1);
                }
        }

        ISA_Print_State();
        for(int i = 0; i < steps && !ISA_Is_Halted(); ++i) {
                ISA_Step();
                exception_check("ISA::Step");
                ISA_Print_State();
        }

        exit(0);
}

/****************************************************************
 * End of file
 ****************************************************************/
