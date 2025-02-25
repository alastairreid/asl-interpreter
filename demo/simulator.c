/***************************************************************
 * An example simulator harness for use when compiling an ISA
 * specification to C code.
 *
 * Copyright (C) 2023-2025 Intel Corporation
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

#define UNUSED __attribute__ ((unused))

// File that error messages are sent to
FILE* ASL_error_file = NULL;

/****************************************************************
 * ELF loader
 *
 * Currently limited to ELF64 files
 *
 * todo: replace all assert failures with proper error handling
 ****************************************************************/

// Load data into simulator memory and pad with zeros as needed
void load_block(char* data, Elf64_Addr addr, Elf64_Xword file_size, Elf64_Xword mem_size) {
        for(uint64_t i = 0; i < file_size; ++i) {
                uint8_t value = *((uint8_t*)(data + i));
                // printf("Setting %lx = %x\n", addr + i, value);
                ASL_WriteMemory8(addr + i, value);
        }
        for(uint64_t i = file_size; i < mem_size; ++i) {
                ASL_WriteMemory8(addr + i, 0);
        }
}

void load_Phdr(char* elf, Elf64_Phdr* ph) {
        if (ph->p_type == PT_LOAD) {
                char* data = elf + ph->p_offset;
                load_block(data, ph->p_paddr, ph->p_filesz, ph->p_memsz);
        }
}

uint64_t load_elf64(const char* filename) {
        FILE *f = fopen(filename, "rb");
        if (!f) {
                perror("Error while reading ELF file: ");
                fprintf(ASL_error_file, "%s\n", filename);
                exit(1);
        }
        fseek(f, 0L, SEEK_END);
        long size = ftell(f);
        fseek(f, 0L, SEEK_SET);

        char *elf = malloc(size);
        assert(elf);
        fread(elf, size, 1, f);
        fclose(f);

        Elf64_Ehdr *hdr = (Elf64_Ehdr*)elf;

        // Check ELF magic number
        if (  hdr->e_ident[EI_MAG0]  != 0x7f
           || hdr->e_ident[EI_MAG1]  != 'E'
           || hdr->e_ident[EI_MAG2]  != 'L'
           || hdr->e_ident[EI_MAG3]  != 'F'
           || hdr->e_ident[EI_CLASS] != ELFCLASS64
           || hdr->e_ident[EI_DATA]  != ELFDATA2LSB
           ) {
                fprintf(ASL_error_file, "File %s is not an ELF64 lsb file\n", filename);
                exit(1);
        }

        Elf64_Off  ph_off     = hdr->e_phoff;
        Elf64_Half ph_num     = hdr->e_phnum;
        Elf64_Half ph_entsize = hdr->e_phentsize;
        for(int i = 0; i < ph_num; ++i) {
                Elf64_Phdr* ph = (Elf64_Phdr*)(((char*) elf) + ph_off + i * ph_entsize);
                load_Phdr(elf, ph);
        }
        return hdr->e_entry;
}

/****************************************************************
 * ASL error handling
 ****************************************************************/

void
ASL_error(const char* loc, const char* msg)
{
        fprintf(ASL_error_file, "%s: ASL error %s\n\n", loc, msg);
        fprintf(ASL_error_file, "This error indicates an error in the specification and should\n");
        fprintf(ASL_error_file, "be reported as a bug.\n");

        exit(1);
}

void
ASL_assert(const char* loc, const char* expr, bool c)
{
        if (!c) {
                fprintf(ASL_error_file, "%s: ASL assertion failure %s\n\n", loc, expr);
                fprintf(ASL_error_file, "This error indicates an error in the specification and should\n");
                fprintf(ASL_error_file, "be reported as a bug.\n");

                exit(1);
        }
}

void
ASL_runtime_error(const char *msg)
{
        fprintf(ASL_error_file, "Runtime error: %s\n", msg);
        fprintf(ASL_error_file, "This error indicates an error in the specification and should\n");
        fprintf(ASL_error_file, "be reported as a bug.\n");

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
 * Register access by name
 *
 * This builds on the ASL_ReadReg64/ASL_WriteReg64 ASL functions
 * to provide access to registers by their name.
 ****************************************************************/

typedef int ASL_regid; // number must match the number in demo.md

typedef struct {
        const char* name;
        ASL_regid asl_id;
} reg_entry;

#define REG_ENTRY(id, nm) { .asl_id=id, .name=#nm }
#define LAST_REG_ENTRY    { .asl_id=-1, .name=NULL }

// This table maps register names (strings) to unique register
// identifiers (ints).
//
// The identifier "-1" is reserved but, otherwise, the number
// allocation is arbitrary but any change to this table
// requires a matching change to the ASL functions
// ASL_ReadReg64 / ASL_WriteReg64.
static reg_entry reg_table[] = {
    REG_ENTRY(  0, PC),
    REG_ENTRY(  1, halted),
    REG_ENTRY( 10, R0),
    REG_ENTRY( 11, R1),
    REG_ENTRY( 12, R2),
    REG_ENTRY( 13, R3),

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

UNUSED static uint64_t get_register(const char* name)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring get of unknown register '%s'\n", name);
                return 0;
        }
        uint64_t r = ASL_ReadReg64(index);
        return r;
}

UNUSED static void set_register(const char* name, uint64_t val)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring set of unknown register '%s'\n", name);
                return;
        }
        printf("Setting %s to %lx\n", name, val);
        ASL_WriteReg64(index, val);
}

/****************************************************************
 * Simulator
 ****************************************************************/

// Storage for the thread-local state in a processor.
// (Note that the variables 'threadlocal_state_ptr' and 'global_state_ptr'
// need to point to these structs and their initializers need to be
// called.)
struct threadlocal_state Processor0;
struct global_state Global;

int main(int argc, const char* argv[])
{
        ASL_error_file = stderr;
        if (argc < 2) {
                fprintf(ASL_error_file, "Usage: simulator --steps=<n> <.elf files>\n");
                exit(1);
        }

        // Initialize all the state structs
        ASL_initialize_threadlocal_state(&Processor0);
        ASL_initialize_global_state(&Global);

        // Set the state pointers
        threadlocal_state_ptr = &Processor0;
        global_state_ptr = &Global;

        ASL_Reset();

        long steps = 10; // default number of steps to run
        for(int i = 1; i < argc; ++i) {
                const char* suffix = strrchr(argv[i], '.');
                if (suffix && 0 == strcmp(suffix, ".elf")) {
                        printf("Loading ELF file %s.\n", argv[i]);
                        uint64_t entry = load_elf64(argv[i]);
                        printf("Entry point = 0x%lx\n", entry);
                        set_register("PC", entry);
                } else if (strncmp(argv[i], "--steps=", 8) == 0) {
                        steps = strtol(argv[i]+8, NULL, 10);
                } else {
                        printf("Error: unrecognized argument '%s'\n", argv[i]);
                        exit(1);
                }
        }

        PrintState();
        for(int i = 0; i < steps && !ASL_IsHalted(); ++i) {
                ASL_Step();
                PrintState();
        }

        exit(0);
}

/****************************************************************
 * End of file
 ****************************************************************/
