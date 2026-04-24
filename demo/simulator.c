/***************************************************************
 * An example simulator harness for use when compiling an ISA
 * specification to C code.
 *
 * Copyright (C) 2023-2026 Intel Corporation
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
#include "sim_vars.h"


#define UNUSED __attribute__ ((unused))

// File that error messages are sent to
FILE* error_file = NULL;

/****************************************************************
 * ELF loader
 *
 * Currently limited to ELF64 files
 *
 * todo: replace all assert failures with proper error handling
 ****************************************************************/

// Load data into simulator memory and pad with zeros as needed
void load_block(void *thread_arg,
                struct threadlocal_state *thread,
                char* data,
                Elf64_Addr addr,
                Elf64_Xword file_size,
                Elf64_Xword mem_size) {
        for(uint64_t i = 0; i < file_size; ++i) {
                uint8_t value = *((uint8_t*)(data + i));
                // printf("Setting %lx = %x\n", addr + i, value);
                ASL_WriteMemory8(thread_arg, thread, addr + i, value);
        }
        for(uint64_t i = file_size; i < mem_size; ++i) {
                ASL_WriteMemory8(thread_arg, thread, addr + i, 0);
        }
}

void load_Phdr(void *thread_arg,
               struct threadlocal_state *thread,
               char* elf,
               Elf64_Phdr* ph) {
        if (ph->p_type == PT_LOAD) {
                char* data = elf + ph->p_offset;
                load_block(thread_arg, thread, data, ph->p_paddr, ph->p_filesz, ph->p_memsz);
        }
}

uint64_t load_elf64(void *thread_arg,
                    struct threadlocal_state *thread,
                    const char* filename) {
        FILE *f = fopen(filename, "rb");
        if (!f) {
                perror("Error while reading ELF file: ");
                fprintf(error_file, "%s\n", filename);
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
                fprintf(error_file, "File %s is not an ELF64 lsb file\n", filename);
                exit(1);
        }

        Elf64_Off  ph_off     = hdr->e_phoff;
        Elf64_Half ph_num     = hdr->e_phnum;
        Elf64_Half ph_entsize = hdr->e_phentsize;
        for(int i = 0; i < ph_num; ++i) {
                Elf64_Phdr* ph = (Elf64_Phdr*)(((char*) elf) + ph_off + i * ph_entsize);
                load_Phdr(thread_arg, thread, elf, ph);
        }
        return hdr->e_entry;
}

/****************************************************************
 * ISA error handling
 ****************************************************************/

void
ASL_error(const char* loc, const char* msg)
{
        fprintf(error_file, "%s: ISA error %s\n\n", loc, msg);
        fprintf(error_file, "This error indicates an error in the specification and should\n");
        fprintf(error_file, "be reported as a bug.\n");

        exit(1);
}

void
ASL_assert(const char* loc, const char* expr, bool c)
{
        if (!c) {
                fprintf(error_file, "%s: ISA assertion failure %s\n\n", loc, expr);
                fprintf(error_file, "This error indicates an error in the specification and should\n");
                fprintf(error_file, "be reported as a bug.\n");

                exit(1);
        }
}

void
ASL_runtime_error(const char *msg)
{
        fprintf(error_file, "Runtime error: %s\n", msg);
        fprintf(error_file, "This error indicates an error in the specification and should\n");
        fprintf(error_file, "be reported as a bug.\n");

        exit(1);
}

/****************************************************************
 * Register access by name
 *
 * This builds on the ASL_ReadReg64/ASL_WriteReg64 ASL functions
 * to provide access to registers by their name.
 ****************************************************************/

typedef int regid_t; // number must match the number in demo.md

typedef struct {
        const char* name;
        regid_t     id;
} reg_entry;

#define REG_ENTRY(idnum, nm) { .id=idnum, .name=#nm }
#define LAST_REG_ENTRY       { .id=-1, .name=NULL }

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
        for(int i = 0; reg_table[i].id >= 0; ++i) {
                if (strcasecmp(reg_table[i].name, name) == 0) {
                        return reg_table[i].id;
                }
        }
        return -1;
}

UNUSED static uint64_t get_register(void *thread_arg,
                                    struct threadlocal_state *thread,
                                    const char* name)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring get of unknown register '%s'\n", name);
                return 0;
        }
        uint64_t r = ASL_ReadReg64(thread_arg, thread, index);
        return r;
}

UNUSED static void set_register(void *thread_arg,
                                struct threadlocal_state *thread,
                                const char* name,
                                uint64_t val)
{
        int index = lookup_regname(name);
        if (index < 0) {
                printf("Ignoring set of unknown register '%s'\n", name);
                return;
        }
        printf("Setting %s to %lx\n", name, val);
        ASL_WriteReg64(thread_arg, thread, index, val);
}

/****************************************************************
 * Simulator
 ****************************************************************/

// Storage for the thread-local state in a processor.
//
// There are several ways that the processor state could be stored.
// - It could all be stored as global C variables - simple and efficient.
// - Or, to better support multithreading, we could partition the processor
//   state into thread-local variables and global variables (such as the RAM)
//   and represent each set of variables as fields of a C struct.
// - And, if we are splitting variables into a number of C structs, for each
//   struct we have the option of having the ISA code access variables
//   via a global variable that points to the struct or via a function
//   argument that must be passed to the function when we call it.
//
// In this demo, we have chosen to split the state into local and global
// variables and to use a function argument to access the local state
// and to use a global pointer to access the global state.
// (Other options are possible and may be better choices in some circumstances.)
//
// For this reason, it will be important to set the global variable
// 'global_state_ptr' to point to Global.
struct threadlocal_state Processor0;
struct global_state Global;

int main(int argc, const char* argv[])
{
        error_file = stderr;
        if (argc < 2) {
                fprintf(error_file, "Usage: simulator --steps=<n> <.elf files>\n");
                exit(1);
        }

        // Initialize all the state structs
        ASL_initialize_threadlocal_state(&Processor0);
        Processor0.client_ptr = (void*)"P0";
        ASL_initialize_global_state(&Global);

        // Set the state pointers
        global_state_ptr = &Global; // note that this is global
        struct threadlocal_state *thread = &Processor0; // note that this is local

        // Initialize extra thread argument, passed through ISL from exported
        // to imported functions
        void *thread_arg = (void *)"T0";

        ASL_Reset(thread_arg, thread);

        long steps = 10; // default number of steps to run
        for(int i = 1; i < argc; ++i) {
                const char* suffix = strrchr(argv[i], '.');
                if (suffix && 0 == strcmp(suffix, ".elf")) {
                        printf("Loading ELF file %s.\n", argv[i]);
                        uint64_t entry = load_elf64(thread_arg, thread, argv[i]);
                        printf("Entry point = 0x%lx\n", entry);
                        set_register(thread_arg, thread, "PC", entry);
                } else if (strncmp(argv[i], "--steps=", 8) == 0) {
                        steps = strtol(argv[i]+8, NULL, 10);
                } else {
                        printf("Error: unrecognized argument '%s'\n", argv[i]);
                        exit(1);
                }
        }

        Print_State(thread_arg, thread);
        for(int i = 0; i < steps && !ASL_IsHalted(thread_arg, thread); ++i) {
                printf("%s: Stepping processor %s\n",
                       (char *)thread_arg, (char *)thread->client_ptr);
                ASL_Step(thread_arg, thread);
                Print_State(thread_arg, thread);
        }

        exit(0);
}

/****************************************************************
 * End of file
 ****************************************************************/
