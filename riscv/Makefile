################################################################
# RISC-V Makefile
#
# Copyright (C) 2022-2026 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.default: simulator

ISA_PATH = ..:.
III     = ../_build/install/default/bin/iii
IIC    = ../_build/install/default/bin/iic

BACKEND = c23

################################################################
# Run demo on interpreter
################################################################

test_interpreter: test.elf test.prj
	env ISA_PATH="${ISA_PATH}" ${III} --batchmode riscv.isa --project test.prj | filecheck test.prj

trace_interpreter: test.elf
	env ISA_PATH="${ISA_PATH}" ${III} --batchmode riscv.isa --project test.prj

################################################################
# Build simulator using C backend
################################################################

CFLAGS = $(shell ${IIC} --print-c-flags)
LDFLAGS = $(shell ${IIC} --print-ld-flags)

CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -Wno-parentheses-equality
CFLAGS += -Wno-unused-variable
CFLAGS += -Wno-unused-function

CC=clang-18 -std=c2x

.PRECIOUS: simulator_%

simulator : simulator.c riscv.isa
	$(IIC) --basename=sim --intermediates=log --backend=${BACKEND} --new-ffi > sim.prj
	env ISA_PATH="${ISA_PATH}" $(III) --batchmode --project=sim.prj riscv.isa
	$(CC) ${CFLAGS} simulator.c -o $@ ${LDFLAGS}

test : simulator test.elf test.prj
	./simulator test.elf --steps=20 | filecheck test.prj

trace : simulator test.elf test.prj
	./simulator test.elf --steps=20

clean ::
	$(RM) sim.prj
	$(RM) log.*.isa
	$(RM) sim_types.h sim_exceptions.h sim_vars.h sim_ffi.h
	$(RM) sim_exceptions.c sim_vars.c sim_funs.c
	$(RM) sim_types.hpp sim_exceptions.hpp sim_vars.hpp
	$(RM) sim_exceptions.cpp sim_vars.cpp sim_funs.cpp
	$(RM) simulator

################################################################
# End
################################################################
