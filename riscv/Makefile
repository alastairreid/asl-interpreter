################################################################
# Demo Makefile
#
# Copyright (C) 2022-2024 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.default: simulator

ASL_PATH = ..:.
ASLI     = ../_build/default/bin/asli.exe
ASL2C    = ../_build/default/bin/asl2c.py

BACKEND = c23

################################################################
# Run demo on interpreter
################################################################

test_interpreter: test.elf test.prj
	env ASL_PATH="${ASL_PATH}" ${ASLI} --nobanner --batchmode riscv.asl --project test.prj | filecheck test.prj

trace_interpreter: test.elf
	env ASL_PATH="${ASL_PATH}" ${ASLI} --batchmode riscv.asl --project test.prj

################################################################
# Build simulator using C backend
################################################################

CFLAGS = $(shell ${ASL2C} --print-c-flags)
LDFLAGS = $(shell ${ASL2C} --print-ld-flags)

CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -Wno-parentheses-equality
CFLAGS += -Wno-unused-variable
CFLAGS += -Wno-unused-function

CC=clang-16 -std=c2x

.PRECIOUS: simulator_%

simulator : simulator.c exports.json riscv.asl
	$(ASL2C) --basename=sim --intermediates=log --backend=${BACKEND} > sim.prj
	env ASL_PATH="${ASL_PATH}" $(ASLI) --nobanner --batchmode --project=sim.prj --configuration=exports.json riscv.asl
	$(CC) ${CFLAGS} simulator.c -o $@ ${LDFLAGS}

test : simulator test.elf test.prj
	./simulator test.elf --steps=20 | filecheck test.prj

trace : simulator test.elf test.prj
	./simulator test.elf --steps=20

clean ::
	$(RM) sim.prj
	$(RM) log.*.asl
	$(RM) sim_types.h sim_exceptions.h sim_vars.h
	$(RM) sim_exceptions.c sim_vars.c sim_funs.c
	$(RM) sim_types.hpp sim_exceptions.hpp sim_vars.hpp
	$(RM) sim_exceptions.cpp sim_vars.cpp sim_funs.cpp
	$(RM) simulator

################################################################
# End
################################################################
