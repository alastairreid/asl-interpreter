################################################################
# Demo Makefile
#
# Copyright (C) 2022-2024 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.default: test

ASL_PATH = ..:.
ASLI     = ../_build/default/bin/asli.exe
ASL2C    = ../_build/default/bin/asl2c.py

################################################################
# Build a test program using the demo ISA
################################################################

clean::
	$(RM) test.s test.o test.elf

test.s : test.S assembly.s

test.o: test.s
	as test.s -o test.o

test.elf: test.o
	ld test.o -o test.elf
	nm test.elf

################################################################
# Run demo on interpreter
################################################################

test: test.elf test.prj
	env ASL_PATH="${ASL_PATH}" ${ASLI} --nobanner --batchmode demo.asl --project test.prj | filecheck test.prj

demo: test.elf
	env ASL_PATH="${ASL_PATH}" ${ASLI} --batchmode demo.asl --project test.prj

################################################################
# Build simulator using C backend
################################################################

CFLAGS = $(shell ${ASL2C} --print-c-flags)
LDFLAGS = $(shell ${ASL2C} --print-ld-flags)

CFLAGS += -Wall
CFLAGS += -g

CC=clang-16 -std=c2x

.PRECIOUS: simulator_%

simulator_% : simulator.c exports.json demo.asl
	$(ASL2C) --basename=sim --intermediates=log --backend=$* > sim.prj
	env ASL_PATH="${ASL_PATH}" $(ASLI) --nobanner --batchmode --project=sim.prj --configuration=exports.json demo.asl
	$(CC) ${CFLAGS} simulator.c -o $@ ${LDFLAGS}

test_% : simulator_% test.elf test.prj
	./simulator_$* test.elf --steps=20 | filecheck test.prj

demo_% : simulator_% test.elf test.prj
	./simulator_$* test.elf --steps=20

clean ::
	$(RM) sim.prj
	$(RM) log.*.asl
	$(RM) sim_types.h sim_exceptions.h sim_vars.h
	$(RM) sim_exceptions.c sim_vars.c sim_funs.c
	$(RM) sim_types.hpp sim_exceptions.hpp sim_vars.hpp
	$(RM) sim_exceptions.cpp sim_vars.cpp sim_funs.cpp
	$(RM) simulator_*

################################################################
# End
################################################################
