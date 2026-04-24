################################################################
# Demo Makefile
#
# Copyright (C) 2022-2026 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
################################################################

.default: test

ISA_PATH = ..:.
III      = ../_build/install/default/bin/iii
IIC      = ../_build/install/default/bin/iic

################################################################
# Test whether we have GNU as or clang emulating as
# (We can only build an elf file and run the test if we have
# GNU as.)
################################################################

HAVE_GNU_AS := `as --version | grep -q 'GNU assembler'`

REPORT_NOT_GAS = "Unable to build ELF file to use in demo: GNU 'as' required - skipping demo"

################################################################
# Build a test program using the demo ISA
################################################################

clean::
	$(RM) test.o test.elf

# run preprocessor on test.S using default rule
test.s: test.S assembly.s

test.o: test.s
	@ if ${HAVE_GNU_AS}; then \
	as test.s -o test.o; \
	else echo ${REPORT_NOT_GAS}; fi

test.elf: test.o
	@ if ${HAVE_GNU_AS}; then \
	ld test.o -o test.elf; \
	nm test.elf; \
	else echo ${REPORT_NOT_GAS}; fi

################################################################
# Run demo on interpreter
################################################################

test: test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	env ISA_PATH="${ISA_PATH}" ${III} --nobanner --batchmode demo.isa --project test.prj | filecheck test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

demo: test.elf
	@ if ${HAVE_GNU_AS}; then \
	env ISA_PATH="${ISA_PATH}" ${III} --batchmode demo.isa --project test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

################################################################
# Build simulator using C backend
################################################################

CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -Wno-unused-but-set-variable
CFLAGS += -Wno-parentheses-equality
CFLAGS += -std=c2x

ifeq ($(origin CC),default)
  CC = clang-16
endif

.PRECIOUS: simulator_%

simulator_% : simulator.c config.json demo.isa
	@ if ${HAVE_GNU_AS}; then \
	$(IIC) --basename=sim --intermediates=log --split-state --new-ffi --backend=$* > sim.prj ; \
	env ISA_PATH="${ISA_PATH}" $(III) --nobanner --batchmode --project=sim.prj --configuration=config.json demo.isa ; \
	$(CC) ${shell ${IIC} --backend=$* --print-c-flags} ${CFLAGS} simulator.c sim_exceptions.c sim_vars.c sim_funs.c -o $@ ${shell ${IIC} --backend=$* --print-ld-flags} ${LDFLAGS} ; \
	else echo ${REPORT_NOT_GAS}; fi

test_% : simulator_% test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	./simulator_$* test.elf --steps=20 | filecheck test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

demo_% : simulator_% test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	./simulator_$* test.elf --steps=20; \
	else echo ${REPORT_NOT_GAS}; fi

clean ::
	$(RM) sim.prj
	$(RM) log.*.isa
	$(RM) sim_types.h sim_exceptions.h sim_vars.h sim_ffi.h
	$(RM) sim_exceptions.c sim_vars.c sim_funs.c
	$(RM) sim_types.hpp sim_exceptions.hpp sim_vars.hpp
	$(RM) sim_exceptions.cpp sim_vars.cpp sim_funs.cpp
	$(RM) simulator_*

################################################################
# End
################################################################
