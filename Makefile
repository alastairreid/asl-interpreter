################################################################
# ASL Makefile
#
# Copyright Arm Limited (c) 2017-2019
# Copyright (C) 2022-2025 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.DEFAULT: all

VERSION = 0.2.0

export DUNE_BUILD_DIR ?= $(CURDIR)/_build

DUNE := dune
OPAM := opam
LIT := lit

# Control verbosity of testing
LIT_VERBOSITY = --succinct
# LIT_VERBOSITY = --verbose
DUNE_VERBOSITY = ALCOTEST_COMPACT=1
# DUNE_VERBOSITY =

build::
	$(DUNE) build
	$(MAKE) -C runtime BUILD_DIR=$(DUNE_BUILD_DIR)/runtime

install::
	$(DUNE) build @install
	${RM} -r ${OPAM_SWITCH_PREFIX}/lib/asli/*
	$(DUNE) install

uninstall::
	$(DUNE) build @install
	$(DUNE) uninstall
	${RM} -r ${OPAM_SWITCH_PREFIX}/lib/asli/*

publish::
	$(DUNE) build @install
	$(OPAM) publish https://github.com/alastairreid/asl-interpreter/archive/$(VERSION).tar.gz

doc::
	$(DUNE) build @doc
	@echo Documentation is in _build/default/_doc/_html/index.html

clean::
	$(RM) *~
	$(DUNE) clean
	$(MAKE) -C demo clean

test: dune_test
test: runtime_test
test: lit_test
test: test_backends
test: test_demo_interpreter
test: test_demos

dune_test: build
	$(DUNE_VERBOSITY) $(DUNE) test

runtime_test:
	$(MAKE) -C runtime test BUILD_DIR=$(DUNE_BUILD_DIR)/runtime

lit_test: build
	env PATH="`pwd`/tests/scripts:${PATH}" ${LIT} tests/lit $(LIT_VERBOSITY)

TEST_ENV += AC_TYPES_DIR="`pwd`/runtime/external/ac_types"
TEST_ENV += SC_TYPES_DIR="`pwd`/runtime/external/systemc/build-install"

WIDE_BITINT_SUPPORTED := `$(MAKE) -C runtime/test wide_bitint_supported`

BACKENDS = interpreter c23 ac fallback sc

test_backends: ${addprefix test_backend_, ${BACKENDS}}

test_backend_%: build
	env PATH="${CURDIR}/tests/scripts:$${PATH}" ${TEST_ENV} ASL_BACKEND=$* ${LIT} tests/backends $(LIT_VERBOSITY)


test_demos: ${addprefix test_demo_, $(filter-out interpreter ac sc, ${BACKENDS})}

test_demo_%: build
	$(MAKE) -C demo clean test_$*

test_demo_interpreter : build
	$(MAKE) -C demo test

demo_interpreter : build
	$(MAKE) -C demo demo

build_systemc:
	test -f runtime/external/systemc/build-install/lib/libsystemc.a || \
	( mkdir -p runtime/external/systemc/build && \
	mkdir -p runtime/external/systemc/build-install && \
	cmake runtime/external/systemc/ -B runtime/external/systemc/build -DCMAKE_INSTALL_PREFIX=runtime/external/systemc/build-install -DCMAKE_CXX_STANDARD=17 -DBUILD_SHARED_LIBS=OFF && \
	cmake --build runtime/external/systemc/build && \
	cmake --install runtime/external/systemc/build )

test_backend_sc: build_systemc

################################################################
# End
################################################################
