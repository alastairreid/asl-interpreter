################################################################
# ASL runtime Makefile
#
# Copyright (C) 2023-2026 Intel Corporation
# SPDX-License-Identifier: BSD-3-Clause
################################################################

.PHONY: default all clean test

BUILD_DIR ?= $(CURDIR)/build
LIB := $(BUILD_DIR)/libASL.a

default: $(LIB)

ifeq ($(origin AR),default)
  AR = ar
endif
ifeq ($(origin CC),default)
  CC = gcc
endif
CPPFLAGS = -Iinclude
CFLAGS = -g -Wall -Werror -Wconversion -std=c99 -fPIC
OBJEXT ?= o

C_FILES = bits8192.c \
          bits4096.c \
          bits2048.c \
          bits1024.c \
          bits128.c \
          bits256.c \
          bits512.c \
          bits64.c \
          error.c \
          end_of_execution.c \
          info.c \
          ram.c

OBJ_FILES = $(C_FILES:%.c=$(BUILD_DIR)/%.$(OBJEXT))
DEP_FILES = $(OBJ_FILES:%.$(OBJEXT)=%.d)

-include $(DEP_FILES)

$(BUILD_DIR)/%.$(OBJEXT): lib/%.c | build_dir
	$(info CC      $(@F))
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -c $< -o $@

$(LIB): $(OBJ_FILES)
	$(RM) $@
	$(info AR      $(@F))
	$(AR) cr $@ $^

build_dir:
	mkdir -p $(BUILD_DIR)

clean:
	$(RM) -r $(BUILD_DIR)

test: $(LIB)
	$(MAKE) -C test BUILD_DIR=$(BUILD_DIR)/test
