################################################################
# ASL runtime tests Makefile
#
# Copyright (C) 2023-2024 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.PHONY: default gtest

CMAKE = cmake

GTEST_DIR = ../external/googletest
GTEST_BUILD_DIR = $(BUILD_DIR)/gtest
OBJ_DIR = $(BUILD_DIR)/obj
TESTS_BIN = $(BUILD_DIR)/tests

default: $(TESTS_BIN)

CXX = g++
CPPFLAGS = -I$(GTEST_DIR)/googletest/include -I../include
CXXFLAGS = -g -Wall -Werror
CXXFLAGS += -std=c++17

CC_FILES = integer_test.cc bits_test.cc ram_test.cc
OBJ_FILES = $(CC_FILES:%.cc=$(OBJ_DIR)/%.o)
DEP_FILES = $(OBJ_FILES:%.o=%.d)

-include $(DEP_FILES)

$(OBJ_DIR)/%.o: %.cc | obj_dir
	$(info CXX     $(@F))
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -MMD -c $< -o $@

obj_dir:
	mkdir -p $(OBJ_DIR)

gtest: | gtest_build_dir
	$(CMAKE) -S $(GTEST_DIR) -B $(GTEST_BUILD_DIR) -DBUILD_GMOCK=OFF
	$(MAKE) -C $(GTEST_BUILD_DIR)

gtest_build_dir:
	mkdir -p $(GTEST_BUILD_DIR)

$(TESTS_BIN): gtest $(OBJ_FILES)
	$(info CXX     $(@F))
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OBJ_FILES) -L$(BUILD_DIR)/.. -lASL -L$(GTEST_BUILD_DIR)/lib -lgtest -lgtest_main -o $@
	$(TESTS_BIN) --gtest_brief
