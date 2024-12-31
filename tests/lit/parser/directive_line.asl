// RUN: not %asli --batchmode %s || filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

# 42 "foobar.asl"
x == 0
// CHECK: File "foobar.asl", line 42, characters 0-1:
// CHECK: Syntax error before 'x'.
