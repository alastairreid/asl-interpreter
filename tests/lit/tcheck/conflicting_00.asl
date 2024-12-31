// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F();
func F();
// CHECK: Type error: function `F` was previously defined
