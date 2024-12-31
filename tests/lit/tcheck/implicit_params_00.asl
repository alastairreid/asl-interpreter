// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F() => bits(N);
// CHECK: Type error: the width parameter(s) `N` of the return type cannot be determined from the function arguments
