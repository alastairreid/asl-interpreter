// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func F();
func F();
// CHECK: Type error: function `F` was previously defined
