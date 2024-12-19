// RUN: not %asli --nobanner %s | %decolor | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

type B of bits(8) {
    [7]    X
    [6]    X
// CHECK: Type error: fieldname `X` is declared multiple times
};
