// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

setter A[] = value : integer;

func T()
begin
    A[] = TRUE;
// CHECK: Type error: type integer does not match boolean
end
