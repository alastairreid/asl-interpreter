// RUN: %asli --batchmode --configuration=%S/config.json --project=%S/asli.prj %s | filecheck %s
// Copyright (C) 2024-2025 Intel Corporation

func C(x : bits(N)) => integer
begin
    return N;
end

func B(x : bits(N)) => integer
begin
    return C(x);
end

func A() => integer
begin
    return B('0');
end

// CHECK:       WARNING: Monomorphization failed for the following definitions.
// CHECK:       This will cause code generation to fail.
// CHECK:       A
// CHECK-NEXT:    B
// CHECK-NEXT:      C
