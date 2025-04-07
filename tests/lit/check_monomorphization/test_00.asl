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

// CHECK:       WARNING: Bitwidth parameters are not statically known in the following definitions.
// CHECK-NEXT:  This will cause code generation to fail.
//
// CHECK:       The bitwidth parameters in this call-graph are not statically known
// CHECK-NEXT:      A with bitwidth parameters {}
// CHECK-NEXT:        B with bitwidth parameters {N}
// CHECK-NEXT:          C with bitwidth parameters {N}
//
// CHECK:       The simplest way to fix monomorphization issues is usually
// CHECK-NEXT:  to change the topmost function in the call-graph so that
// CHECK-NEXT:  the parameters to the function it calls are statically known.
//
// CHECK:       For example, you might use an if-statement or case-statement to separate
// CHECK-NEXT:  the cases for different parameter sizes.
// CHECK-NEXT:  This is often easiest to do in machine generated code such as instruction decoders.
