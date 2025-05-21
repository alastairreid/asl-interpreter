// RUN: %asli --batchmode --exec=":xform_lower" --exec=":xform_constprop" "--exec=:xform_monomorphize" --exec=":show Reduce_N_4*" --exec=":show Reduce_N_2*" --exec=":show Reduce_N_1*" %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func Reduce{N}(x : bits(N * esize), esize : integer, N : integer) => bits(esize)
begin
    if N == 1 then
        return x[0 +: esize];
    else
        let N1 = N DIV 2; // size of bottom part
        let N2 = N - N1; // size of top part
        let result1 = Reduce(x[0 +: N1*esize], esize, N1);
        let result2 = Reduce(x[N1*esize +: N2*esize], esize, N2);
        return result1 + result2;
    end
end

// CHECK: func Reduce_N_4_esize_32.0{}(x : bits(128)) => bits(32)
// CHECK: begin
// CHECK:     let N1 : integer{2} = 2;
// CHECK:     let N2 : integer{2} = 2;
// CHECK:     let result1 : bits(32) = Reduce_N_2_esize_32.0(x[0 +: 64]);
// CHECK:     let result2 : bits(32) = Reduce_N_2_esize_32.0(x[64 +: 64]);
// CHECK:     return result1 + result2;
// CHECK: end

// CHECK: func Reduce_N_2_esize_32.0{}(x : bits(64)) => bits(32)
// CHECK: begin
// CHECK:     let N1 : integer{1} = 1;
// CHECK:     let N2 : integer{1} = 1;
// CHECK:     let result1 : bits(32) = Reduce_N_1_esize_32.0(x[0 +: 32]);
// CHECK:     let result2 : bits(32) = Reduce_N_1_esize_32.0(x[32 +: 32]);
// CHECK:     return result1 + result2;
// CHECK: end

// CHECK: func Reduce_N_1_esize_32.0{}(x : bits(32)) => bits(32)
// CHECK: begin
// CHECK:     return x[0 +: 32];
// CHECK: end

func FUT1(x : bits(128)) => bits(32)
begin
    return Reduce(x, 32, 4);
end
