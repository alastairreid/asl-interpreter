// RUN: %asli --batchmode %s
// Copyright (C) 2023-2025 Intel Corporation

// Correct type parameters: N can be synthesized from the type of x
func F1{N : integer}(x : bits(N)) => boolean;

// Correct function type: we can synthesize N from the type of x
func F2(x : bits(N)) => boolean;

// Correct function type: N is an explicit argument
func F3(N : integer, x : bits(N)) => boolean;

// Correct function type: N is an explicit argument and argument order does not matter
func F4(x : bits(N), N : integer) => boolean;

// Correct function type: N is an explicit argument
func F5(N : integer) => bits(N);

// Correct function type: N is an explicit parameter and an explicit argument
func F6{N : integer}(N : integer) => bits(N);

// Correct function type: N is an explicit parameter and an explicit argument
func F7{N : integer}(N : integer, x : bits(N)) => bits(N);
