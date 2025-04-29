// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : integer);
func F(x : boolean);

func main() => integer
begin
    F('0');
    // CHECK: While typechecking a call to 'F' with arguments
    // CHECK:   F(bits(1))
    // CHECK: Unable to decide which of the following definitions of 'F' to call
    // CHECK:   F{}(x : boolean) => ()
    // CHECK:   F{}(x : integer) => ()
end
