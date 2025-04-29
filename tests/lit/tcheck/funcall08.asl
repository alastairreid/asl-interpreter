// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : boolean, y : boolean, z : boolean);

func main() => integer
begin
    F(0, 1, 2);
    // CHECK: While typechecking a call to 'F' with arguments
    // CHECK:   F(integer{0}, integer{1}, integer{2})
    // CHECK: Type error in procedure arguments for call to 'F'
    // CHECK    F{}(x : boolean, y : boolean, z : boolean) => ()
end
