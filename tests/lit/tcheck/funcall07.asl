// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

func main() => integer
begin
    F(0, 1, 2);
    // CHECK: Type error: Unknown procedure F
end
