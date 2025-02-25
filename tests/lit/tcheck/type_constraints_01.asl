// RUN: not %asli --batchmode --check-constraints %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func W(i : integer {0..3})
begin
end

func FUT()
begin
    for i = 0 to 4 do
        W(i);
        // CHECK: Type error: `{0..4}` is not a subrange of `{0..3}`
    end
end
