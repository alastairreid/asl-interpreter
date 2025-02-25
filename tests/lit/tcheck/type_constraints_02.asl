// RUN: %asli --batchmode --check-constraints %s
// Copyright (C) 2023-2025 Intel Corporation

func W(i : integer {0..3})
begin
end

func FUT()
begin
    for i = 0 to 3 do
        W(i);
    end
end
