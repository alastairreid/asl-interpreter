// RUN: %asli --batchmode %s
// Copyright (C) 2025-2025 Intel Corporation

func F(x : array [size] of integer) => integer
begin
    var sum = 0;
    for i = 0 to size-1 do
        sum = sum + x[i];
    end
    return sum;
end

func main() => integer
begin
    let x : integer = (F(array (1,2,3)));
    let y : integer = (F(array (1)));
end
