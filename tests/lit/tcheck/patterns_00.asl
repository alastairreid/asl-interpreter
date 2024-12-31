// RUN: not %asli --batchmode %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

var X : integer;

func F(x : integer) => boolean
begin
    case x of
        when X => return TRUE;
        otherwise => return FALSE;
    end
// CHECK: Type error: pattern match of `X` should be a constant.
end
