// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

type E0 of exception;
type E1 of exception{ payload : boolean };

func FUT?(x : integer) => integer
begin
    if x == 0 then
        throw E0;
    elsif x == 1 then
        throw E1{ payload=TRUE };
    elsif x == 2 then
        throw E1{ payload=FALSE };
    else
        return 42;
    end
    return -1;
end

func main() => integer
begin
    print(FUT?(0)); println();
    // CHECK: ASL error: uncaught exception

    return 0;
end
