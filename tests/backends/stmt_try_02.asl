// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

type E0 of exception;
type E1 of exception{ payload : boolean };
type E2 of exception;

func FUT?(x : integer) => integer
begin
    if x == 0 then
        throw E0;
    elsif x == 1 then
        throw E1{ payload=TRUE };
    elsif x == 2 then
        throw E1{ payload=FALSE };
    elsif x == 3 then
        throw E2;
    else
        return 42;
    end
    return -1;
end

func FUT2?(x : integer)
begin
    try
        print(FUT?(x)); println();
        print("Did not throw\n");
    catch
        when e : E0 => print("Caught exception E0"); println();
        when e : E1 =>
            if e.payload then
                print("Caught exception E1{TRUE}"); println();
            else
                print("Caught exception E1{FALSE}"); println();
            end
        otherwise =>
            print("Caught another exception"); println();
    end
    print("Exited try block"); println();
end

func main() => integer
begin
    FUT2?(0);
    // CHECK: Caught exception E0
    // CHECK: Exited try block

    FUT2?(1);
    // CHECK: Caught exception E1{TRUE}
    // CHECK: Exited try block

    FUT2?(2);
    // CHECK: Caught exception E1{FALSE}
    // CHECK: Exited try block

    FUT2?(3);
    // CHECK: Caught another exception
    // CHECK: Exited try block

    FUT2?(4);
    // CHECK: 0x2a
    // CHECK: Did not throw
    // CHECK: Exited try block

    return 0;
end

