// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

type E of exception;

func Test(x : integer) => integer
begin
    try
        if x < 0 then
            print("THROWING\n");
            throw E;
        end
    catch
        when e : E =>
            print("CAUGHT\n");
            return -x;
    end
    return x;
end

func main() => integer
begin
    print_int_dec(Test?(1)); println();
    // CHECK: 1
    print_int_dec(Test?(-2)); println();
    // CHECK: THROWING
    // CHECK: CAUGHT
    // CHECK: 2
    print_int_dec(Test?(3)); println();
    // CHECK: 3

    return 0;
end

