// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    return __let r : integer = x*x __in r+r;
end

func main() => integer
begin
    print_int_dec(Test(5)); println();
    // CHECK: 50

    return 0;
end


