// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    let y = x;
    let - = x;
    return x;
end

func main() => integer
begin
    print_int_dec(Test(3)); println();
    // CHECK: 3

    return 0;
end

