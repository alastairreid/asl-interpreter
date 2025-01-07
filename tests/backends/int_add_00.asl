// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x + y;
end

func main() => integer
begin
    print_int_dec(Test(1, 2)); println();
    // CHECK: 3
    return 0;
end
