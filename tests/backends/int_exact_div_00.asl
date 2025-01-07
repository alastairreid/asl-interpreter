// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer, y : integer) => integer
begin
    return x DIV y;
end

func main() => integer
begin
    print_int_dec(Test(33, 3)); println();
    // CHECK: 11
    return 0;
end
