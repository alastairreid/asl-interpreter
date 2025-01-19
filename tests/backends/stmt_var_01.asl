// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : integer) => integer
begin
    var y = x;
    y = x + 1;
    return y;
end

func main() => integer
begin
    print_int_dec(Test(3)); println();
    // CHECK: 4

    return 0;
end
