// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test1(x : integer) => integer {0..255}
begin
    return x as {0..255};
end

func Test2(x : integer) => integer {0..255}
begin
    return x as integer {0..255};
end

func main() => integer
begin
    print_int_dec(Test1(4)); println();
    // CHECK: 4
    print_int_dec(Test2(5)); println();
    // CHECK: 5

    return 0;
end


