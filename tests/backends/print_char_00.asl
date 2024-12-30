// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func main() => integer
begin
    print_char(65);
    // CHECK: A
    print_char(66);
    // CHECK: B
    print_char(67);
    // CHECK: C

    return 0;
end
