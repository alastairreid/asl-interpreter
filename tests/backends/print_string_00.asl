// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func main() => integer
begin
    print_string("Hello, world!");
    // CHECK: Hello, world!

    return 0;
end

