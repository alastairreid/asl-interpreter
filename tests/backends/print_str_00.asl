// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func main() => integer
begin
    print_str("Hello, world!");
    // CHECK: Hello, world!

    return 0;
end

