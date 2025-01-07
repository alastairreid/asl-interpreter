// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : boolean) => boolean
begin
    return x;
end

func main() => integer
begin
    print(FUT(TRUE)); println();
    // CHECK: TRUE
    print(FUT(FALSE)); println();
    // CHECK: FALSE

    return 0;
end
