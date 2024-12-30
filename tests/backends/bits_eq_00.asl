// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : bits(8), y : bits(8)) => boolean
begin
    return asl_eq_bits(x, y);
end

func main() => integer
begin
    print(FUT(8'x1, 8'x1); println();
    // CHECK: TRUE
    print(FUT(8'x1, 8'x2); println();
    // CHECK: FALSE
    print(FUT(8'x2, 8'x1); println();
    // CHECK: FALSE
    print(FUT(8'x2, 8'x2); println();
    // CHECK: TRUE

    return 0;
end
