// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func FUT(x : boolean) => boolean
begin
    print("A"); println();
    if x then
        return x;
    end
    // next statement not reached if x
    print("B"); println();
    return !x;
end

func main() => integer
begin
    print(FUT(TRUE)); println();
    // CHECK: A
    // CHECK-NOT: B
    // CHECK: TRUE
    print(FUT(FALSE)); println();
    // CHECK: A
    // CHECK: B
    // CHECK: TRUE

    return 0;
end
