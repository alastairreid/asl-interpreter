// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func FUT(x : boolean)
begin
    print("A"); println();
    if x then
        return;
    end
    // next statement not reached if x
    print("B"); println();
    return;
end

func main() => integer
begin
    FUT(TRUE);
    // CHECK: A
    // CHECK-NOT: B
    FUT(FALSE);
    // CHECK: A
    // CHECK: B

    return 0;
end

