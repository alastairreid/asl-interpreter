// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

func Test(x : boolean) => integer
begin
    case x of
        when TRUE => return 3;
        otherwise => return 10;
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test(FALSE)); println();
    // CHECK: 10
    print_int_dec(Test(TRUE)); println();
    // CHECK: 3

    return 0;
end
