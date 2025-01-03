// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : integer) => integer
begin
    case x of
        when 1 => return 3;
        when 2 => return 4;
        when 3,4,5 => return 5;
        // missing otherwise - error if no match
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test(0)); println();
    // CHECK: unmatched case

    return 0;
end

