// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

// UNSUPPORTED: interpreter

func Test(x : bits(65)) => integer
begin
    case x of
        when 65'x0 => return 1;
        // CHECK: Unimplemented large (> 64 bit) bitvector pattern
        otherwise => return 10;
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test(65'x1)); println();

    return 0;
end


