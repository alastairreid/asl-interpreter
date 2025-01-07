// RUN: not %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

// UNSUPPORTED: interpreter

func Test(x : integer) => integer
begin
    case x of
        when 0x1_0000_0000_0000_0000 => return 1;
        // CHECK: Unimplemented large (> 64 bit) integer pattern
        otherwise => return 10;
    end
    return 20; // unreachable
end

func main() => integer
begin
    print_int_dec(Test(-1)); println();

    return 0;
end

