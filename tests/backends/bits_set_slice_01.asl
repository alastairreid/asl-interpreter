// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2024 Intel Corporation

func Test(x : bits(4), y : bits(4)) => bits(16)
begin
    var result : bits(16);
    result = asl_zeros_bits(16);

    for i = 0 to 3 do
        if x[i +: 1] == '1' then
            result[i * 4 +: 4] = y;
        end
    end
    return result;
end

func main() => integer
begin
    print_bits_hex(Test('1100', '1111')); println();
    // CHECK: 16'xff00
    return 0;
end
