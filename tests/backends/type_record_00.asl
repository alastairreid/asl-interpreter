// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

record X {
  f : bits(4);
  g : bits(4);
};

func Test(r : X) => bits(4)
begin
    return r.f;
end

func main() => integer
begin
    var r1 : X;
    r1.f = '0101';
    r1.g = '0011';

    print_bits_hex(Test(r1)); println();
    // CHECK: 4'x5

    let r2 = X{ f='1000', g='1001' };
    print_bits_hex(Test(r2)); println();
    // CHECK: 4'x8

    return 0;
end
