// RUN: %aslrun %s | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

record R {
    f0 : bits(4);
    f1 : boolean;
};

func FUT1(x : R, y : bits(4)) => R
begin
    return x with { f0 = y };
end

func FUT2(x : R, y : boolean) => R
begin
    return x with { f1 = y };
end

func printR(x : R)
begin
    print("R{");
    print("f0="); print_bits_hex(x.f0);
    print(", ");
    print("f1="); print(x.f1);
    print("}");
end

func main() => integer
begin
    let r = R{f0 = '0000', f1 = FALSE};
    printR(FUT1(r, '1001')); println();
    // CHECK: R{f0=4'x9, f1=FALSE}

    printR(FUT1(r, '1011')); println();
    // CHECK: R{f0=4'xb, f1=FALSE}

    printR(FUT2(r, TRUE)); println();
    // CHECK: R{f0=4'x0, f1=TRUE}

    return 0;
end
