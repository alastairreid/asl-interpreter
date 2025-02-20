// RUN: not %asli --max-errors=10 --check-exception-markers --check-call-markers --batchmode %s | filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

type E of exception;

func NoThrow1() => integer
begin
    return 0;
end

func NoThrow2()
begin
    return;
end

func MayThrow1?() => integer
begin
    if TRUE then throw E; end
    return 0;
end

func MayThrow2?()
begin
    if TRUE then throw E; end
end

func AlwaysThrow!()
begin
    throw E;
end

func F1() => integer
begin
    let x = NoThrow1?();
    // CHECK: Type error: Exception marker '?' on call to 'NoThrow1.0' does not match exception marker '' on definition
end

func F2() => integer
begin
    NoThrow2!();
    // CHECK: Type error: Exception marker '!' on call to 'NoThrow2.0' does not match exception marker '' on definition
end

func F3() => integer
begin
    let x = MayThrow1();
    // CHECK: Type error: Exception marker '' on call to 'MayThrow1.0' does not match exception marker '?' on definition
end

func F4() => integer
begin
    MayThrow2!();
    // CHECK: Type error: Exception marker '!' on call to 'MayThrow2.0' does not match exception marker '?' on definition
end

func F5() => integer
begin
    AlwaysThrow();
    // CHECK: Type error: Exception marker '' on call to 'AlwaysThrow.0' does not match exception marker '!' on definition
end

func F6() => integer
begin
    AlwaysThrow?();
    // CHECK: Type error: Exception marker '?' on call to 'AlwaysThrow.0' does not match exception marker '!' on definition
end
