// RUN: %aslrun %s --extra-c=%S/ffi_config_00.c | filecheck %s
// Copyright (C) 2025-2026 Intel Corporation

// UNSUPPORTED: interpreter

config ConfigBool : Boolean := False;
config ConfigInt : {1, 2, 3, 4} := 1;

function main() -> Builtin::Foreign::CInt
begin
    // Any changes to configuration variables should be performed
    // before any ASL code is executed so this test depends on
    // the accompanying C file defining a function that is
    // executed before main that will change the values of the
    // configuration variables and is expected to produce the
    // following output.

    // CHECK: Changing ConfigBool from false to true
    // CHECK: Changing ConfigInt from 1 to 4

    Std::Print::Boolean(ConfigBool); Print("\n");
    // CHECK: True
    Std::Print::Integer::Dec(ConfigInt); Print("\n");
    // CHECK: 4

    return Builtin::Foreign::CInt::From_Integer(0);
end
