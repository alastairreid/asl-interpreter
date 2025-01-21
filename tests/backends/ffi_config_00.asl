// RUN: %aslrun %s --extra-c=%S/ffi_config_00.c | filecheck %s
// Copyright (C) 2025-2025 Intel Corporation

// UNSUPPORTED: interpreter

config ConfigBool : boolean = FALSE;
config ConfigInt : integer {1, 2, 3, 4} = 1;

func main() => integer
begin
    // Any changes to configuration variables should be performed
    // before any ASL code is executed so this test depends on
    // the accompanying C file defining a function that is
    // executed before main that will change the values of the
    // configuration variables and is expected to produce the
    // following output.

    // CHECK: Changing ConfigBool from false to true
    // CHECK: Changing ConfigInt from 1 to 4

    print(ConfigBool); println();
    // CHECK: TRUE
    print_int_dec(ConfigInt); println();
    // CHECK: 4

    return 0;
end
