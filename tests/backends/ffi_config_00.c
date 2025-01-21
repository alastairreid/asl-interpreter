// FFI testing support functions for configuration variables
//
// Copyright (C) 2025-2025 Intel Corporation

#include <stdio.h>
#include "asl_ffi.h"

// The following function will be executed *before* main
void __attribute__((constructor)) FFI_Init_Configs()
{
        printf("Changing ConfigBool from %s to true\n",
               ASL_get_config_ConfigBool() ? "true" : "false");
        ASL_set_config_ConfigBool(true);

        printf("Changing ConfigInt from %d to 4\n",
               ASL_get_config_ConfigInt());
        ASL_set_config_ConfigInt(4);
}
