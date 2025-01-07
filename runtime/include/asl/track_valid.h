////////////////////////////////////////////////////////////////
// Runtime library for ASL's C backend to support tracking
// validity masks
//
// Copyright (C) 2024-2025 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_TRACK_VALID_H
#define ASL_TRACK_VALID_H

#ifdef __cplusplus
extern "C" {
#endif

/*
   Function for tracking validity mask of variables of bitvector
   type.

   var_name is the variable name, listed in the "track-valid" section
   of the input .json file.

   low and width denote bitvector field/slice that has an arbitrary
   (invalid) value. low is the number of the least significant bit
   in the slice and width is the width of the slice.
*/
void ASL_fuzz(const char *var_name, int low, int width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_TRACK_VALID_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
