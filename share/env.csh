#!/bin/tcsh

# Auto-detect the directory where this script is located
# In tcsh, we need to use a different approach since $0 behaves differently when sourced

# Get the script path - this works whether the script is executed or sourced
if ( $?0 ) then
    # Script is being executed directly
    set script_path = "$0"
else
    # Script is being sourced - use the last argument from history
    set script_path = "`history 1 | awk '{print $NF}'`"
    # If that doesn't work, try to get it from the shell stack
    if ( ! -f "$script_path" ) then
        set script_path = "${_}"
    endif
endif

# Resolve the directory path
set SCRIPT_DIR = "`dirname $script_path`"
set SCRIPT_DIR = "`cd $SCRIPT_DIR && pwd`"

# Set environment variables relative to the script location
setenv ASLI_INSTALL_DIR "${SCRIPT_DIR}"
setenv ASLI_BIN_DIR "${SCRIPT_DIR}/bin"
setenv LD_LIBRARY_PATH "${SCRIPT_DIR}/lib:${LD_LIBRARY_PATH}"
setenv PATH "${SCRIPT_DIR}/bin:${PATH}"

# Clean up temporary variables
unset script_path
unset SCRIPT_DIR

echo "Environment configured for ASLi installation at: ASLI_INSTALL_DIR = ${ASLI_INSTALL_DIR}"
