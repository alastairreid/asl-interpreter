#!/bin/tcsh

set called=($_)
if ("$called" != "") then
    set script_path = $called[2] # the script was sourced
endif
if ("$0" != "tcsh") then
    set script_path = $0         # the script was run
endif

# Resolve the directory path
set SCRIPT_DIR = "`dirname $script_path`"
set SCRIPT_DIR = "`cd $SCRIPT_DIR && pwd`"

# Set environment variables relative to the script location
setenv ASLI_INSTALL_DIR "${SCRIPT_DIR}"
setenv ASLI_BIN_DIR "${SCRIPT_DIR}/bin"
if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH "${SCRIPT_DIR}/lib:${LD_LIBRARY_PATH}"
else
    setenv LD_LIBRARY_PATH "${SCRIPT_DIR}/lib"
endif
setenv PATH "${SCRIPT_DIR}/bin:${PATH}"
rehash

# Clean up temporary variables
unset script_path
unset SCRIPT_DIR

echo "Environment configured for ASLi installation at: ASLI_INSTALL_DIR = ${ASLI_INSTALL_DIR}"
