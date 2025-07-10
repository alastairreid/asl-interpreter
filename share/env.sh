#!/bin/bash

# Auto-detect the directory where this script is located (using realpath if available)
if command -v realpath >/dev/null 2>&1; then
    SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
else
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

# Set environment variables relative to the script location
export ASLI_INSTALL_DIR="${SCRIPT_DIR}"
export ASLI_BIN_DIR="${SCRIPT_DIR}/bin"
export LD_LIBRARY_PATH="${SCRIPT_DIR}/lib:${LD_LIBRARY_PATH}"
export PATH="${SCRIPT_DIR}/bin:$PATH"

echo "Environment configured for ASLi installation at: ASLI_INSTALL_DIR = ${ASLI_INSTALL_DIR}"
