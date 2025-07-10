#! /usr/bin/env python3

"""
Build asli and install it into directory without including
a full copy of ~/.opam.

Typical usage:

    cd asl-interpreter
    ./mk_install.py --release-dir=$HOME/release --version=`date -I`
    # will be released into ${release_dir}/${version}

"""

import argparse
import os
import os.path
import pathlib
import shutil
import subprocess
import sys
import textwrap

# Run command (printing command first if verbose) and abort if command fails
# (The assumption is that the command printed a useful/meaningful error message already)
def run(cmd):
    print(" ".join(cmd))
    try:
        subprocess.run(cmd, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Command '{' '.join(e.cmd)}' returned non-zero exit status {e.returncode}.")
        sys.exit(e.returncode)

def main() -> int:
    parser = argparse.ArgumentParser(
            prog = 'mk_install.py',
            description = __doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            )
    parser.add_argument("--release-dir", help="release directory", metavar="release_dir", required=True)
    parser.add_argument("--version", help="version (used as subdirectory name)", metavar="version", required=True)
    args = parser.parse_args()

    opam_switch = os.environ.get('OPAM_SWITCH_PREFIX')
    release_dir = f"{args.release_dir}/{args.version}"
    release_dir = os.path.abspath(release_dir)
    bin_dir     = f"{release_dir}/bin"
    lib_dir     = f"{release_dir}/lib"

    if os.path.exists(release_dir):
        print(f"Error: release_dir '{release_dir}' already exists")
        sys.exit(1)

    print(f"Building release")

    run(["make", "build"])

    print(f"Copying release to {release_dir}")

    shutil.copytree(f"_build/install/default", release_dir, dirs_exist_ok=True)
    shutil.rmtree(f"{release_dir}/lib/asli/libASL") # just a copy of the source code

    # copy Z3 file over into release (user will need to set LD_LIBRARY_PATH to lib_dir)
    shutil.copy(f"{opam_switch}/lib/stublibs/libz3.so", lib_dir)

    # copy shell scripts that set environment variables
    shutil.copy("share/env.sh", release_dir)
    shutil.copy("share/env.csh", release_dir)
    print(f"Generated environment setting files in {release_dir}/env.{{sh,csh}}")

    print(f"Copied release to {release_dir}")

    sys.exit(0)


if __name__ == "__main__":
    main()
