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

def write_to_file(filename, contents):
    with open(filename, 'w') as f:
        f.write(contents)

def main() -> int:
    parser = argparse.ArgumentParser(
            prog = 'mk_install',
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
    sh_file     = f"{release_dir}/env.sh"
    csh_file    = f"{release_dir}/env.csh"

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

    print(f"Copied release in {release_dir}")

    write_to_file(sh_file, textwrap.dedent(f"""
export ASLI_INSTALL_DIR={release_dir}
export ASLI_BIN_DIR={bin_dir}
export LD_LIBRARY_PATH={lib_dir}
export PATH={bin_dir}:$PATH
    """))

    write_to_file(csh_file, textwrap.dedent(f"""
setenv ASLI_INSTALL_DIR {release_dir}
setenv ASLI_BIN_DIR {bin_dir}
setenv LD_LIBRARY_PATH {lib_dir}
setenv PATH {bin_dir}:$PATH
    """))

    print(f"Generated environment setting files in {sh_file} and {csh_file}")

    sys.exit(0)


if __name__ == "__main__":
    main()

