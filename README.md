# ISA Interpreter
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/IntelLabs/isa-tools/badge)](https://scorecard.dev/viewer/?uri=github.com/IntelLabs/isa-tools)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/9891/badge)](https://www.bestpractices.dev/projects/9891)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

## Overview

Intel&reg; ISA Specification Language is an executable language for writing
clear, precise specifications of Instruction Set Architectures (ISAs).

The ISA interpreter (iii) is an implementation of the language that can execute .isa
specifications either in an interpreter or by compiling via C code.  We include
a small demonstration of how to use iii to build simulators for a toy
architecture specification.

This tool is based on Arm's open source asl-interpreter release.


# Roadmap

This is intended as a preview to enable research collaboration.  Over time, we
aim to turn iii into a more trustworthy and flexible tool that you can use to
build your own tools around .isa specifications.

The tool does not implement the full .isa language yet. In particular, we are
still refining the design of the module system and the foreign function interface.

The tool has a number of known bugs and we are discovering more as we improve
our testing framework. Please report any bugs that you run into and be patient
with us as we try to balance bugfixes against adding new features.


The tool is still in a state where we may make large changes to the tool and in
how we use the tool so please tell us what changes you would like to see and
contact us in advance if you are considering making major changes yourself.


# Contributing

The ISA interpreter project welcomes external contributions through pull
requests to the `main` branch.

Please refer to the [Contributing](CONTRIBUTING.md) for additional information on
the contribution acceptance guidelines.

We use signed commits, please remember to sign your commits before making a
pull request.  See instructions
[here](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/managing-repository-settings/managing-the-commit-signoff-policy-for-your-repository#about-commit-signoffs)
for how to sign commits.

Please run the tests by running `make test` before creating a pull request.


# Feedback
We encourage feedback and suggestions via
[GitHub Issues](https://github.com/IntelLabs/isa-tools/issues) as well
as via
[GitHub Discussions](https://github.com/IntelLabs/isa-tools/discussions).


## Requirements and Building

### On Ubuntu

To build and run the ISA interpreter, you will need to install OCaml.
The following commands are sufficient to install requirements and to build iii on Ubuntu systems.

    apt update
    apt install -y libgmp-dev opam pkg-config
    pip3 install -r requirements.txt
    opam init --no-setup --disable-sandboxing --compiler=5.4.1
    opam install . --deps-only --with-test --with-doc --locked --yes
    eval $(opam env)

    git submodule init
    git submodule update
    make build

Some of the more advanced usage of iii depends on the LLVM C compiler "clang"
version 16 or higher. If you don't already have this installed, it can be
installed using the following commands.

    # install clang-16
    apt install -y lsb-release wget software-properties-common gnupg
    wget https://apt.llvm.org/llvm.sh
    chmod +x llvm.sh
    ./llvm.sh 16

### On macOS using Homebrew

To run regression tests and/or use the C23 backend on Macs, you need to
install clang-16.  Using a more recent version of clang such as clang-19 will
not work because it limits the use of bit-precise integers to 128 bits.

    brew install llvm@16
    brew install cmake
    echo 'export PATH="/opt/homebrew/opt/llvm@16/bin:$PATH"' >> ~/.zshrc
    sudo bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
    opam update
    opam init
    eval $(opam env --switch=default)
    pip3 install -r requirements.txt
    opam init --no-setup --disable-sandboxing --compiler=5.4.1
    opam install . --deps-only --with-test --with-doc --locked --yes
    eval $(opam env)
    git submodule init
    git submodule update
    make build

## License and contribution

The software is provided under the [BSD-3-Clause license](https://spdx.org/licenses/BSD-3-Clause.html).
Contributions to this project are accepted under the same license.

This software includes code from one open source project.

 * The [CIL project](https://people.eecs.berkeley.edu/~necula/cil/)
   defines a useful
   [visitor class](https://github.com/cil-project/cil/blob/936b04103eb573f320c6badf280e8bb17f6e7b26/src/cil.ml#L931)
   for traversing C ASTs.
   The file `visitor.ml` is a modified copy of this class that generalizes
   the type to work with an arbitrary AST.

   CIL is distributed under a [BSD-3-Clause license](https://github.com/cil-project/cil/blob/develop/LICENSE).


## Building and development

### Cloning the repository

Since the repository contains submodules, be sure to recursively clone it:

```
    git clone --recursive https://github.com/IntelLabs/isa-tools.git
```

### Directory structure

This interpreter consists of a single directory organized as follows

  * Metadata, documentation, etc:
      * `LICENCE`             - Software licence
      * `README.md`           - This file
      * `CHANGES.md`          - Changelog
      * `CONTRIBUTING.md`     - How to contribute to this project
      * `Security.md`         - How to report security concerns
      * `Makefile`            - Build system file
  * Source code consisting of
      * Abstract syntax tree
          * `libISA/isa_ast.ml`      - ISA Abstract Syntax Tree
      * Lexer
          * `libISA/isa_lexer.mll`   - ISA lexer (ocamllex file)
      * Grammar and Parser
          * `libISA/isa_parser.mly`  - ISA parser
          * `libISA/isa_visitor.ml`  - code to traverse abstract syntax tree
          * `libISA/isa_utils.ml`    - code to transform abstract syntax tree
      * Typechecker
          * `libISA/tcheck.ml`       - typechecker
      * Interpreter
          * `libISA/primops.ml`      - implementation of ISA builtin types and operations
          * `libISA/value.ml`        - interpreter support code
          * `libISA/eval.ml`         - evaluator for ISA language
      * ISA standard library
          * `stdlib.isa`             - builtin types and functions
      * Code generation
          * `libISA/xform_*.ml`      - transformations on the AST
          * `libISA/backend_*.ml`    - code generation backends
          * `libISA/runtime*.ml`     - runtime-specific code generation
          * `runtime`                - runtime library for generated C
      * Programs
          * `bin/iii.ml`             - interactive ISA tool
          * `bin/iic.py`             - C code generation tool
      * Misc
          * `libISA/utils.ml`        - utility code
  * Code copied from other open source projects
      * `libISA/visitor.ml`


### Editor plugins

For VIM:
You can copy `editors/isa.vim` to `~/.vim/syntax/isa.vim` and use `:set syntax=isa` to enable syntax highlighting of .isa code.
And you can add the line `au BufRead,BufNewFile *.isa set filetype=isa` to `~/.vimrc` to
automatically use this for all isa files.


### Using ISA interpreter

The ISA interpreter reads .isa files specified on the command line and
provides an interactive environment for executing ISA
statements and expressions.

```
    $ iii
     _____  _____
    |_   _|/ ____|  /\      Intel(R) ISA specification tool
      | | | (___   /  \     Copyright (C) 2022-2026 Intel Corporation
      | |  \___ \ / /\ \    Copyright Arm Limited (c) 2017-2019
     _| |_ ____) / ____ \
    |_____|_____/_/    \_\  ISA compiler 0.0.0

    Type :? for help
    ISA> 1+1
    2
    ISA> Zero_Extend('11', 32)
    32'x3
    ISA> let x : Bits(32) = Zero_Extend('11', 32);
    ISA> x
    32'x3
    ISA> :quit
```

The ISA interpreter needs `stdlib.isa` which is part of this repository. You
either run the ISA interpreter from a directory containing `stdlib.isa` or run
the ISA interpreter from anywhere by setting `ISA_PATH` to point to a
directory containing `stdlib.isa`.

### Using the ISA interpreter to model an ISA

To demonstrate how iii can be used to model an ISA, we wrote a *trivial* ISA with just two instructions: Increment and Halt.

The demo directory contains an ISA specification of this ISA and consists of the following files.

- Makefile: Rules for building and running demos/tests.
- demo.isa: An ISA specification of the demo architecture.
- assembly.s: GNU as extension to support the demo instruction set.
- test.S: A simple test program: increments two registers and halts.
- test.prj: A file of iii commands for running a test interactively.
- simulator.c: A C simulator harness for creating compiled simulators.
- config.json: A configuration file used by the C simulator harness.

The ISA specification defines the registers, memory, instruction fetch and instruction execute.  It also implements a simulator API that enable iii to use the specification as a simulator.

Before running the simulator, we need to convert the test program `test.S` to an ELF binary using 'make test.elf'.

    $ cd demo
    $ make test.elf
    clang-16 -std=c2x -E  test.S > test.s
    as test.s -o test.o
    ld test.o -o test.elf
    nm test.elf
    0000000000402000 T __bss_start
    0000000000402000 T _edata
    0000000000402000 T _end
    0000000000401000 T _start

#### Using the demo specification as a simulator

To simulate execution of the test program using iii, load `demo.isa` into iii, load the ELF file `test.elf` and use `:step` to step through the program and the `Print_State` function (defined in `demo.isa`) to observe the processor state at each step.

    $ ISA_PATH=.:.. ../_build/install/default/bin/iii demo.isa
    ISA> :elf test.elf
    Loading ELF file test.elf.
    Entry point = 0x401000
    ISA> Print_State();
    RUNNING PC=64'x401000 R=[ 64'x0 64'x0 64'x0 64'x0 ]
    ISA> :step
    ISA> Print_State();
    RUNNING PC=64'x401001 R=[ 64'x0 64'x1 64'x0 64'x0 ]
    ISA> :step
    ISA> Print_State();
    RUNNING PC=64'x401002 R=[ 64'x0 64'x1 64'x0 64'x1 ]
    ISA> :step
    ISA> Print_State();
    HALTED PC=64'x401003 R=[ 64'x0 64'x1 64'x0 64'x1 ]
    ISA> :quit

From the output of `Print_State` you can see that the program counter `PC`
is incremented after every instruction, that the first instruction incremented `R[1]`, that the second instruction incremented `R[3]` and that the third instruction halted the processor.
This is just about the most exciting program we can run using such a limited instruction set.

iii can also accept commands from a "project file". For example, we could put all of the `:step` and `Print_State();` commands in a file `test.prj` and run the same test like this.

    ISA_PATH=.:.. ../_build/install/default/bin/iii demo.isa --project=test.prj

We often use this with the LLVM project [FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html) tool in our integration tests.

#### Compiling the demo specification

For larger architecture specifications, it can be more effective to compile the specification instead. To compile the specification, we first build a project file containing a sequence of iii commands to compile the specification to C code. There are multiple options for doing this, the "fallback" backend is the most portable.

    ../_build/install/default/bin/iic --basename=sim --backend=fallback > sim.prj

We then load the demo specification into iii and run the project file to generate C code. The configuration file `config.json` is used to specify which ISA functions are called by hand-written C code.

    ISA_PATH=.:.. ../_build/install/default/bin/iii --project=sim.prj --configuration=config.json demo.isa

The generated code is in C files that start with the basename `sim` such as `sim_funs.c`.

To compile and link the C code, we need to use some compiler and linker flags. We can use the `iic` tool to get the right flags for each backend.

    IIC=../_build/install/default/bin/iic
    CFLAGS=`$IIC --backend=fallback --print-c-flags`
    LDFLAGS=`$IIC --backend=fallback --print-ld-flags`

We can now compile and link the `simulator.c` harness. To keep things simple, this file `#includes` the C files generated from the specification. For some choices of backend, you will need to use clang version 16 or later.

    cc $CFLAGS simulator.c -o simulator $LDFLAGS

And, finally, we can run the simulator. In this case, we run it for up to 20 steps or until the processor halts.

    $ ./simulator test.elf --steps=20
    Loading ELF file test.elf.
    Entry point = 0x401000
    Setting PC to 401000
    RUNNING PC=64'x401000 R=[ 64'x0 64'x0 64'x0 64'x0 ]
    RUNNING PC=64'x401001 R=[ 64'x0 64'x1 64'x0 64'x0 ]
    RUNNING PC=64'x401002 R=[ 64'x0 64'x1 64'x0 64'x1 ]
    HALTED PC=64'x401003 R=[ 64'x0 64'x1 64'x0 64'x1 ]

#### Making the demo ISA more realistic

We kept the demo ISA to the bare minimum to make it easier to understand what all the pieces do. To make it more realistic, we invite you to add features such as the following. (For inspiration, you might look at the design of classic 8-bit microprocessors or at RISC processors.)

- A decrement instruction
- Add and subtract instructions
- Load and store instructions
- Conditional branch instructions
- Add a stack and instructions to call a function and return from a function.
- Add an interrupt mechanism


Enjoy!
