(lang dune 3.13)

(name isa-tools)
(version 2.0.0)

; Copyright Arm Limited (c) 2017-2019
; Copyright (C) 2022-2026 Intel Corporation
; SPDX-License-Identifier: BSD-3-Clause

(using menhir 3.0)

(package
  (name isa-tools)
  (synopsis "Interpreter for Intel(R) ISA Specification Language")
  (description "\| Intel(R) ISA Specification Language is an executable language for writing
               "\| clear, precise specifications of Instruction Set Architectures (ISAs).
               "\|
               "\| The Intel(R) ISA interpreter (iii) is an implementation of the language
               "\| that can execute .isa specifications either in an interpreter or by
               "\| compiling via C code.
               "\|
               "\| Currently supports reading/typechecking the specification,
               "\| interactive execution of .isa statements and expressions,
               "\| executing opcodes one at a time,
               "\| loading ELF files and executing binaries.
               "\|
               "\| Experimentally includes support for generating C code
               "\| that can be used to execute binaries faster and can be
               "\| linked with other C/C++ libraries.
  )
  (depends
    ("alcotest"  :with-test)
    "linenoise"
    ("menhir"    :build)
    ("ocaml"     (>= "5.4"))
    ("ocolor"    (>= "1.2.2"))
    "odoc"
    "yojson"
    ("z3"        (>= "4.13.0"))
    "zarith"
  )
)

(license BSD-3-Clause)
(authors "Alastair Reid")
(maintainers "Alastair Reid <alastair.reid@intel.com>")
(source (github IntelLabs/isa-tools))
(bug_reports "https://github.com/IntelLabs/isa-tools/issues")
(homepage "https://github.com/IntelLabs/isa-tools")
; (documentation ...)

(generate_opam_files true)
