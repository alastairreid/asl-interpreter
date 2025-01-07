(lang dune 3.11)
(using dune_site 0.1)

(name asli)
(version 1.0.0)

; Copyright Arm Limited (c) 2017-2019
; Copyright (C) 2022-2025 Intel Corporation
; SPDX-Licence-Identifier: BSD-3-Clause

(using menhir 2.0)

(package
  (name asli)
  (synopsis "Interpreter for Architecture Specification Language (ASL)")
  (description "\| Architecture Specification Language (ASL) is an executable language for writing
               "\| clear, precise specifications of Instruction Set Architectures (ISAs).
               "\|
               "\| The ASL interpreter (ASLi) is an implementation of ASL that can execute ASL
               "\| specifications either in an interpreter or by compiling via C code.
               "\|
               "\| Currently supports reading/typechecking the specification,
               "\| interactive execution of ASL statements and expressions,
               "\| executing opcodes one at a time,
               "\| loading ELF files and executing binaries.
               "\|
               "\| Experimentally includes support for generating C code
               "\| that can be used to execute binaries faster and can be
               "\| linked with other C/C++ libraries.
  )
  (sites (lib plugins)
         (lib runtime)
         (lib runtime_include)
         (share stdlib))
  (depends
    ("alcotest"  :with-test)
    "dune-site"
    "linenoise"
    ("menhir"    :build)
    ("ocaml"     (>= "4.14"))
    ("ocolor"    (>= "1.2.2"))
    "odoc"
    "yojson"
    ("z3"        (>= "4.8.7"))
    "zarith"
  )
)

(license BSD-3-Clause)
(authors "Alastair Reid")
(maintainers "Alastair Reid <alastair.reid@intel.com>")
(source (github IntelLabs/asl-interpreter))
(bug_reports "https://github.com/IntelLabs/asl-interpreter/issues")
(homepage "https://github.com/IntelLabs/asl-interpreter")
; (documentation ...)

(generate_opam_files true)
