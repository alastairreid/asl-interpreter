1.0.0 (2024-12-19)
------------------

* Change ASLi to support most of ASL version 1.0 requiring significant
  changes to the AST, lexer, parser and typechecker.
* Cleanup the "CPU" API that ASLi expects an ISA to implement.
* Add support for compiling ASL specifications to C by adding
  many transformations and multiple runtimes.
* Add a demo ISA to illustrate how to generate simulators from
  an ASL specification.


0.2.0 (2020-05-15)
------------------

* Handle more of Arm's specs (make grammar, etc. more flexible)
* Split libASL out from ASLi to make it easier to reuse
  parts of ASLi in other tools.
* Changed to semantic numbering system (i.e., 3-part format).
* Internal change to using Dune build system.
  This replaces the ocamlfind dependency with a dune dependency.


0.1 (2020-01-01)
----------------

* Added support for loading ELF files and executing binaries.


0.0 (2019-08-30)
----------------

Initial release of ASLi supporting

- loading ASL specifications
- evaluating expressions and statements
