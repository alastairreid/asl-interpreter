#! /bin/bash

set -e

make
_build/install/default/bin/iii --batchmode --exec=':xform_constprop' --exec=':to_mlir --no-typecheck --output-file t1.mlir' riscv/riscv.isa
cat std.mlir t1.mlir > t2.mlir
../llvm-project/build/bin/mlir-opt --inline-threshold=100 --inline --int-range-optimizations --canonicalize t2.mlir
