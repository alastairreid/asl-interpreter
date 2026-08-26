#! /bin/bash

set -e

make

_build/install/default/bin/iii --batchmode --exec=':xform_constprop' --exec=':to_mlir --no-typecheck --output-file t1.mlir' riscv/riscv.isa

cat std.mlir t1.mlir > t2.mlir

opts=""
# opts+=" --inline-threshold=100"
# opts+=" --inline"
# opts+=" --int-range-optimizations"
opts+=" --canonicalize"

../llvm-project/build/bin/mlir-opt ${opts} t2.mlir
