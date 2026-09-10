#! /bin/bash

set -e

make

_build/install/default/bin/iii --batchmode --exec=':xform_constprop' --exec=':to_mlir --no-typecheck --output-file t1.mlir' riscv/riscv.isa test2.isa

cat std.mlir t1.mlir > t2.mlir

opts=""
# opts+=" --inline-threshold=100"
# opts+=" --inline"
# opts+=" --int-range-optimizations"
opts+=" --canonicalize"
opts+=" --allow-unregistered-dialect"

../llvm-project/build/bin/mlir-opt ${opts} t2.mlir
