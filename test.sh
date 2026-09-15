#! /bin/bash

set -e

make

iii_opts=""
iii_opts+=" --batchmode"
iii_opts+=" --check-exception-markers"
iii_opts+=" --check-call-markers"

echo Generating MLIR
_build/install/default/bin/iii ${iii_opts} --exec=':xform_constprop' --exec=':to_mlir --typecheck --output-file t1.mlir' riscv/riscv.isa test2.isa

cat std.mlir t1.mlir > t2.mlir

mlir_opts=""
# mlir_opts+=" --inline-threshold=100"
# mlir_opts+=" --inline"
# mlir_opts+=" --int-range-optimizations"
mlir_opts+=" --canonicalize"
mlir_opts+=" --allow-unregistered-dialect"

echo Checking MLIR
../llvm-project/build/bin/mlir-opt ${mlir_opts} t2.mlir
