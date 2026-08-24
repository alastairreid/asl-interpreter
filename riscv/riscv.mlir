/////////////////////////////////////////////////////////////////
// Copyright (C) 2022-2026 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
//
// A manual conversion to MLIR of
// A subset of the RISC-V RV32I ISA written in .isa
//
// This subset is intended to support small scale experimentation
// and research and to act as a slightly larger example than the
// 'demo' architecture included in the ISA-tools repository
// (https://github.com/IntelLabs/isa-tools).
//
// This sample code is unsupported and incompletely tested
// and may contain bugs.
// We do not intend to extend this to support the full RISC-V ISA.
//
// For an accurate, supported specification of RISC-V, we strongly
// recommend use of the official RISC-V specification
// (https://github.com/riscv/sail-riscv) and the
// Sail toolchain (https://github.com/rems-project/sail).
/////////////////////////////////////////////////////////////////


// To run xdsl-opt on this, use:
//
//     (cd ../xdsl; uv run xdsl-opt ../isa-tools/riscv/riscv.mlir)

!size = i32

// type Std::Bits(n : {0..}) = {0..2**n};
!Std$Bits = i256 // should be tuple<!size, !bigint.bigint>

module {

    //////////////////////////////////////////////////////////////// 
    // Standard library builtin functions
    //////////////////////////////////////////////////////////////// 

    // todo: should size be an explicit parameter?
    func.func @Std$Bits$Eq(%a : !Std$Bits, %b : !Std$Bits) -> i1
    func.func @Std$Bits$Ne(%a : !Std$Bits, %b : !Std$Bits) -> i1 // optionally omit this?

    func.func @Std$Bits$And(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Or(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Xor(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Not(%a : !Std$Bits) -> !Std$Bits

    func.func @Std$Bits$Add(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Subtract(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Multiply(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Divide(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits
    func.func @Std$Bits$Negate(%a : !Std$Bits) -> !Std$Bits

    func.func @Std$Bits$Zero(%size : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$All_Ones(%size : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Zero_Extend(%a : !Std$Bits, %size : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Sign_Extend(%a : !Std$Bits, %size : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Slice(%a : !Std$Bits, %index : !bigint.bigint, %size : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Concat(%a : !Std$Bits, %b : !Std$Bits) -> !Std$Bits

    func.func @Std$Bits$Shift_Left_Logical(%a : !Std$Bits, %amount : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Shift_Right_Logical(%a : !Std$Bits, %amount : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Shift_Right_Arithmetic(%a : !Std$Bits, %amount : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Power_Of_2(%amount : !bigint.bigint) -> !Std$Bits

    func.func @Std$Bits$Length(%a : !Std$Bits) -> !bigint.bigint
    func.func @Std$Bits$Extract(%a : !bigint.bigint) -> !Std$Bits
    func.func @Std$Bits$Unsigned(%a : !Std$Bits) -> !bigint.bigint
    func.func @Std$Bits$Signed(%a : !Std$Bits) -> !bigint.bigint

    // necessary to be able to use scf_for, array indexes, etc.
    func.func @Std$Integer$From_s64(%a : i64) -> !bigint.bigint
    func.func @Std$Integer$From_u64(%a : i64) -> !bigint.bigint
    func.func @Std$Integer$To_i64(%a : !bigint.bigint) -> i64

    //////////////////////////////////////////////////////////////// 
    // Standard library defined functions
    //////////////////////////////////////////////////////////////// 

    // todo: add these 

    //////////////////////////////////////////////////////////////// 
    // User defined global variables and constants
    //////////////////////////////////////////////////////////////// 

    // memref.global constant @XLEN : memref<!bigint.bigint> = bigint.constant 32 : !bigint.bigint
    // todo: should be a constant with initial value of 32
    "memref.global"() {"sym_name" = "XLEN", "type" = memref<!bigint.bigint>, "sym_visibility" = "public", "initial_value" } : () -> ()

    // memref.global "private" @Halted : memref<i1> = uninitialized
    "memref.global"() {"sym_name" = "Halted", "type" = memref<i1>, "sym_visibility" = "public", "initial_value" } : () -> ()

    // memref.global "private" @PC : memref<!bigint.bigint> = uninitialized
    "memref.global"() {"sym_name" = "PC", "type" = memref<!Std$Bits>, "sym_visibility" = "public", "initial_value" } : () -> ()

    // memref.global "private" @X : memref<!Std$Bits x 32> = uninitialized
    // "memref.global"() {"sym_name" = "X", "type" = memref<32x!Std$Bits>, "sym_visibility" = "public", "initial_value" } : () -> ()

    //////////////////////////////////////////////////////////////// 
    // User defined functions
    //////////////////////////////////////////////////////////////// 

    func.func @RAM$Init()

    func.func @RISCV$Reset() -> () {
        // Halted := False;
        %false = arith.constant 0 : i1
        %ref_Halted = memref.get_global @Halted : memref<i1>
        memref.store %false, %ref_Halted[] : memref<i1>

        // PC := Std::Bits::Zero(XLEN);
        %ref_XLEN = memref.get_global @XLEN : memref<!bigint.bigint>
        %0 = memref.load %ref_XLEN[] : memref<!bigint.bigint>
        %1 = func.call @Std$Bits$Zero(%0) : (!bigint.bigint) -> !Std$Bits
        %ref_PC = memref.get_global @PC : memref<!Std$Bits>
        memref.store %1, %ref_PC[] : memref<!Std$Bits>

        // for i := 1 to 31 do
        %one_i64 = arith.constant 1 : i64
        %one = func.call @Std$Integer$From_u64(%one_i64) : (i64) -> !bigint.bigint

        %thirty_one_i64 = arith.constant 31 : i64
        %thirty_one = func.call @Std$Integer$From_u64(%thirty_one_i64) : (i64) -> !bigint.bigint

        // scf.for %i = %one to %thirty_one step %one : !bigint.bigint {
        scf.for %i = %one_i64 to %thirty_one_i64 step %one_i64 : i64 {

        //     X[i] := Std::Bits::Zero(XLEN);
            %3 = func.call @Std$Bits$Zero(%0) : (!bigint.bigint) -> !Std$Bits
            %ref_X = memref.get_global @X : memref<32x!Std$Bits>
            %index_i = "arith.index_cast"(%i) : (i64) -> index
            memref.store %1, %ref_X[%index_i] : memref<32x!Std$Bits>

        // endfor;
        }

        // RAM::Init();
        func.call @RAM$Init() : () -> ()

        // Implicit return at end of function
        func.return
    }

    //////////////////////////////////////////////////////////////// 
    // User defined instruction functions
    //////////////////////////////////////////////////////////////// 

    // function Instr::LUI(imm20 : Bits(20), rd : Bits(5))
    func.func @RISCV$Instr$LUI(%imm20 : !Std$Bits, %rd : !Std$Bits)
    // begin
    {
        // Implicit assertions to check the type of the arguments
        // Note: these assertions at the top of a function would be better treated as assumptions
        // because we assume  that every call to this function asserts
        // that the arguments are the right size
            // Implicit assert Std::Bits::Length(%imm20) == 20
            %0 = func.call @Std$Bits$Length(%imm20) : (!Std$Bits) -> !bigint.bigint
            %twenty_i64 = arith.constant 20 : i64
            %twenty = func.call @Std$Integer$From_u64(%twenty_i64) : (i64) -> !bigint.bigint
            %1 = bigint.eq %0, %twenty : i1
            cf.assert %1, "Length(imm20) == 20"

            // Implicit assert Std::Bits::Length(%rd) == 5
            %2 = func.call @Std$Bits$Length(%rd) : (!Std$Bits) -> !bigint.bigint
            %five_i64 = arith.constant 5 : i64
            %five = func.call @Std$Integer$From_u64(%five_i64) : (i64) -> !bigint.bigint
            %3 = bigint.eq %2, %five : i1
            cf.assert %1, "Length(rd) == 5"

        // let d := Std::Bits::Unsigned(rd);
            // Implicit argument type check: assert Std::Bits::Length(%rd) >= 0
            %4 = func.call @Std$Bits$Length(%rd) : (!Std$Bits) -> !bigint.bigint
            %zero_i64 = arith.constant 0 : i64
            %zero = func.call @Std$Integer$From_u64(%zero_i64) : (i64) -> !bigint.bigint
            %5 = bigint.gte %4, %zero : i1
            cf.assert %1, "Length(arg0) > 05"

            %d = func.call @Std$Bits$Unsigned(%rd) : (!Std$Bits) -> !bigint.bigint

        // X[d] := Std::Bits::Sign_Extend(imm20 ++ Zero(12), XLEN);
            %twelve_i64 = arith.constant 12 : i64
            %twelve = func.call @Std$Integer$From_u64(%twelve_i64) : (i64) -> !bigint.bigint
            %z12 = func.call @Std$Bits$Zero(%twelve) : (!bigint.bigint) -> !Std$Bits
            %arg0 = func.call @Std$Bits$Concat(%imm20, %z12) : (!Std$Bits, !Std$Bits) -> !Std$Bits

            %ref_XLEN = memref.get_global @XLEN : memref<!bigint.bigint>
            %xlen = memref.load %ref_XLEN[] : memref<!bigint.bigint>

            // Implicit argument type check: assert Std::Bits::Length(%arg0) == 32
            // Note: this assertion is completely redundant because it follows from
            // the assertion that Length(imm20) == 20 and properties of Std::Bits::Zero and Std::Bits::Concat.
            // So we should be able to remove this redundant assertion
            %len_arg0 = func.call @Std$Bits$Length(%rd) : (!Std$Bits) -> !bigint.bigint
            %thirty_two_i64 = arith.constant 32 : i64
            %thirty_two = func.call @Std$Integer$From_u64(%thirty_two_i64) : (i64) -> !bigint.bigint
            %assert_arg0 = bigint.eq %len_arg0, %thirty_two : i1
            cf.assert %assert_arg0, "Length(arg0) == 32"
            
            %assert_arg1 = bigint.gte %xlen, %thirty_two : i1
            cf.assert %assert_arg1, "xlen >= 32"

            %result = func.call @Std$Bits$Sign_Extend(%arg0, %xlen) : (!Std$Bits, !bigint.bigint) -> !Std$Bits

            // Implicit result type check: assume Std::Bits::Length(%result) == %xlen
            // Note that, like function arguments, this should really be an assume, not an assert
            // since the function definition should end with a corresponding assertion about the result type
            %len_result1 = func.call @Std$Bits$Length(%result) : (!Std$Bits) -> !bigint.bigint
            %assert_result1 = bigint.eq %len_result1, %xlen : i1
            cf.assert %assert_result1, "Length(result) == XLEN"

            %ref_X = memref.get_global @X : memref<32x!Std$Bits>

            // Implicit argument type check: assert Std::Bits::Length(%result) == %xlen
            // Note: this assertion is completely redundant because exactly the same property
            // was checked in %assert_result1. So we should be able to remove this redundant
            // assertion
            %len_result = func.call @Std$Bits$Length(%result) : (!Std$Bits) -> !bigint.bigint
            %assert_result = bigint.eq %len_result, %xlen : i1
            cf.assert %assert_result, "Length(result) == XLEN"

            %i64_d = func.call @Std$Integer$To_i64(%d) : (!bigint.bigint) -> i64
            %index_d = "arith.index_cast"(%i64_d) : (i64) -> index
            memref.store %result, %ref_X[%index_d] : memref<32x!Std$Bits>

        // Implicit return at end of function
        func.return
    // end
    }

}
