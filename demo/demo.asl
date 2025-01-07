/////////////////////////////////////////////////////////////////
// A simple 64-bit processor spec for use as a demo.
// (This is not intended to match any actual processor)
//
// Copyright (C) 2023-2025 Intel Corporation
/////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////
// Reset and Step
/////////////////////////////////////////////////////////////////

func Reset()
begin
    halted = FALSE;
    PC = 64'x0;
    for i = 0 to 3 do
        R[i] = 64'x0;
    end
    RAMInit();
end

// The 'Step' function takes a single atomic step
// such as executing an instruction
func Step()
begin
    if halted then
        return;
    end

    let opcode = RAM[PC, 1];

    case opcode of
        when '0001 00xx' => // INC r
            let r = UInt(opcode[1:0]);
            R[r] = R[r] + 1;

        otherwise =>
            // Unrecognized instructions halt the processor
            halted = TRUE;
    end

    PC = PC + 1;
end

/////////////////////////////////////////////////////////////////
// Registers
/////////////////////////////////////////////////////////////////

// Has the processor halted?
var halted : boolean;

// Program counter
var PC : bits(64);

// 4 entry 64-bit register file
var R : array [4] of bits(64);

/////////////////////////////////////////////////////////////////
// 64-bit byte-addressable memory
/////////////////////////////////////////////////////////////////

var __Memory : __RAM(64);

func RAMInit()
begin
    asl_ram_init(64, __Memory, 64'xdead_beef_dead_beef);
end

getter RAM[address : bits(64), size_in_bytes : integer] => bits(8 * size_in_bytes)
begin
    return asl_ram_read(64, size_in_bytes, __Memory, address);
end

setter RAM[address : bits(64), size_in_bytes : integer] = value : bits(8 * size_in_bytes)
begin
    asl_ram_write(64, size_in_bytes, __Memory, address, value);
end

/////////////////////////////////////////////////////////////////
// Debugging support: print processor state
/////////////////////////////////////////////////////////////////

func PrintState()
begin
    if halted then
        print("HALTED ");
    else
        print("RUNNING ");
    end
    print("PC="); print_bits_hex(PC);
    print(" R=[");
    for i = 0 to 3 do
        print(" "); print_bits_hex(R[i]);
    end
    print(" ]");
    println();
end

/////////////////////////////////////////////////////////////////
// Simulator interface
/////////////////////////////////////////////////////////////////

func ASL_Reset()
begin
    Reset();
end

func ASL_Step()
begin
    Step();
end

func ASL_IsHalted() => boolean
begin
    return halted;
end

func ASL_GetPC() => integer
begin
    return UInt(PC);
end

func ASL_SetPC(x : bits(64))
begin
    PC = x;
end

// Used when loading ELF files
func ASL_WriteMemory8(address : bits(64), x : bits(8))
begin
    RAM[address, 1] = x;
end

func ASL_ReadReg64(index : integer) => bits(64)
begin
    case index of
        when 0 => return PC;
        when 1 => return (if halted then 64'x1 else 64'x0);
        when 10 => return R[0];
        when 11 => return R[1];
        when 12 => return R[2];
        when 13 => return R[3];
        otherwise => UnimplementedFeature("ASL_ReadReg64"); return Zeros(64);
    end
end

func ASL_WriteReg64(index : integer, val : bits(64))
begin
    case index of
        when 0 => PC = val;
        when 1 => halted = val[0] == '1';
        when 10 => R[0] = val;
        when 11 => R[1] = val;
        when 12 => R[2] = val;
        when 13 => R[3] = val;
        otherwise => UnimplementedFeature("ASL_WriteReg64");
    end
end

// If this function is called, it indicates that there is an gap in the specification
// caused by a missing feature.
//
// In a complete specification, there should be no calls to this function.
func UnimplementedFeature(name : string)
begin
    print("Unimplemented feature (");
    print(name);
    print("): Report a bug in the specification");
    println();
    assert FALSE;
end

/////////////////////////////////////////////////////////////////
// End
/////////////////////////////////////////////////////////////////
