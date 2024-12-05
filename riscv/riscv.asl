/////////////////////////////////////////////////////////////////
// A subset of the RISC-V RV32I ISA written in ASL
/////////////////////////////////////////////////////////////////

// XLEN is a constant in RV32I
constant XLEN : integer = 32;

/////////////////////////////////////////////////////////////////
// Reset and Step
/////////////////////////////////////////////////////////////////

func Reset()
begin
    halted = FALSE;
    PC = Zeros(XLEN);
    for i = 1 to 31 do
        X[i] = Zeros(XLEN);
    end
    RAMInit();
end

// The 'Step' function takes a single atomic step
// such as executing an instruction
//
// In a full ISA specification, it also handles
// breakpoints, single-stepping, exceptions, etc.
func Step?()
begin
    if halted then
        return;
    end

    let encoding = RAM[PC, 4];
    _NextPC = PC + 4;
    DecodeAndExecute32?(encoding);

    PC = _NextPC;
end

type ExceptionTaken of exception;

/////////////////////////////////////////////////////////////////
// Registers
/////////////////////////////////////////////////////////////////

// Value that PC will be set to after successful completion of
// the current instruction.
// This is usually PC + <instruction length> but it can
// be set to some other value by branches, exceptions, etc.
var _NextPC : bits(XLEN);

// Register file
// (Note that this is only accessed via the X accessor functions
// and that element 0 is never read or written)
var _X : array [32] of bits(XLEN);

// Has the processor halted?
var halted : boolean;

// Program counter
// Note that the bottom bit is ignored by the accessor function
var _PC : bits(XLEN);

/////////////////////////////////////////////////////////////////
// Accessor functions
/////////////////////////////////////////////////////////////////

// Note that X[0] always returns zero
// (Design detail: this could also be specified by initializing _X[0]
// to zero and never modifying it. Special casing it on reads makes
// it easier for readers and analysis tools to recognize that X[0] = 0
// without having to keep track of system invariants.)
getter X[i : integer {0..31}] => bits(XLEN)
begin
    if i == 0 then
        return Zeros(XLEN);
    else
        return _X[i];
    end
end

setter X[i : integer {0..31}] = val : bits(XLEN)
begin
    if i != 0 then
        _X[i] = val;
    end
end

getter PC => bits(XLEN)
begin
    return [_PC[XLEN-1 : 1], '0'];
end

setter PC = target : bits(XLEN)
begin
    _PC = [target[XLEN-1 : 1], '0'];
end

func BranchTo(target : bits(XLEN))
begin
    _NextPC = [target[XLEN-1 : 1], '0'];
end

/////////////////////////////////////////////////////////////////
// Instruction decode (for 32-bit encoding)
/////////////////////////////////////////////////////////////////

func DecodeAndExecute32?(encoding : bits(32))
begin
    // Note: this case statement would normally be machine
    // generated from an instruction encoding database.
    // Note too that the immediate field is reverse permutation
    // from that normally shown in documents
    // Note: this code is based on the table in chapter 34 
    // "RV32/64G Instruction Set Listings" of the RVI
    // Instruction Set Manual volume 1. (page 554)
    case encoding of
        when 'xxxx xxxx xxxx xxxx xxxx xxxx x011 0111' => Instr_LUI  (encoding[31:12], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx xxxx xxxx x001 0111' => Instr_AUIPC(encoding[31:12], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx xxxx xxxx x110 1111' => Instr_JAL  (encoding[31,19:12,20,30:21], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx xxxx xxxx x110 0111' => Instr_JALR (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x000 xxxx x110 0011' => Instr_BEQ  (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x001 xxxx x110 0011' => Instr_BNE  (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x100 xxxx x110 0011' => Instr_BLT  (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x101 xxxx x110 0011' => Instr_BGE  (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x110 xxxx x110 0011' => Instr_BLTU (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x111 xxxx x110 0011' => Instr_BGEU (encoding[31,7,30:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x000 xxxx x000 0011' => Instr_LB   (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x001 xxxx x000 0011' => Instr_LH   (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x010 xxxx x000 0011' => Instr_LW   (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x100 xxxx x000 0011' => Instr_LBU  (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x101 xxxx x000 0011' => Instr_LHU  (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x000 xxxx x010 0011' => Instr_SB   (encoding[31:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x001 xxxx x010 0011' => Instr_SH   (encoding[31:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x010 xxxx x010 0011' => Instr_SW   (encoding[31:25,11:7], encoding[24:20], encoding[19:15]);
        when 'xxxx xxxx xxxx xxxx x000 xxxx x001 0011' => Instr_ADDI (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x010 xxxx x001 0011' => Instr_SLTI (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x011 xxxx x001 0011' => Instr_SLTIU(encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x100 xxxx x001 0011' => Instr_XORI (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x110 xxxx x001 0011' => Instr_ORI  (encoding[31:20], encoding[19:15], encoding[11:7]);
        when 'xxxx xxxx xxxx xxxx x111 xxxx x001 0011' => Instr_ANDI (encoding[31:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x001 xxxx x001 0011' => Instr_SLLI (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x101 xxxx x001 0011' => Instr_SRLI (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0100 000x xxxx xxxx x101 xxxx x001 0011' => Instr_SRAI (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_ADD  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0100 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SUB  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SLL  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SLT  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SLTU (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_XOR  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SRL  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0100 000x xxxx xxxx x000 xxxx x011 0011' => Instr_SRA  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_OR   (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '0000 000x xxxx xxxx x000 xxxx x011 0011' => Instr_AND  (encoding[24:20], encoding[19:15], encoding[11:7]);
        when '1000 0011 0011 0000 0000 0000 0000 1111' => Instr_FENCETSO!();
        when '0000 0001 0000 0000 0000 0000 0000 1111' => Instr_PAUSE!();
        when '0000 0000 0000 0000 0000 0000 0111 0011' => Instr_ECALL!();
        when '0000 0000 0001 0000 0000 0000 0111 0011' => Instr_EBREAK!();
        otherwise => ReportUndefined!();
    end
end

////////////////////////////////////////////////////////////////
// Instruction specifications
//
// The function arguments are based on the table in chapter 34
// of the RVI ISM page 554
////////////////////////////////////////////////////////////////

func Instr_LUI(imm20 : bits(20), rd : bits(5))
begin
    let d = UInt(rd);
    X[d] = SignExtend([imm20, Zeros(12)], XLEN);
end

func Instr_AUIPC(imm20 : bits(20), rd : bits(5))
begin
    let d = UInt(rd);
    X[d] = PC + SignExtend([imm20, Zeros(12)], XLEN);
end

func Instr_JAL(imm20 : bits(20), rd : bits(5))
begin
    let d = UInt(rd);
    let offset = SignExtend([imm20, '0'], XLEN);
    X[d] = _NextPC;
    BranchTo(PC + offset);
end

func Instr_JALR(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let offset = SignExtend(imm12, XLEN);
    let target = X[s1] + offset;
    X[d] = _NextPC;
    BranchTo(target);
end

func Instr_BEQ(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if X[s1] == X[s2] then
        BranchTo(target);
    end
end

func Instr_BNE(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if X[s1] == X[s2] then
        BranchTo(target);
    end
end

func Instr_BLT(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if SInt(X[s1]) < SInt(X[s2]) then
        BranchTo(target);
    end
end

func Instr_BGE(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if SInt(X[s1]) >= SInt(X[s2]) then
        BranchTo(target);
    end
end

func Instr_BLTU(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if UInt(X[s1]) == UInt(X[s2]) then
        BranchTo(target);
    end
end

func Instr_BGEU(imm13 : bits(13), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let offset = SignExtend([imm13, '0'], XLEN);
    let target = PC + offset;
    if UInt(X[s1]) >= UInt(X[s2]) then
        BranchTo(target);
    end
end

func Instr_LB(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = MemRead(address, 1);
    X[d] = ZeroExtend(value, XLEN);
end

func Instr_LH(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = MemRead(address, 4);
    X[d] = SignExtend(value, XLEN);
end

func Instr_LW(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = MemRead(address, 4);
    X[d] = SignExtend(value, XLEN);
end

func Instr_LBU(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = MemRead(address, 1);
    X[d] = ZeroExtend(value, XLEN);
end

func Instr_LHU(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = MemRead(address, 2);
    X[d] = ZeroExtend(value, XLEN);
end

func Instr_SB(imm12 : bits(12), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = X[s2][0 +: 8];
    MemWrite(address, 1, value);
end

func Instr_SH(imm12 : bits(12), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = X[s2][0 +: 16];
    MemWrite(address, 2, value);
end

func Instr_SW(imm12 : bits(12), rs2 : bits(5), rs1 : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let address = X[s1] + SignExtend(imm12, XLEN);
    let value = X[s2][0 +: 32];
    MemWrite(address, 4, value);
end

func Instr_ADDI(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = operand1 + operand2;
    X[d] = result;
end

func Instr_SLTI(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = if SInt(operand1) < SInt(operand2) then '1' else '0';
    X[d] = ZeroExtend(result, XLEN);
end

func Instr_SLTIU(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = if UInt(operand1) < UInt(operand2) then '1' else '0';
    X[d] = ZeroExtend(result, XLEN);
end

func Instr_XORI(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = operand1 XOR operand2;
    X[d] = result;
end

func Instr_ORI(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = operand1 OR operand2;
    X[d] = result;
end

func Instr_ANDI(imm12 : bits(12), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = SignExtend(imm12, XLEN);
    let result = operand1 AND operand2;
    X[d] = result;
end

func Instr_SLLI(shamt : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let distance = UInt(shamt);
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand = X[s1];
    let result = ShiftLeft(operand, distance);
    X[d] = result;
end

func Instr_SRLI(shamt : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let distance = UInt(shamt);
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand = X[s1];
    let result = ShiftRightLogical(operand, distance);
    X[d] = result;
end

func Instr_SRAI(shamt : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let distance = UInt(shamt);
    let s1 = UInt(rs1);
    let d = UInt(rd);
    let operand = X[s1];
    let result = ShiftRightArithmetic(operand, distance);
    X[d] = result;
end

func Instr_ADD(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = operand1 + operand2;
    X[d] = result;
end

func Instr_SUB(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = operand1 - operand2;
    X[d] = result;
end

func Instr_SLL(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let distance = UInt(operand2[0 +: 5]);
    let result = ShiftLeft(operand1, distance);
    X[d] = result;
end

func Instr_SRL(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let distance = UInt(operand2[0 +: 5]);
    let result = ShiftRightLogical(operand1, distance);
    X[d] = result;
end

func Instr_SRA(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let distance = UInt(operand2[0 +: 5]);
    let result = ShiftRightArithmetic(operand1, distance);
    X[d] = result;
end

func Instr_SLT(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = if SInt(operand1) < SInt(operand2) then '1' else '0';
    X[d] = ZeroExtend(result, XLEN);
end


func Instr_SLTU(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = if UInt(operand1) < UInt(operand2) then '1' else '0';
    X[d] = ZeroExtend(result, XLEN);
end

func Instr_XOR(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = operand1 XOR operand2;
    X[d] = result;
end

func Instr_OR(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = operand1 OR operand2;
    X[d] = result;
end

func Instr_AND(rs2 : bits(5), rs1 : bits(5), rd : bits(5))
begin
    let s1 = UInt(rs1);
    let s2 = UInt(rs2);
    let d = UInt(rd);
    let operand1 = X[s1];
    let operand2 = X[s2];
    let result = operand1 AND operand2;
    X[d] = result;
end

func Instr_FENCETSO!()
begin
    MemoryFenceTSO!();
end

func Instr_PAUSE!()
begin
    PauseExecution!();
end

func Instr_ECALL!()
begin
    EnvironmentCall!();
end

func Instr_EBREAK!()
begin
    EnvironmentBreak!();
end

/////////////////////////////////////////////////////////////////
// Helper functions
/////////////////////////////////////////////////////////////////

func ReportUndefined!()
begin
    UnimplementedFeature("ReportUndefined");
    throw ExceptionTaken;
end

func MemoryFenceTSO!()
begin
    UnimplementedFeature("MemoryFenceTSO");
    throw ExceptionTaken;
end

func PauseExecution!()
begin
    UnimplementedFeature("PauseExecution");
    throw ExceptionTaken;
end

func EnvironmentCall!()
begin
    UnimplementedFeature("EnvironmentCall");
    throw ExceptionTaken;
end

func EnvironmentBreak!()
begin
    UnimplementedFeature("EnvironmentBreak");
    throw ExceptionTaken;
end

/////////////////////////////////////////////////////////////////
// Memory subsystem
/////////////////////////////////////////////////////////////////

func MemRead{size_in_bytes}(address : bits(XLEN), size_in_bytes : integer {1, 2, 4}) => bits(8 * size_in_bytes)
begin
    // todo: consider checking alignment
    return RAM[address, size_in_bytes];
end

func MemWrite{size_in_bytes}(address : bits(XLEN), size_in_bytes : integer {1, 2, 4}, value : bits(8 * size_in_bytes))
begin
    // todo: consider checking alignment
    RAM[address, size_in_bytes] = value;
end

/////////////////////////////////////////////////////////////////
// Byte-addressable memory
//
// Note that the ASL primitives support unaligned access. If the
// ISA does not support that, then explicit checks should be added.
/////////////////////////////////////////////////////////////////

var __Memory : __RAM(XLEN);

func RAMInit()
begin
    asl_ram_init(XLEN, __Memory, 64'xdead_beef_dead_beef);
end

getter RAM[address : bits(XLEN), size_in_bytes : integer] => bits(8 * size_in_bytes)
begin
    return asl_ram_read(XLEN, size_in_bytes, __Memory, address);
end

setter RAM[address : bits(XLEN), size_in_bytes : integer] = value : bits(8 * size_in_bytes)
begin
    asl_ram_write(XLEN, size_in_bytes, __Memory, address, value);
end

/////////////////////////////////////////////////////////////////
// Debugging support: print processor state
/////////////////////////////////////////////////////////////////

func PrintState()
begin
    print("PC="); print_bits_hex(PC);
    if halted then
        print("HALTED ");
    else
        print("RUNNING ");
    end
    println();
    for i = 0 to 31 do
        print(" X[");
        print_int_dec(i);
        print("] = ");
        print_bits_hex(X[i]);
        if i MOD 8 == 0 then
            println();
        end
    end
end

/////////////////////////////////////////////////////////////////
// Simulator interface
// This is the standard interface to the ASLi tool.
// Note that the bitwidths are dictated by the ASLi interface,
// not by the ISA.
// In particular, ASL_GetPC/SetPC and ASL_Read/WriteReg64
// operate on 64-bit values.
/////////////////////////////////////////////////////////////////

func ASL_Reset()
begin
    Reset();
end

func ASL_Step?()
begin
    Step?();
end

func ASL_IsHalted() => boolean
begin
    return halted;
end

func ASL_GetPC() => bits(64)
begin
    return ZeroExtend(PC, 64);
end

func ASL_SetPC(x : bits(64))
begin
    PC = x[0 +: XLEN];
end

// Used when loading ELF files
func ASL_WriteMemory8(address : bits(64), x : bits(8))
begin
    RAM[address[0 +: XLEN], 1] = x;
end

// Read a register identified by an index.
// These indexes are arbitrary but must match the table
// used by the simulator.
// (In full ISA specifications, some or all of this code would be machine
// generated.)
func ASL_ReadReg64(index : integer {0 .. 2^15-1}) => bits(64)
begin
    case index of
        when 0 => return ZeroExtend(PC, 64);
        when 1 => return (if halted then 64'x1 else 64'x0);
        when {0x10 .. 0x2f} => return ZeroExtend(X[index - 0x10], 64);
        otherwise => UnimplementedFeature("ASL_ReadReg64"); return Zeros(64);
    end
end

// Write a register identified by an index.
// These indexes are arbitrary but must match the table
// used by the simulator.
// (In full ISA specifications, some or all of this code would be machine
// generated.)
func ASL_WriteReg64(index : integer, val : bits(64))
begin
    case index of
        when 0 => PC = val[0 +: XLEN];
        when 1 => halted = val[0] == '1';
        when {0x10 .. 0x2f} => X[index - 0x10] = val[0 +: XLEN];
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
    asl_end_execution(FALSE);
end

/////////////////////////////////////////////////////////////////
// End
/////////////////////////////////////////////////////////////////
