/-
  RV32EM.Step: Operational Semantics

  Defines what each instruction DOES to the machine state.
  step : Mach → Option Mach

  ## Execution Model

  Each step:
  1. Fetch: Read 32-bit word at PC from memory
  2. Decode: Convert word to Instr (may fail → none)
  3. Execute: Update registers/memory/PC based on instruction

  Returns `none` if:
  - Decode fails (invalid instruction)
  - Undefined behavior (e.g., division by zero in some models)

  ## Reference

  - Section 2.4: Integer Computational Instructions
  - Section 2.5: Control Transfer Instructions
  - Section 2.6: Load and Store Instructions
  - Section 7.1-7.2: M Extension (multiply/divide)
-/

import RV32EM.Basic
import RV32EM.Instr
import RV32EM.Decode
import RV32EM.Mach

namespace RV32EM

/-!
## Arithmetic Helpers

Signed comparisons and operations need care with BitVec.
-/

/-- Signed less-than comparison for 32-bit values -/
def slt (a b : Word32) : Word32 :=
  if a.toInt < b.toInt then 1 else 0

/-- Unsigned less-than comparison for 32-bit values -/
def sltu (a b : Word32) : Word32 :=
  if a.toNat < b.toNat then 1 else 0

/-- Signed division (rounds toward zero) -/
def sdiv (a b : Word32) : Word32 :=
  if b = 0 then
    -- RISC-V spec: division by zero returns -1 (all ones)
    BitVec.ofInt 32 (-1)
  else if a.toInt = Int.negSucc (2^31 - 1) && b.toInt = -1 then
    -- Overflow case: -2^31 / -1 = -2^31 (wraps)
    a
  else
    BitVec.ofInt 32 (a.toInt / b.toInt)

/-- Unsigned division -/
def udiv (a b : Word32) : Word32 :=
  if b = 0 then
    -- RISC-V spec: division by zero returns 2^32-1 (all ones)
    BitVec.ofNat 32 (2^32 - 1)
  else
    BitVec.ofNat 32 (a.toNat / b.toNat)

/-- Signed remainder -/
def srem (a b : Word32) : Word32 :=
  if b = 0 then
    -- RISC-V spec: remainder with divisor 0 returns dividend
    a
  else if a.toInt = Int.negSucc (2^31 - 1) && b.toInt = -1 then
    -- Overflow case: -2^31 % -1 = 0
    0
  else
    BitVec.ofInt 32 (a.toInt % b.toInt)

/-- Unsigned remainder -/
def urem (a b : Word32) : Word32 :=
  if b = 0 then a
  else BitVec.ofNat 32 (a.toNat % b.toNat)

/-- Multiply and return high 32 bits (signed × signed) -/
def mulh (a b : Word32) : Word32 :=
  let product := a.toInt * b.toInt
  BitVec.ofInt 32 (product >>> 32)

/-- Multiply and return high 32 bits (signed × unsigned) -/
def mulhsu (a b : Word32) : Word32 :=
  let product := a.toInt * b.toNat
  BitVec.ofInt 32 (product >>> 32)

/-- Multiply and return high 32 bits (unsigned × unsigned) -/
def mulhu (a b : Word32) : Word32 :=
  let product := a.toNat * b.toNat
  BitVec.ofNat 32 (product >>> 32)

/-- Shift amount: use only low 5 bits of register value -/
def shiftAmt (v : Word32) : Nat :=
  (v.toNat % 32)

/-!
## Instruction Execution

Pattern: read operands → compute → write result → advance PC
Most instructions: PC := PC + 4
Branches/jumps: PC := computed target
-/

/-- Execute a single instruction.
    Takes current machine state and instruction, returns new state. -/
def execInstr (m : Mach) (i : Instr) : Option Mach :=
  match i with

  -- R-type ALU

  | .ADD rd rs1 rs2 =>
      let result := m.getReg rs1 + m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .SUB rd rs1 rs2 =>
      let result := m.getReg rs1 - m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .AND rd rs1 rs2 =>
      let result := m.getReg rs1 &&& m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .OR rd rs1 rs2 =>
      let result := m.getReg rs1 ||| m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .XOR rd rs1 rs2 =>
      let result := m.getReg rs1 ^^^ m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .SLT rd rs1 rs2 =>
      let result := slt (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .SLTU rd rs1 rs2 =>
      let result := sltu (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .SLL rd rs1 rs2 =>
      let result := m.getReg rs1 <<< shiftAmt (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .SRL rd rs1 rs2 =>
      let result := m.getReg rs1 >>> shiftAmt (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .SRA rd rs1 rs2 =>
      let result := (m.getReg rs1).sshiftRight (shiftAmt (m.getReg rs2))
      some (m.setReg rd result |>.incPC)

  -- I-type ALU

  | .ADDI rd rs1 imm =>
      let result := m.getReg rs1 + signExtend12 imm
      some (m.setReg rd result |>.incPC)

  | .ANDI rd rs1 imm =>
      let result := m.getReg rs1 &&& signExtend12 imm
      some (m.setReg rd result |>.incPC)

  | .ORI rd rs1 imm =>
      let result := m.getReg rs1 ||| signExtend12 imm
      some (m.setReg rd result |>.incPC)

  | .XORI rd rs1 imm =>
      let result := m.getReg rs1 ^^^ signExtend12 imm
      some (m.setReg rd result |>.incPC)

  | .SLTI rd rs1 imm =>
      let result := slt (m.getReg rs1) (signExtend12 imm)
      some (m.setReg rd result |>.incPC)

  | .SLTIU rd rs1 imm =>
      let result := sltu (m.getReg rs1) (signExtend12 imm)
      some (m.setReg rd result |>.incPC)

  -- I-type Shifts

  | .SLLI rd rs1 shamt =>
      let result := m.getReg rs1 <<< shamt.val
      some (m.setReg rd result |>.incPC)

  | .SRLI rd rs1 shamt =>
      let result := m.getReg rs1 >>> shamt.val
      some (m.setReg rd result |>.incPC)

  | .SRAI rd rs1 shamt =>
      let result := (m.getReg rs1).sshiftRight shamt.val
      some (m.setReg rd result |>.incPC)

  -- Loads
  -- Note: Our memory model is word-addressable for simplicity.
  -- A real implementation would need byte-addressable memory for LB/LH.

  | .LW rd rs1 imm =>
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getMem addr
      some (m.setReg rd value |>.incPC)

  | .LH rd rs1 imm =>
      -- Simplified: treat as word load with sign extension of low 16 bits
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getMem addr
      let signedHalf := (value.extractLsb' 0 16).signExtend 32
      some (m.setReg rd signedHalf |>.incPC)

  | .LB rd rs1 imm =>
      -- Simplified: treat as word load with sign extension of low 8 bits
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getMem addr
      let signedByte := (value.extractLsb' 0 8).signExtend 32
      some (m.setReg rd signedByte |>.incPC)

  | .LHU rd rs1 imm =>
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getMem addr
      let unsignedHalf := (value.extractLsb' 0 16).zeroExtend 32
      some (m.setReg rd unsignedHalf |>.incPC)

  | .LBU rd rs1 imm =>
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getMem addr
      let unsignedByte := (value.extractLsb' 0 8).zeroExtend 32
      some (m.setReg rd unsignedByte |>.incPC)

  -- Stores

  | .SW rs1 rs2 imm =>
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getReg rs2
      some (m.setMem addr value |>.incPC)

  | .SH rs1 rs2 imm =>
      -- Simplified: write low 16 bits (word-aligned store)
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getReg rs2
      -- In word-addressable model, we'd need read-modify-write for sub-word
      -- For now, just store the full word (simplification)
      some (m.setMem addr value |>.incPC)

  | .SB rs1 rs2 imm =>
      -- Simplified: same as SH
      let addr := m.getReg rs1 + signExtend12 imm
      let value := m.getReg rs2
      some (m.setMem addr value |>.incPC)

  -- Branches
  -- Branch offset is in imm[12:1], representing multiples of 2.
  -- Effective offset = signExtend(imm) << 1 = signExtend(imm << 1)
  -- But since our Instr stores imm[12:1], we shift left by 1.

  | .BEQ rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1 = v2 then
        -- Branch taken: PC := PC + (sext(imm) << 1)
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  | .BNE rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1 ≠ v2 then
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  | .BLT rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1.toInt < v2.toInt then
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  | .BGE rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1.toInt ≥ v2.toInt then
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  | .BLTU rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1.toNat < v2.toNat then
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  | .BGEU rs1 rs2 imm =>
      let v1 := m.getReg rs1
      let v2 := m.getReg rs2
      if v1.toNat ≥ v2.toNat then
        let offset := (signExtend12 imm) <<< 1
        some (m.setPC (m.pc + offset))
      else
        some m.incPC

  -- U-type

  | .LUI rd imm =>
      -- rd := imm << 12
      let result := (imm.zeroExtend 32) <<< 12
      some (m.setReg rd result |>.incPC)

  | .AUIPC rd imm =>
      -- rd := PC + (imm << 12)
      let result := m.pc + ((imm.zeroExtend 32) <<< 12)
      some (m.setReg rd result |>.incPC)

  -- Jumps

  | .JAL rd imm =>
      -- rd := PC + 4; PC := PC + sext(imm << 1)
      let returnAddr := m.pc + 4
      let offset := (signExtend20 imm) <<< 1
      let target := m.pc + offset
      some (m.setReg rd returnAddr |>.setPC target)

  | .JALR rd rs1 imm =>
      -- rd := PC + 4; PC := (rs1 + sext(imm)) & ~1
      let returnAddr := m.pc + 4
      let target := (m.getReg rs1 + signExtend12 imm) &&& ~~~1
      some (m.setReg rd returnAddr |>.setPC target)

  -- M Extension: Multiply

  | .MUL rd rs1 rs2 =>
      -- rd := (rs1 × rs2)[31:0] (low 32 bits)
      let result := m.getReg rs1 * m.getReg rs2
      some (m.setReg rd result |>.incPC)

  | .MULH rd rs1 rs2 =>
      let result := mulh (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .MULHSU rd rs1 rs2 =>
      let result := mulhsu (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .MULHU rd rs1 rs2 =>
      let result := mulhu (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  -- M Extension: Divide

  | .DIV rd rs1 rs2 =>
      let result := sdiv (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .DIVU rd rs1 rs2 =>
      let result := udiv (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .REM rd rs1 rs2 =>
      let result := srem (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

  | .REMU rd rs1 rs2 =>
      let result := urem (m.getReg rs1) (m.getReg rs2)
      some (m.setReg rd result |>.incPC)

/-!
## Step Functions
-/

/-- Execute one step: fetch → decode → execute -/
def step (m : Mach) : Option Mach := do
  let instrWord := m.getMem m.pc
  let instr ← decode instrWord
  execInstr m instr

/-- Execute up to n steps (stops early if step returns none) -/
def exec (n : Nat) (m : Mach) : Option Mach :=
  match n with
  | 0 => some m
  | n + 1 => do
      let m' ← step m
      exec n m'

/-- Execute until PC reaches a target address or n steps elapse -/
def execUntil (target : Addr) (fuel : Nat) (m : Mach) : Option Mach :=
  if m.pc = target then some m
  else match fuel with
  | 0 => none  -- Out of fuel
  | n + 1 => do
      let m' ← step m
      execUntil target n m'

end RV32EM
