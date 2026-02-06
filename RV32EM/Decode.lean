/-
  RV32EM.Decode: Word32 → Option Instr

  Converts 32-bit machine code to abstract instruction representation.
  Returns `none` for invalid or unrecognized bit patterns.

  ## Decoding Strategy

  1. Extract opcode (bits [6:0]) to determine instruction class
  2. Based on opcode, determine the format (R/I/S/B/U/J)
  3. Extract funct3 (bits [14:12]) and funct7 (bits [31:25]) as needed
  4. Extract register fields and reconstruct immediates
  5. Return the appropriate Instr constructor

  ## Immediate Reconstruction

  Immediates are scattered across the instruction word. Decoding reverses
  the scrambling done in Encode.lean:

  ```
  I-type: imm[11:0] ← bits[31:20]                    (straightforward)
  S-type: imm[11:5] ← bits[31:25], imm[4:0] ← bits[11:7]
  B-type: imm[12]   ← bit 31,     imm[10:5] ← bits[30:25],
          imm[4:1]  ← bits[11:8], imm[11]   ← bit 7
  U-type: imm[31:12] ← bits[31:12]                   (straightforward)
  J-type: imm[20]    ← bit 31,     imm[10:1] ← bits[30:21],
          imm[11]    ← bit 20,     imm[19:12] ← bits[19:12]
  ```

  ## Reference

  - Section 2.2: Base Instruction Formats
  - Section 2.3: Immediate Encoding Variants
-/

import RV32EM.Basic
import RV32EM.Instr
import RV32EM.Encode  -- For opcode constants

namespace RV32EM

/-!
## Bit Field Extraction

All RISC-V instructions have fixed positions for common fields:
- opcode: bits [6:0]
- rd:     bits [11:7]
- funct3: bits [14:12]
- rs1:    bits [19:15]
- rs2:    bits [24:20]
- funct7: bits [31:25]
-/

/-- Extract opcode (bits [6:0]) -/
def getOpcode (w : Word32) : BitVec 7 :=
  w.extractLsb' 0 7

/-- Extract rd field (bits [11:7]) as register index -/
def getRd (w : Word32) : Reg :=
  let bits := w.extractLsb' 7 5
  ⟨bits.toNat, by omega⟩

/-- Extract funct3 (bits [14:12]) -/
def getFunct3 (w : Word32) : BitVec 3 :=
  w.extractLsb' 12 3

/-- Extract rs1 field (bits [19:15]) as register index -/
def getRs1 (w : Word32) : Reg :=
  let bits := w.extractLsb' 15 5
  ⟨bits.toNat, by omega⟩

/-- Extract rs2 field (bits [24:20]) as register index -/
def getRs2 (w : Word32) : Reg :=
  let bits := w.extractLsb' 20 5
  ⟨bits.toNat, by omega⟩

/-- Extract funct7 (bits [31:25]) -/
def getFunct7 (w : Word32) : BitVec 7 :=
  w.extractLsb' 25 7

/-- Extract shamt for shift instructions (bits [24:20], 5 bits) -/
def getShamt (w : Word32) : Fin 32 :=
  let bits := w.extractLsb' 20 5
  ⟨bits.toNat, by omega⟩

/-!
## Immediate Extraction

Each format stores immediates differently. These functions reconstruct
the original immediate value from the scattered bits.
-/

/-- I-type immediate: bits[31:20] → imm[11:0]
    Straightforward extraction, no scrambling. -/
def getImmI (w : Word32) : Imm12 :=
  w.extractLsb' 20 12

/-- S-type immediate: reassemble from bits[31:25] and bits[11:7]
    imm[11:5] ← bits[31:25]
    imm[4:0]  ← bits[11:7] -/
def getImmS (w : Word32) : Imm12 :=
  let hi : BitVec 7 := w.extractLsb' 25 7  -- imm[11:5]
  let lo : BitVec 5 := w.extractLsb' 7 5   -- imm[4:0]
  hi ++ lo

/-- B-type immediate: reassemble from scattered bits
    imm[12]   ← bit 31
    imm[11]   ← bit 7
    imm[10:5] ← bits[30:25]
    imm[4:1]  ← bits[11:8]
    imm[0]    = 0 (not stored, branch targets are 2-byte aligned)

    We return imm[12:1] as a 12-bit value (the stored bits). -/
def getImmB (w : Word32) : Imm12 :=
  let bit12 : BitVec 1 := w.extractLsb' 31 1   -- imm[12]
  let bit11 : BitVec 1 := w.extractLsb' 7 1    -- imm[11]
  let hi6   : BitVec 6 := w.extractLsb' 25 6   -- imm[10:5]
  let lo4   : BitVec 4 := w.extractLsb' 8 4    -- imm[4:1]
  bit12 ++ bit11 ++ hi6 ++ lo4

/-- U-type immediate: bits[31:12] → imm[19:0]
    The immediate represents the upper 20 bits of a 32-bit value. -/
def getImmU (w : Word32) : Imm20 :=
  w.extractLsb' 12 20

/-- J-type immediate: reassemble from scattered bits
    imm[20]    ← bit 31
    imm[19:12] ← bits[19:12]
    imm[11]    ← bit 20
    imm[10:1]  ← bits[30:21]
    imm[0]     = 0 (not stored, jump targets are 2-byte aligned)

    We return imm[20:1] as a 20-bit value (the stored bits). -/
def getImmJ (w : Word32) : Imm20 :=
  let bit20    : BitVec 1  := w.extractLsb' 31 1   -- imm[20]
  let bits19_12: BitVec 8  := w.extractLsb' 12 8   -- imm[19:12]
  let bit11    : BitVec 1  := w.extractLsb' 20 1   -- imm[11]
  let bits10_1 : BitVec 10 := w.extractLsb' 21 10  -- imm[10:1]
  bit20 ++ bits19_12 ++ bit11 ++ bits10_1

/-!
## Instruction Decoders by Opcode

Each opcode has a dedicated decoder that uses funct3/funct7 to
determine the exact instruction.
-/

/-- Decode R-type ALU instructions (opcode = 0110011)
    Dispatches on funct3 and funct7. -/
def decodeOP (w : Word32) : Option Instr :=
  let rd := getRd w
  let rs1 := getRs1 w
  let rs2 := getRs2 w
  let funct3 := getFunct3 w
  let funct7 := getFunct7 w
  match funct3, funct7 with
  -- Base RV32I
  | 0b000, 0b0000000 => some (.ADD rd rs1 rs2)
  | 0b000, 0b0100000 => some (.SUB rd rs1 rs2)
  | 0b001, 0b0000000 => some (.SLL rd rs1 rs2)
  | 0b010, 0b0000000 => some (.SLT rd rs1 rs2)
  | 0b011, 0b0000000 => some (.SLTU rd rs1 rs2)
  | 0b100, 0b0000000 => some (.XOR rd rs1 rs2)
  | 0b101, 0b0000000 => some (.SRL rd rs1 rs2)
  | 0b101, 0b0100000 => some (.SRA rd rs1 rs2)
  | 0b110, 0b0000000 => some (.OR rd rs1 rs2)
  | 0b111, 0b0000000 => some (.AND rd rs1 rs2)
  -- M extension (funct7 = 0000001)
  | 0b000, 0b0000001 => some (.MUL rd rs1 rs2)
  | 0b001, 0b0000001 => some (.MULH rd rs1 rs2)
  | 0b010, 0b0000001 => some (.MULHSU rd rs1 rs2)
  | 0b011, 0b0000001 => some (.MULHU rd rs1 rs2)
  | 0b100, 0b0000001 => some (.DIV rd rs1 rs2)
  | 0b101, 0b0000001 => some (.DIVU rd rs1 rs2)
  | 0b110, 0b0000001 => some (.REM rd rs1 rs2)
  | 0b111, 0b0000001 => some (.REMU rd rs1 rs2)
  | _, _ => none

/-- Decode I-type ALU instructions (opcode = 0010011)
    Note: Shifts have special encoding where funct7 is in imm[11:5]. -/
def decodeOPIMM (w : Word32) : Option Instr :=
  let rd := getRd w
  let rs1 := getRs1 w
  let imm := getImmI w
  let funct3 := getFunct3 w
  let funct7 := getFunct7 w  -- For shifts: distinguishes SRLI/SRAI
  let shamt := getShamt w
  match funct3 with
  | 0b000 => some (.ADDI rd rs1 imm)
  | 0b010 => some (.SLTI rd rs1 imm)
  | 0b011 => some (.SLTIU rd rs1 imm)
  | 0b100 => some (.XORI rd rs1 imm)
  | 0b110 => some (.ORI rd rs1 imm)
  | 0b111 => some (.ANDI rd rs1 imm)
  | 0b001 =>
    if funct7 == 0b0000000 then some (.SLLI rd rs1 shamt)
    else none
  | 0b101 =>
    if funct7 == 0b0000000 then some (.SRLI rd rs1 shamt)
    else if funct7 == 0b0100000 then some (.SRAI rd rs1 shamt)
    else none
  | _ => none

/-- Decode load instructions (opcode = 0000011) -/
def decodeLOAD (w : Word32) : Option Instr :=
  let rd := getRd w
  let rs1 := getRs1 w
  let imm := getImmI w
  let funct3 := getFunct3 w
  match funct3 with
  | 0b000 => some (.LB rd rs1 imm)
  | 0b001 => some (.LH rd rs1 imm)
  | 0b010 => some (.LW rd rs1 imm)
  | 0b100 => some (.LBU rd rs1 imm)
  | 0b101 => some (.LHU rd rs1 imm)
  | _ => none

/-- Decode store instructions (opcode = 0100011) -/
def decodeSTORE (w : Word32) : Option Instr :=
  let rs1 := getRs1 w
  let rs2 := getRs2 w
  let imm := getImmS w
  let funct3 := getFunct3 w
  match funct3 with
  | 0b000 => some (.SB rs1 rs2 imm)
  | 0b001 => some (.SH rs1 rs2 imm)
  | 0b010 => some (.SW rs1 rs2 imm)
  | _ => none

/-- Decode branch instructions (opcode = 1100011) -/
def decodeBRANCH (w : Word32) : Option Instr :=
  let rs1 := getRs1 w
  let rs2 := getRs2 w
  let imm := getImmB w
  let funct3 := getFunct3 w
  match funct3 with
  | 0b000 => some (.BEQ rs1 rs2 imm)
  | 0b001 => some (.BNE rs1 rs2 imm)
  | 0b100 => some (.BLT rs1 rs2 imm)
  | 0b101 => some (.BGE rs1 rs2 imm)
  | 0b110 => some (.BLTU rs1 rs2 imm)
  | 0b111 => some (.BGEU rs1 rs2 imm)
  | _ => none

/-- Decode JALR (opcode = 1100111, I-type format) -/
def decodeJALR (w : Word32) : Option Instr :=
  let rd := getRd w
  let rs1 := getRs1 w
  let imm := getImmI w
  let funct3 := getFunct3 w
  if funct3 == 0b000 then some (.JALR rd rs1 imm)
  else none

/-!
## Main Decode Function

Dispatches on opcode to the appropriate format decoder.
-/

/-- Decode a 32-bit word to an instruction.
    Returns `none` if the encoding is invalid or unrecognized. -/
def decode (w : Word32) : Option Instr :=
  let opcode := getOpcode w
  match opcode with
  | 0b0110011 => decodeOP w       -- R-type ALU
  | 0b0010011 => decodeOPIMM w    -- I-type ALU
  | 0b0000011 => decodeLOAD w     -- Loads
  | 0b0100011 => decodeSTORE w    -- Stores
  | 0b1100011 => decodeBRANCH w   -- Branches
  | 0b0110111 => some (.LUI (getRd w) (getImmU w))    -- LUI
  | 0b0010111 => some (.AUIPC (getRd w) (getImmU w))  -- AUIPC
  | 0b1101111 => some (.JAL (getRd w) (getImmJ w))    -- JAL
  | 0b1100111 => decodeJALR w     -- JALR
  | _ => none

end RV32EM
