/-
  RV32EM.Encode: Instruction → Word32

  Converts abstract instruction representation to 32-bit machine code.

  ## How This Relates to RISC-V Spec

  RISC-V Spec Section 2.2 defines six instruction formats. Each format
  specifies how the 32 bits are partitioned into fields:

  ```
  Bit position: 31       25 24    20 19    15 14  12 11     7 6      0
                ┌──────────┬────────┬────────┬──────┬────────┬────────┐
  R-type:       │  funct7  │  rs2   │  rs1   │funct3│   rd   │ opcode │
                │  (7 bits)│(5 bits)│(5 bits)│(3 b) │(5 bits)│(7 bits)│
                └──────────┴────────┴────────┴──────┴────────┴────────┘

                ┌───────────────────┬────────┬──────┬────────┬────────┐
  I-type:       │     imm[11:0]     │  rs1   │funct3│   rd   │ opcode │
                │     (12 bits)     │(5 bits)│(3 b) │(5 bits)│(7 bits)│
                └───────────────────┴────────┴──────┴────────┴────────┘

                ┌──────────┬────────┬────────┬──────┬────────┬────────┐
  S-type:       │imm[11:5] │  rs2   │  rs1   │funct3│imm[4:0]│ opcode │
                │  (7 bits)│(5 bits)│(5 bits)│(3 b) │(5 bits)│(7 bits)│
                └──────────┴────────┴────────┴──────┴────────┴────────┘

                ┌──────────┬────────┬────────┬──────┬────────┬────────┐
  B-type:       │  imm[12| │  rs2   │  rs1   │funct3│imm[4:1 │ opcode │
                │   10:5]  │        │        │      │   |11] │        │
                └──────────┴────────┴────────┴──────┴────────┴────────┘

                ┌─────────────────────────────────────┬────────┬────────┐
  U-type:       │            imm[31:12]               │   rd   │ opcode │
                │             (20 bits)               │(5 bits)│(7 bits)│
                └─────────────────────────────────────┴────────┴────────┘

                ┌─────────────────────────────────────┬────────┬────────┐
  J-type:       │        imm[20|10:1|11|19:12]        │   rd   │ opcode │
                │             (20 bits)               │(5 bits)│(7 bits)│
                └─────────────────────────────────────┴────────┴────────┘
  ```

  ## Encoding Strategy

  For each format, we:
  1. Convert register indices (Fin 32) to 5-bit BitVec
  2. Extract/arrange immediate bits according to format
  3. Concatenate fields left-to-right (MSB first)

  The `++` operator concatenates BitVecs: if `a : BitVec m` and `b : BitVec n`,
  then `a ++ b : BitVec (m + n)` with `a` in the high bits.

  ## Reference

  - Section 2.2: Base Instruction Formats
  - Section 2.3: Immediate Encoding Variants
  - https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html (visual reference)
-/

import RV32EM.Basic
import RV32EM.Instr

namespace RV32EM

/-!
## Opcodes (Section 2.2)

The 7-bit opcode field identifies the instruction class.
Multiple instructions share an opcode; funct3/funct7 distinguish them.
-/

def OPCODE_OP      : BitVec 7 := 0b0110011  -- R-type: ADD, SUB, AND, OR, XOR, SLT, shifts
def OPCODE_OP_IMM  : BitVec 7 := 0b0010011  -- I-type: ADDI, ANDI, ORI, XORI, SLTI, shifts
def OPCODE_LOAD    : BitVec 7 := 0b0000011  -- I-type: LW, LH, LB, LHU, LBU
def OPCODE_STORE   : BitVec 7 := 0b0100011  -- S-type: SW, SH, SB
def OPCODE_BRANCH  : BitVec 7 := 0b1100011  -- B-type: BEQ, BNE, BLT, BGE, BLTU, BGEU
def OPCODE_LUI     : BitVec 7 := 0b0110111  -- U-type: LUI
def OPCODE_AUIPC   : BitVec 7 := 0b0010111  -- U-type: AUIPC
def OPCODE_JAL     : BitVec 7 := 0b1101111  -- J-type: JAL
def OPCODE_JALR    : BitVec 7 := 0b1100111  -- I-type: JALR

/-!
## Field Conversion Helpers

Convert Lean types to the BitVec widths needed for encoding.
-/

/-- Convert register index (Fin 32) to 5-bit field -/
def regToBits (r : Reg) : BitVec 5 :=
  BitVec.ofNat 5 r.val

/-- Convert 12-bit immediate to BitVec 12 (identity, but explicit) -/
def imm12ToBits (imm : Imm12) : BitVec 12 := imm

/-- Convert 20-bit immediate to BitVec 20 (identity, but explicit) -/
def imm20ToBits (imm : Imm20) : BitVec 20 := imm

/-- Convert shift amount (Fin 32) to 5-bit field -/
def shamtToBits (sh : Fin 32) : BitVec 5 :=
  BitVec.ofNat 5 sh.val

/-!
## Format Encoders

Each format has a helper that assembles fields into 32 bits.
Fields are concatenated MSB-first using `++`.
-/

/-- R-type: funct7 ++ rs2 ++ rs1 ++ funct3 ++ rd ++ opcode -/
def encodeR (funct7 : BitVec 7) (rs2 rs1 : Reg) (funct3 : BitVec 3)
    (rd : Reg) (opcode : BitVec 7) : Word32 :=
  funct7 ++ regToBits rs2 ++ regToBits rs1 ++ funct3 ++ regToBits rd ++ opcode

/-- I-type: imm[11:0] ++ rs1 ++ funct3 ++ rd ++ opcode -/
def encodeI (imm : Imm12) (rs1 : Reg) (funct3 : BitVec 3)
    (rd : Reg) (opcode : BitVec 7) : Word32 :=
  imm ++ regToBits rs1 ++ funct3 ++ regToBits rd ++ opcode

/-- I-type shift: funct7 ++ shamt ++ rs1 ++ funct3 ++ rd ++ opcode
    Note: Shifts encode shamt in imm[4:0], with imm[11:5] as funct7 -/
def encodeIshift (funct7 : BitVec 7) (shamt : Fin 32) (rs1 : Reg)
    (funct3 : BitVec 3) (rd : Reg) (opcode : BitVec 7) : Word32 :=
  funct7 ++ shamtToBits shamt ++ regToBits rs1 ++ funct3 ++ regToBits rd ++ opcode

/-- S-type: imm[11:5] ++ rs2 ++ rs1 ++ funct3 ++ imm[4:0] ++ opcode
    The 12-bit immediate is split: high 7 bits at [31:25], low 5 bits at [11:7] -/
def encodeS (rs2 rs1 : Reg) (imm : Imm12) (funct3 : BitVec 3)
    (opcode : BitVec 7) : Word32 :=
  let immHi : BitVec 7 := imm.extractLsb' 5 7   -- imm[11:5]
  let immLo : BitVec 5 := imm.extractLsb' 0 5   -- imm[4:0]
  immHi ++ regToBits rs2 ++ regToBits rs1 ++ funct3 ++ immLo ++ opcode

/-- B-type: imm[12] ++ imm[10:5] ++ rs2 ++ rs1 ++ funct3 ++ imm[4:1] ++ imm[11] ++ opcode

    Branch immediates encode multiples of 2 (bit 0 is always 0, not stored).
    The 12-bit immediate in Instr represents imm[12:1].

    Encoding layout (Section 2.3):
    - Bit 31:    imm[12]
    - Bits 30:25: imm[10:5]
    - Bits 11:8:  imm[4:1]
    - Bit 7:     imm[11]
-/
def encodeB (rs1 rs2 : Reg) (imm : Imm12) (funct3 : BitVec 3)
    (opcode : BitVec 7) : Word32 :=
  let bit12  : BitVec 1 := imm.extractLsb' 11 1  -- imm[12] (sign bit)
  let bit11  : BitVec 1 := imm.extractLsb' 10 1  -- imm[11]
  let hi6    : BitVec 6 := imm.extractLsb' 4 6   -- imm[10:5]
  let lo4    : BitVec 4 := imm.extractLsb' 0 4   -- imm[4:1]
  bit12 ++ hi6 ++ regToBits rs2 ++ regToBits rs1 ++ funct3 ++ lo4 ++ bit11 ++ opcode

/-- U-type: imm[31:12] ++ rd ++ opcode
    The 20-bit immediate becomes bits [31:12] of the result -/
def encodeU (rd : Reg) (imm : Imm20) (opcode : BitVec 7) : Word32 :=
  imm ++ regToBits rd ++ opcode

/-- J-type: imm[20] ++ imm[10:1] ++ imm[11] ++ imm[19:12] ++ rd ++ opcode

    Jump immediates encode multiples of 2 (bit 0 not stored).
    The 20-bit immediate in Instr represents imm[20:1].

    Encoding layout (Section 2.3):
    - Bit 31:     imm[20] (sign bit)
    - Bits 30:21: imm[10:1]
    - Bit 20:     imm[11]
    - Bits 19:12: imm[19:12]
-/
def encodeJ (rd : Reg) (imm : Imm20) (opcode : BitVec 7) : Word32 :=
  let bit20   : BitVec 1  := imm.extractLsb' 19 1   -- imm[20]
  let bits10_1: BitVec 10 := imm.extractLsb' 0 10   -- imm[10:1]
  let bit11   : BitVec 1  := imm.extractLsb' 10 1   -- imm[11]
  let bits19_12: BitVec 8 := imm.extractLsb' 11 8   -- imm[19:12]
  bit20 ++ bits10_1 ++ bit11 ++ bits19_12 ++ regToBits rd ++ opcode

/-!
## Main Encode Function

Pattern match on each instruction constructor and apply the appropriate
format encoder with the correct funct3/funct7 values.

### funct3/funct7 Reference

**R-type (OPCODE_OP = 0110011):**
| Instruction | funct7    | funct3 |
|-------------|-----------|--------|
| ADD         | 0000000   | 000    |
| SUB         | 0100000   | 000    |
| SLL         | 0000000   | 001    |
| SLT         | 0000000   | 010    |
| SLTU        | 0000000   | 011    |
| XOR         | 0000000   | 100    |
| SRL         | 0000000   | 101    |
| SRA         | 0100000   | 101    |
| OR          | 0000000   | 110    |
| AND         | 0000000   | 111    |

**I-type ALU (OPCODE_OP_IMM = 0010011):**
| Instruction | funct3 | Note                        |
|-------------|--------|-----------------------------|
| ADDI        | 000    |                             |
| SLTI        | 010    |                             |
| SLTIU       | 011    |                             |
| XORI        | 100    |                             |
| ORI         | 110    |                             |
| ANDI        | 111    |                             |
| SLLI        | 001    | funct7 = 0000000            |
| SRLI        | 101    | funct7 = 0000000            |
| SRAI        | 101    | funct7 = 0100000            |

**Loads (OPCODE_LOAD = 0000011):**
| Instruction | funct3 |
|-------------|--------|
| LB          | 000    |
| LH          | 001    |
| LW          | 010    |
| LBU         | 100    |
| LHU         | 101    |

**Stores (OPCODE_STORE = 0100011):**
| Instruction | funct3 |
|-------------|--------|
| SB          | 000    |
| SH          | 001    |
| SW          | 010    |

**Branches (OPCODE_BRANCH = 1100011):**
| Instruction | funct3 |
|-------------|--------|
| BEQ         | 000    |
| BNE         | 001    |
| BLT         | 100    |
| BGE         | 101    |
| BLTU        | 110    |
| BGEU        | 111    |

**M Extension (OPCODE_OP = 0110011, funct7 = 0000001):**
| Instruction | funct3 |
|-------------|--------|
| MUL         | 000    |
| MULH        | 001    |
| MULHSU      | 010    |
| MULHU       | 011    |
| DIV         | 100    |
| DIVU        | 101    |
| REM         | 110    |
| REMU        | 111    |
-/

def encode : Instr → Word32
  -- R-type ALU (Section 2.4.2)
  | .ADD  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b000 rd OPCODE_OP
  | .SUB  rd rs1 rs2 => encodeR 0b0100000 rs2 rs1 0b000 rd OPCODE_OP
  | .AND  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b111 rd OPCODE_OP
  | .OR   rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b110 rd OPCODE_OP
  | .XOR  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b100 rd OPCODE_OP
  | .SLT  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b010 rd OPCODE_OP
  | .SLTU rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b011 rd OPCODE_OP
  | .SLL  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b001 rd OPCODE_OP
  | .SRL  rd rs1 rs2 => encodeR 0b0000000 rs2 rs1 0b101 rd OPCODE_OP
  | .SRA  rd rs1 rs2 => encodeR 0b0100000 rs2 rs1 0b101 rd OPCODE_OP

  -- I-type ALU (Section 2.4.1)
  | .ADDI  rd rs1 imm => encodeI imm rs1 0b000 rd OPCODE_OP_IMM
  | .ANDI  rd rs1 imm => encodeI imm rs1 0b111 rd OPCODE_OP_IMM
  | .ORI   rd rs1 imm => encodeI imm rs1 0b110 rd OPCODE_OP_IMM
  | .XORI  rd rs1 imm => encodeI imm rs1 0b100 rd OPCODE_OP_IMM
  | .SLTI  rd rs1 imm => encodeI imm rs1 0b010 rd OPCODE_OP_IMM
  | .SLTIU rd rs1 imm => encodeI imm rs1 0b011 rd OPCODE_OP_IMM

  -- I-type shifts (Section 2.4.1)
  | .SLLI rd rs1 shamt => encodeIshift 0b0000000 shamt rs1 0b001 rd OPCODE_OP_IMM
  | .SRLI rd rs1 shamt => encodeIshift 0b0000000 shamt rs1 0b101 rd OPCODE_OP_IMM
  | .SRAI rd rs1 shamt => encodeIshift 0b0100000 shamt rs1 0b101 rd OPCODE_OP_IMM

  -- Loads (Section 2.6)
  | .LW  rd rs1 imm => encodeI imm rs1 0b010 rd OPCODE_LOAD
  | .LH  rd rs1 imm => encodeI imm rs1 0b001 rd OPCODE_LOAD
  | .LB  rd rs1 imm => encodeI imm rs1 0b000 rd OPCODE_LOAD
  | .LHU rd rs1 imm => encodeI imm rs1 0b101 rd OPCODE_LOAD
  | .LBU rd rs1 imm => encodeI imm rs1 0b100 rd OPCODE_LOAD

  -- Stores (Section 2.6)
  | .SW rs1 rs2 imm => encodeS rs2 rs1 imm 0b010 OPCODE_STORE
  | .SH rs1 rs2 imm => encodeS rs2 rs1 imm 0b001 OPCODE_STORE
  | .SB rs1 rs2 imm => encodeS rs2 rs1 imm 0b000 OPCODE_STORE

  -- Branches (Section 2.5.2)
  | .BEQ  rs1 rs2 imm => encodeB rs1 rs2 imm 0b000 OPCODE_BRANCH
  | .BNE  rs1 rs2 imm => encodeB rs1 rs2 imm 0b001 OPCODE_BRANCH
  | .BLT  rs1 rs2 imm => encodeB rs1 rs2 imm 0b100 OPCODE_BRANCH
  | .BGE  rs1 rs2 imm => encodeB rs1 rs2 imm 0b101 OPCODE_BRANCH
  | .BLTU rs1 rs2 imm => encodeB rs1 rs2 imm 0b110 OPCODE_BRANCH
  | .BGEU rs1 rs2 imm => encodeB rs1 rs2 imm 0b111 OPCODE_BRANCH

  -- U-type (Section 2.4.1)
  | .LUI   rd imm => encodeU rd imm OPCODE_LUI
  | .AUIPC rd imm => encodeU rd imm OPCODE_AUIPC

  -- J-type (Section 2.5.1)
  | .JAL rd imm => encodeJ rd imm OPCODE_JAL

  -- JALR (Section 2.5.1) - I-type format
  | .JALR rd rs1 imm => encodeI imm rs1 0b000 rd OPCODE_JALR

  -- M extension (Section 7.1, 7.2)
  | .MUL    rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b000 rd OPCODE_OP
  | .MULH   rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b001 rd OPCODE_OP
  | .MULHSU rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b010 rd OPCODE_OP
  | .MULHU  rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b011 rd OPCODE_OP
  | .DIV    rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b100 rd OPCODE_OP
  | .DIVU   rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b101 rd OPCODE_OP
  | .REM    rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b110 rd OPCODE_OP
  | .REMU   rd rs1 rs2 => encodeR 0b0000001 rs2 rs1 0b111 rd OPCODE_OP

end RV32EM
