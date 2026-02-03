/-
  RV32EM.Instr: Instruction datatype definitions

  This file defines the abstract syntax of RV32EM instructions.
  Each constructor represents one instruction.

  Reference Sections:
  - Section 2.4: Integer Computational Instructions (ADD, SUB, AND, OR, etc.)
  - Section 2.5: Control Transfer Instructions (BEQ, BNE, JAL, JALR)
  - Section 2.6: Load and Store Instructions (LW, SW)
  - Section 7.1: M Extension (MUL, DIV, REM)

  URL: https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/rv32.html
-/

import RV32EM.Basic

namespace RV32EM

/-!
## RISC-V Instruction Formats

All instructions are 32 bits. Six formats determine operand layout:

| Format | Fields                                      | Used by           |
|--------|---------------------------------------------|-------------------|
| R-type | funct7[31:25] rs2 rs1 funct3 rd opcode     | ALU reg-reg       |
| I-type | imm[31:20] rs1 funct3 rd opcode            | ALU imm, loads    |
| S-type | imm[31:25] rs2 rs1 funct3 imm[11:7] opcode | stores            |
| B-type | imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] op | branches          |
| U-type | imm[31:12] rd opcode                        | LUI, AUIPC        |
| J-type | imm[20|10:1|11|19:12] rd opcode            | JAL               |

Immediates are sign-extended. B-type and J-type encode multiples of 2.
-/

inductive Instr where
  /--! ### R-type: Register-Register ALU Operations
      Format: funct7 | rs2 | rs1 | funct3 | rd | opcode
      Semantics: rd := rs1 OP rs2 -/
  | ADD   (rd rs1 rs2 : Reg)  -- rd := rs1 + rs2
  | SUB   (rd rs1 rs2 : Reg)  -- rd := rs1 - rs2
  | AND   (rd rs1 rs2 : Reg)  -- rd := rs1 & rs2
  | OR    (rd rs1 rs2 : Reg)  -- rd := rs1 | rs2
  | XOR   (rd rs1 rs2 : Reg)  -- rd := rs1 ^ rs2
  | SLT   (rd rs1 rs2 : Reg)  -- rd := (rs1 <ₛ rs2) ? 1 : 0  (signed)
  | SLTU  (rd rs1 rs2 : Reg)  -- rd := (rs1 <ᵤ rs2) ? 1 : 0  (unsigned)
  | SLL   (rd rs1 rs2 : Reg)  -- rd := rs1 << rs2[4:0]
  | SRL   (rd rs1 rs2 : Reg)  -- rd := rs1 >>ᵤ rs2[4:0]  (logical)
  | SRA   (rd rs1 rs2 : Reg)  -- rd := rs1 >>ₛ rs2[4:0]  (arithmetic)




end RV32EM
