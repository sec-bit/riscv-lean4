/-
  RV32EM.Step: Operational semantics (step function)

  This file defines what each instruction DOES.
  step : Mach → Option Mach

  Returns `none` if:
  - Instruction at PC is invalid (decode fails)
  - Instruction causes an exception (e.g., misaligned access)

  Reference Sections:
  - Section 2.4: Integer Computational Instructions (semantics)
  - Section 2.5: Control Transfer Instructions (semantics)
  - Section 2.6: Load and Store Instructions (semantics)
  - Section 7.1: M Extension (semantics)

  URL: https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/rv32.html
-/

import RV32EM.Basic
import RV32EM.Instr
import RV32EM.Decode
import RV32EM.Mach

namespace RV32EM

/-!
## Execute Individual Instructions

Define semantics for each instruction.
Pattern: take current state, return new state.
-/

/-- Execute a single decoded instruction -/
def execInstr (m : Mach) (i : Instr) : Option Mach :=
  match i with
  | Instr.PLACEHOLDER => none  -- TODO: Replace with real instructions
  -- Example implementations:
  --
  -- | Instr.ADD rd rs1 rs2 =>
  --     let v1 := m.getReg rs1
  --     let v2 := m.getReg rs2
  --     let result := v1 + v2
  --     some (m.setReg rd result |>.incPC)
  --
  -- | Instr.ADDI rd rs1 imm =>
  --     let v1 := m.getReg rs1
  --     let result := v1 + signExtend12 imm
  --     some (m.setReg rd result |>.incPC)
  --
  -- | Instr.LW rd rs1 imm =>
  --     let addr := m.getReg rs1 + signExtend12 imm
  --     let value := m.getMem addr
  --     some (m.setReg rd value |>.incPC)
  --
  -- | Instr.SW rs1 rs2 imm =>
  --     let addr := m.getReg rs1 + signExtend12 imm
  --     let value := m.getReg rs2
  --     some (m.setMem addr value |>.incPC)
  --
  -- | Instr.BEQ rs1 rs2 imm =>
  --     let v1 := m.getReg rs1
  --     let v2 := m.getReg rs2
  --     if v1 = v2 then
  --       some (m.setPC (m.pc + signExtend12 imm))
  --     else
  --       some (m.incPC)
  --
  -- | Instr.JAL rd imm =>
  --     let returnAddr := m.pc + 4
  --     let target := m.pc + signExtend20 imm
  --     some (m.setReg rd returnAddr |>.setPC target)

/-!
## Main Step Function
-/

/-- Execute one step: fetch, decode, execute -/
def step (m : Mach) : Option Mach := do
  -- Fetch: read instruction at PC
  let instrWord := m.getMem m.pc
  -- Decode: convert to instruction
  let instr ← decode instrWord
  -- Execute: perform the instruction
  execInstr m instr

/-- Execute n steps -/
def exec (n : Nat) (m : Mach) : Option Mach :=
  match n with
  | 0 => some m
  | n + 1 => do
      let m' ← step m
      exec n m'

end RV32EM

