
-- RV32EM.Mach: Machine state definition
-- This file defines the state of a RISC-V machine:
-- - Program counter (pc)
-- - Register file (x0-x31, where x0 is always 0)
-- - Memory (simplified model)
-- Reference: RISC-V Spec Section 2.1 (Programmers' Model)
-- URL: https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/rv32.html


import RV32EM.Basic

namespace RV32EM

/-!
## Register File

x0 is hardwired to 0. We can model this in two ways:
1. Store 32 registers, but read x0 always returns 0
2. Store 31 registers, x0 is not stored

We use approach 1 for simplicity, enforcing x0 = 0 in read/write functions.
-/

-- Register file: 32 registers -/
structure RegFile where
  regs : Fin 32 → Word32

namespace RegFile

-- Create initial register file (all zeros)
def init : RegFile :=
  ⟨fun _ => 0⟩

-- Read a register (x0 always returns 0)
def read (rf : RegFile) (r : Reg) : Word32 :=
  if r = 0 then 0 else rf.regs r

-- Write to a register (writes to x0 are ignored)
def write (rf : RegFile) (r : Reg) (v : Word32) : RegFile :=
  if r = 0 then rf else ⟨fun i => if i = r then v else rf.regs i⟩

end RegFile

/-!
## Memory Model

Simplified: word-addressable memory as a function.
Real RISC-V is byte-addressable, but word-addressable is simpler to start.

TODO: Consider byte-addressable memory for LB/SB instructions.
-/

-- Memory: maps addresses to words -/
structure Memory where
  mem : Addr → Word32

namespace Memory

-- Create empty memory (all zeros) -/
def init : Memory :=
  ⟨fun _ => 0⟩

-- Read a word from memory -/
def read (m : Memory) (addr : Addr) : Word32 :=
  m.mem addr

-- Write a word to memory -/
def write (m : Memory) (addr : Addr) (v : Word32) : Memory :=
  ⟨fun a => if a = addr then v else m.mem a⟩

end Memory

/-!
## Machine State

Complete state of the machine.
-/

-- Complete machine state -/
structure Mach where
  pc   : Addr      -- Program counter
  regs : RegFile   -- Register file
  mem  : Memory    -- Memory

namespace Mach

-- Create initial machine state -/
def init : Mach :=
  { pc := 0
  , regs := RegFile.init
  , mem := Memory.init }

-- Read register -/
def getReg (m : Mach) (r : Reg) : Word32 :=
  m.regs.read r

-- Write register -/
def setReg (m : Mach) (r : Reg) (v : Word32) : Mach :=
  { m with regs := m.regs.write r v }

-- Read memory -/
def getMem (m : Mach) (addr : Addr) : Word32 :=
  m.mem.read addr

-- Write memory -/
def setMem (m : Mach) (addr : Addr) (v : Word32) : Mach :=
  { m with mem := m.mem.write addr v }

-- Increment PC by 4 (standard instruction size) -/
def incPC (m : Mach) : Mach :=
  { m with pc := m.pc + 4 }

-- Set PC to a new address -/
def setPC (m : Mach) (addr : Addr) : Mach :=
  { m with pc := addr }

end Mach

end RV32EM
