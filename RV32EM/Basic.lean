/-
  RV32EM.Basic: Fundamental types for RISC-V formalization

  Reference: RISC-V Spec Section 2.1 (Programmers' Model)
  URL: https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/rv32.html
-/

namespace RV32EM

/-!
## Word Types

RISC-V RV32 uses 32-bit words. We use Lean's BitVec for fixed-width integers.
-/

/-- 32-bit word (XLEN = 32 for RV32) -/
abbrev Word32 := BitVec 32

/-- Memory address (32-bit for RV32) -/
abbrev Addr := BitVec 32

/-!
## Register Indices

RV32I has 32 registers: x0-x31
- x0 is hardwired to 0 (writes are ignored, reads always return 0)
- x1-x31 are general purpose

Reference: RISC-V Spec Section 2.1
-/

/-- Register index (0-31) -/
abbrev Reg := Fin 32

/-!
## Immediate Values

Different instruction formats use different immediate sizes.
The actual bit manipulation happens in Encode/Decode.

Reference: RISC-V Spec Section 2.3
-/

/-- 12-bit immediate (I-type, S-type, B-type) -/
abbrev Imm12 := BitVec 12

/-- 20-bit immediate (U-type, J-type) -/
abbrev Imm20 := BitVec 20

/-!
## Helper Functions

TODO: Add sign extension, zero extension helpers as needed
-/

/-- Sign-extend a 12-bit immediate to 32 bits -/
def signExtend12 (imm : Imm12) : Word32 :=
  imm.signExtend 32

/-- Sign-extend a 20-bit immediate to 32 bits -/
def signExtend20 (imm : Imm20) : Word32 :=
  imm.signExtend 32

end RV32EM

