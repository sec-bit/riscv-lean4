/-
  RV32EM: A Lean 4 Formalization of RISC-V RV32EM

  This is a formal specification of the RISC-V RV32EM instruction set,
  including instruction encoding/decoding and operational semantics.

  RV32EM = RV32I (base integer) + M extension (multiply/divide)
         - E variant uses 16 registers instead of 32 (we use 32 for now)
-/

import RV32EM.Basic
-- import RV32EM.Instr    -- Uncomment as you implement
-- import RV32EM.Encode
-- import RV32EM.Decode
-- import RV32EM.Mach
-- import RV32EM.Step
-- import RV32EM.Proofs

