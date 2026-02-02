# RV32EM Lean 4 Specification

A formal specification of the RISC-V RV32EM instruction set in Lean 4.

## Status

**Work in Progress** - Skeleton only, implementation needed.

## Goal

Formalize a subset of RISC-V RV32EM with:
- Instruction encoding/decoding
- Operational semantics (step function)
- Basic correctness proofs

## Target Instructions

| Format | Instructions |
|--------|-------------|
| R-type | ADD, SUB, AND, OR, XOR, SLT |
| I-type | ADDI, ANDI, ORI, XORI, SLTI |
| I-type (load) | LW |
| S-type | SW |
| B-type | BEQ, BNE, BLT |
| U-type | LUI, AUIPC |
| J-type | JAL |
| I-type (jump) | JALR |
| M-ext | MUL, DIV |

## File Structure

```
rv32em-lean/
├── lakefile.lean       # Build configuration
├── lean-toolchain      # Lean version
├── RV32EM.lean         # Module root
└── RV32EM/
    ├── Basic.lean      # Types: Word32, Reg, Imm
    ├── Instr.lean      # Instruction datatype
    ├── Encode.lean     # Instr → BitVec 32
    ├── Decode.lean     # BitVec 32 → Option Instr
    ├── Mach.lean       # Machine state
    ├── Step.lean       # Execution semantics
    └── Proofs.lean     # Correctness theorems
```

## Building

```bash
lake build
```

## References

- [RISC-V Spec (Unprivileged)](https://five-embeddev.com/riscv-user-isa-manual/Priv-v1.12/rv32.html)
- [RISC-V Instruction Reference](https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html)
- [risc0-lean4](https://github.com/risc0/risc0-lean4) - Reference implementation

## RISC-V Spec Sections

| Section | Topic | Relevant Files |
|---------|-------|----------------|
| 2.1 | Programmers' Model | Mach.lean |
| 2.2 | Base Instruction Formats | Encode.lean, Decode.lean |
| 2.3 | Immediate Encoding | Encode.lean, Decode.lean |
| 2.4 | Integer Computational | Instr.lean, Step.lean |
| 2.5 | Control Transfer | Instr.lean, Step.lean |
| 2.6 | Load and Store | Instr.lean, Step.lean |
| 7.1 | M Extension | Instr.lean, Step.lean |

## Progress

- [x] Project setup
- [x] Basic types (Word32, Reg, Imm)
- [x] Machine state structure
- [ ] Instruction datatype (15-20 instructions)
- [ ] Encode function
- [ ] Decode function
- [ ] Step function
- [ ] Roundtrip proof (decode ∘ encode = id)
- [ ] x0 preservation proof
