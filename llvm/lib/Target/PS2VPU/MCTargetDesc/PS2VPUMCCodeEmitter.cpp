//===-- PS2VPUMCCodeEmitter.cpp - Convert PS2VPU code to machine code -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the PS2VPUMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUFixupKinds.h"
#include "PS2VPUMCExpr.h"
#include "PS2VPUMCTargetDesc.h"
#include "PS2VPUMCInstrInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/TargetParser/SubtargetFeature.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");

namespace {

class PS2VPUMCCodeEmitter : public MCCodeEmitter {
  MCContext &Ctx;
  MCInstrInfo const &MCII;

public:
  PS2VPUMCCodeEmitter(const MCInstrInfo &MII, MCContext &ctx)
      : Ctx(ctx), MCII(MII) {}
  PS2VPUMCCodeEmitter(const PS2VPUMCCodeEmitter &) = delete;
  PS2VPUMCCodeEmitter &operator=(const PS2VPUMCCodeEmitter &) = delete;
  ~PS2VPUMCCodeEmitter() override = default;

  void encodeInstruction(const MCInst &MI, SmallVectorImpl<char> &CB,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;
  void encodeSingleInstruction(const MCInst &MI, SmallVectorImpl<char> &CB,
                               SmallVectorImpl<MCFixup> &Fixups,
                               const MCSubtargetInfo &STI) const;

  // getBinaryCodeForInstr - TableGen'erated function for getting the
  // binary encoding for an instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// getMachineOpValue - Return binary encoding of operand. If the machine
  /// operand requires relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;
  unsigned getCallTargetOpValue(const MCInst &MI, unsigned OpNo,
                                SmallVectorImpl<MCFixup> &Fixups,
                                const MCSubtargetInfo &STI) const;
  unsigned getBranchTargetOpValue(const MCInst &MI, unsigned OpNo,
                                  SmallVectorImpl<MCFixup> &Fixups,
                                  const MCSubtargetInfo &STI) const;
  unsigned getSImm13OpValue(const MCInst &MI, unsigned OpNo,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const;
  unsigned getBranchPredTargetOpValue(const MCInst &MI, unsigned OpNo,
                                      SmallVectorImpl<MCFixup> &Fixups,
                                      const MCSubtargetInfo &STI) const;
  unsigned getBranchOnRegTargetOpValue(const MCInst &MI, unsigned OpNo,
                                       SmallVectorImpl<MCFixup> &Fixups,
                                       const MCSubtargetInfo &STI) const;
};

} // end anonymous namespace

void PS2VPUMCCodeEmitter::encodeInstruction(const MCInst &MI,
                                            SmallVectorImpl<char> &CB,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  MCInst &HMB = const_cast<MCInst &>(MI);
  assert(PS2VPUMCInstrInfo::isBundle(HMB));
  assert(PS2VPUMCInstrInfo::bundleSize(HMB) == 2);
  LLVM_DEBUG(dbgs() << "Encoding bundle\n";);

  // We need special handling here because some lower instructions (eg. LOI)
  // affect upper instruction encoding
  auto LowerInstr = PS2VPUMCInstrInfo::instruction(HMB, 0);
  auto UpperInstr = PS2VPUMCInstrInfo::instruction(HMB, 1);

  assert(PS2VPUMCInstrInfo::isLowerInstruction(MCII, LowerInstr));
  assert(PS2VPUMCInstrInfo::isUpperInstruction(MCII, UpperInstr));

  LLVM_DEBUG(dbgs() << "Encoding insn `"
                    << PS2VPUMCInstrInfo::getName(MCII, UpperInstr) << "'\n");
  LLVM_DEBUG(dbgs() << "Encoding insn `"
                    << PS2VPUMCInstrInfo::getName(MCII, LowerInstr) << "'\n");

  unsigned UpperBits = getBinaryCodeForInstr(UpperInstr, Fixups, STI);
  unsigned LowerBits = 0;
  if (LowerInstr.getOpcode() == PS2VPUNS::LOIv1)
  {
    UpperBits |= 0x80000000;
    LowerBits = LowerInstr.getOperand(1).getSFPImm();
  }
  else {
    LowerBits = getBinaryCodeForInstr(LowerInstr, Fixups, STI);
  }

  support::endian::write(CB, LowerBits,
                         Ctx.getAsmInfo()->isLittleEndian()
                             ? llvm::endianness::little
                             : llvm::endianness::big);
  support::endian::write(CB, UpperBits,
                         Ctx.getAsmInfo()->isLittleEndian()
                             ? llvm::endianness::little
                             : llvm::endianness::big);

  MCNumEmitted += 2; // Keep track of the # of mi's emitted.
}

unsigned
PS2VPUMCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                                      SmallVectorImpl<MCFixup> &Fixups,
                                      const MCSubtargetInfo &STI) const {
  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return MO.getImm();

  assert(MO.isExpr());
  const MCExpr *Expr = MO.getExpr();
  if (const PS2VPUMCExpr *SExpr = dyn_cast<PS2VPUMCExpr>(Expr)) {
    MCFixupKind Kind = (MCFixupKind)SExpr->getFixupKind();
    Fixups.push_back(MCFixup::create(0, Expr, Kind));
    return 0;
  }

  int64_t Res;
  if (Expr->evaluateAsAbsolute(Res))
    return Res;

  llvm_unreachable("Unhandled expression!");
  return 0;
}

unsigned
PS2VPUMCCodeEmitter::getSImm13OpValue(const MCInst &MI, unsigned OpNo,
                                     SmallVectorImpl<MCFixup> &Fixups,
                                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm())
    return MO.getImm();

  assert(MO.isExpr() &&
         "getSImm13OpValue expects only expressions or an immediate");

  const MCExpr *Expr = MO.getExpr();

  // Constant value, no fixup is needed
  if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
    return CE->getValue();

  MCFixupKind Kind;
  if (const PS2VPUMCExpr *SExpr = dyn_cast<PS2VPUMCExpr>(Expr)) {
    Kind = MCFixupKind(SExpr->getFixupKind());
  } else {
    bool IsPic = Ctx.getObjectFileInfo()->isPositionIndependent();
    Kind = IsPic ? MCFixupKind(PS2VPU::fixup_PS2VPU_got13)
                 : MCFixupKind(PS2VPU::fixup_PS2VPU_13);
  }

  Fixups.push_back(MCFixup::create(0, Expr, Kind));
  return 0;
}

unsigned
PS2VPUMCCodeEmitter::getCallTargetOpValue(const MCInst &MI, unsigned OpNo,
                                         SmallVectorImpl<MCFixup> &Fixups,
                                         const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  const MCExpr *Expr = MO.getExpr();
  const PS2VPUMCExpr *SExpr = dyn_cast<PS2VPUMCExpr>(Expr);

//  if (MI.getOpcode() == SP::TLS_CALL) {
//    // No fixups for __tls_get_addr. Will emit for fixups for tls_symbol in
//    // encodeInstruction.
//#ifndef NDEBUG
//    // Verify that the callee is actually __tls_get_addr.
//    assert(SExpr && SExpr->getSubExpr()->getKind() == MCExpr::SymbolRef &&
//           "Unexpected expression in TLS_CALL");
//    const MCSymbolRefExpr *SymExpr = cast<MCSymbolRefExpr>(SExpr->getSubExpr());
//    assert(SymExpr->getSymbol().getName() == "__tls_get_addr" &&
//           "Unexpected function for TLS_CALL");
//#endif
//    return 0;
//  }

  MCFixupKind Kind = MCFixupKind(SExpr->getFixupKind());
  Fixups.push_back(MCFixup::create(0, Expr, Kind));
  return 0;
}

unsigned
PS2VPUMCCodeEmitter::getBranchTargetOpValue(const MCInst &MI, unsigned OpNo,
                                           SmallVectorImpl<MCFixup> &Fixups,
                                           const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isReg() || MO.isImm())
    return getMachineOpValue(MI, MO, Fixups, STI);

  Fixups.push_back(
      MCFixup::create(0, MO.getExpr(), (MCFixupKind)PS2VPU::fixup_PS2VPU_br22));
  return 0;
}

unsigned PS2VPUMCCodeEmitter::getBranchPredTargetOpValue(
    const MCInst &MI, unsigned OpNo, SmallVectorImpl<MCFixup> &Fixups,
    const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isReg() || MO.isImm())
    return getMachineOpValue(MI, MO, Fixups, STI);

  Fixups.push_back(
      MCFixup::create(0, MO.getExpr(), (MCFixupKind)PS2VPU::fixup_PS2VPU_br19));
  return 0;
}

unsigned PS2VPUMCCodeEmitter::getBranchOnRegTargetOpValue(
    const MCInst &MI, unsigned OpNo, SmallVectorImpl<MCFixup> &Fixups,
    const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isReg() || MO.isImm())
    return getMachineOpValue(MI, MO, Fixups, STI);

  Fixups.push_back(
      MCFixup::create(0, MO.getExpr(), (MCFixupKind)PS2VPU::fixup_PS2VPU_br16_2));
  Fixups.push_back(MCFixup::create(0, MO.getExpr(),
                                   (MCFixupKind)PS2VPU::fixup_PS2VPU_br16_14));

  return 0;
}

#include "PS2VPUGenMCCodeEmitter.inc"

MCCodeEmitter *llvm::createPS2VPUMCCodeEmitter(const MCInstrInfo &MCII,
                                              MCContext &Ctx) {
  return new PS2VPUMCCodeEmitter(MCII, Ctx);
}
