//===- PS2VPUMCInstrInfo.cpp - PS2VPU sub-class of MCInst ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class extends MCInstrInfo to allow PS2VPU specific MCInstr queries
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "MCTargetDesc/PS2VPUMCInstrInfo.h"
#include "MCTargetDesc/PS2VPUMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstdint>
#include <limits>

using namespace llvm;

PS2VPU::PacketIterator::PacketIterator(MCInstrInfo const &MCII,
                                        MCInst const &Inst)
    : MCII(MCII), BundleCurrent(Inst.begin() +
                                PS2VPUMCInstrInfo::bundleInstructionsOffset),
      BundleEnd(Inst.end()) {}

PS2VPU::PacketIterator::PacketIterator(MCInstrInfo const &MCII,
                                        MCInst const &Inst, std::nullptr_t)
    : MCII(MCII), BundleCurrent(Inst.end()), BundleEnd(Inst.end()) {}

PS2VPU::PacketIterator &PS2VPU::PacketIterator::operator++() {
  ++BundleCurrent;
  return *this;
}

MCInst const &PS2VPU::PacketIterator::operator*() const {
  return *BundleCurrent->getInst();
}

bool PS2VPU::PacketIterator::operator==(PacketIterator const &Other) const {
  return BundleCurrent == Other.BundleCurrent && BundleEnd == Other.BundleEnd;
}

iterator_range<PS2VPU::PacketIterator>
PS2VPUMCInstrInfo::bundleInstructions(MCInstrInfo const &MCII,
                                       MCInst const &MCI) {
  assert(isBundle(MCI));
  return make_range(PS2VPU::PacketIterator(MCII, MCI),
                    PS2VPU::PacketIterator(MCII, MCI, nullptr));
}

iterator_range<MCInst::const_iterator>
PS2VPUMCInstrInfo::bundleInstructions(MCInst const &MCI) {
  assert(isBundle(MCI));
  return drop_begin(MCI, bundleInstructionsOffset);
}

size_t PS2VPUMCInstrInfo::bundleSize(MCInst const &MCI) {
  if (PS2VPUMCInstrInfo::isBundle(MCI))
    return (MCI.size() - bundleInstructionsOffset);
  else
    return (1);
}
StringRef PS2VPUMCInstrInfo::getName(MCInstrInfo const &MCII,
                                      MCInst const &MCI) {
  return MCII.getName(MCI.getOpcode());
}
MCInstrDesc const &PS2VPUMCInstrInfo::getDesc(MCInstrInfo const &MCII,
                                               MCInst const &MCI) {
  return MCII.get(MCI.getOpcode());
}
//
//namespace {
//bool canonicalizePacketImpl(MCInstrInfo const &MCII, MCSubtargetInfo const &STI,
//                            MCContext &Context, MCInst &MCB,
//                            PS2VPUMCChecker *Check) {
//  // Check the bundle for errors.
//  bool CheckOk = Check ? Check->check(false) : true;
//  if (!CheckOk)
//    return false;
//
//  MCInst OrigMCB = MCB;
//
//  // Examine the packet and convert pairs of instructions to compound
//  // instructions when possible.
//  if (!PS2VPUDisableCompound)
//    PS2VPUMCInstrInfo::tryCompound(MCII, STI, Context, MCB);
//  PS2VPUMCShuffle(Context, false, MCII, STI, MCB);
//
//  const SmallVector<DuplexCandidate, 8> possibleDuplexes =
//      (STI.hasFeature(PS2VPU::FeatureDuplex))
//          ? PS2VPUMCInstrInfo::getDuplexPossibilties(MCII, STI, MCB)
//          : SmallVector<DuplexCandidate, 8>();
//
//  // Examine the packet and convert pairs of instructions to duplex
//  // instructions when possible.
//  PS2VPUMCShuffle(Context, MCII, STI, MCB, possibleDuplexes);
//
//  // Examines packet and pad the packet, if needed, when an
//  // end-loop is in the bundle.
//  PS2VPUMCInstrInfo::padEndloop(MCB, Context);
//
//  // If compounding and duplexing didn't reduce the size below
//  // 4 or less we have a packet that is too big.
//  if (PS2VPUMCInstrInfo::bundleSize(MCB) > PS2VPU_PACKET_SIZE) {
//    if (Check)
//      Check->reportError("invalid instruction packet: out of slots");
//    return false;
//  }
//  // Check the bundle for errors.
//  CheckOk = Check ? Check->check(true) : true;
//  if (!CheckOk)
//    return false;
//
//  PS2VPUMCShuffle(Context, true, MCII, STI, MCB);
//
//  return true;
//}
//} // namespace
//
//bool PS2VPUMCInstrInfo::canonicalizePacket(MCInstrInfo const &MCII,
//                                            MCSubtargetInfo const &STI,
//                                            MCContext &Context, MCInst &MCB,
//                                            PS2VPUMCChecker *Check,
//                                            bool AttemptCompatibility) {
//  auto ArchSTI = PS2VPU_MC::getArchSubtarget(&STI);
//  if (!AttemptCompatibility || ArchSTI == nullptr)
//    return canonicalizePacketImpl(MCII, STI, Context, MCB, Check);
//
//  const MCRegisterInfo *RI = Context.getRegisterInfo();
//  PS2VPUMCChecker DefaultCheck(Context, MCII, STI, MCB, *RI, false);
//  PS2VPUMCChecker *BaseCheck = (Check == nullptr) ? &DefaultCheck : Check;
//  PS2VPUMCChecker PerfCheck(*BaseCheck, STI, false);
//  if (canonicalizePacketImpl(MCII, STI, Context, MCB, &PerfCheck))
//    return true;
//
//  PS2VPUMCChecker ArchCheck(*BaseCheck, *ArchSTI, true);
//  return canonicalizePacketImpl(MCII, *ArchSTI, Context, MCB, &ArchCheck);
//}

MCInst const &PS2VPUMCInstrInfo::instruction(MCInst const &MCB, size_t Index) {
  assert(isBundle(MCB));
  //assert(Index < PS2VPU_PRESHUFFLE_PACKET_SIZE);
  return *MCB.getOperand(bundleInstructionsOffset + Index).getInst();
}

bool PS2VPUMCInstrInfo::isBundle(MCInst const &MCI) {
  auto Result = PS2VPUNS::BUNDLE == MCI.getOpcode();
  assert(!Result || (MCI.size() > 0 && MCI.getOperand(0).isImm()));
  return Result;
}

bool PS2VPUMCInstrInfo::isUpperInstruction(MCInstrInfo const &MCII,
                                           const MCInst &MCI) {
  return MCII.get(MCI.getOpcode()).TSFlags & (((uint64_t)1) << 63);
}
bool PS2VPUMCInstrInfo::isLowerInstruction(MCInstrInfo const &MCII,
                                           const MCInst &MCI) {
  return !isUpperInstruction(MCII, MCI);
}