//===- PS2VPUMCInstrInfo.cpp - Utility functions on PS2VPU MCInsts ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Utility functions for PS2VPU specific MCInst queries
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCINSTRINFO_H
#define LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCINSTRINFO_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/MathExtras.h"
#include <cstddef>
#include <cstdint>

namespace llvm {

class PS2VPUMCChecker;
class MCContext;
class MCExpr;
class MCInstrDesc;
class MCInstrInfo;
class MCRegisterInfo;
class MCSubtargetInfo;

namespace PS2VPU {

class PacketIterator
    : public llvm::iterator_facade_base<
          PacketIterator, std::forward_iterator_tag, const MCInst> {
  MCInstrInfo const &MCII;
  MCInst::const_iterator BundleCurrent;
  MCInst::const_iterator BundleEnd;

public:
  PacketIterator(MCInstrInfo const &MCII, MCInst const &Inst);
  PacketIterator(MCInstrInfo const &MCII, MCInst const &Inst, std::nullptr_t);

  PacketIterator &operator++();
  MCInst const &operator*() const;
  bool operator==(PacketIterator const &Other) const;
};

} // end namespace PS2VPU

namespace PS2VPUMCInstrInfo {

constexpr size_t bundleInstructionsOffset = 1;

// Return the instruction at Index
MCInst const &instruction(MCInst const &MCB, size_t Index);
// Returns whether this MCInst is a wellformed bundle
bool isBundle(MCInst const &MCI);

// Returns a iterator range of instructions in this bundle
iterator_range<PS2VPU::PacketIterator>
bundleInstructions(MCInstrInfo const &MCII, MCInst const &MCI);
iterator_range<MCInst::const_iterator> bundleInstructions(MCInst const &MCI);

// Put the packet in to canonical form, compound, duplex, pad, and shuffle
//bool canonicalizePacket(MCInstrInfo const &MCII, MCSubtargetInfo const &STI,
//                        MCContext &Context, MCInst &MCB,
//                        PS2VPUMCChecker *Checker,
//                        bool AttemptCompatibility = false);

// Returns the number of instructions in the bundle
size_t bundleSize(MCInst const &MCI);

} // end namespace PS2VPUMCInstrInfo

} // end namespace llvm

#endif // LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCINSTRINFO_H
