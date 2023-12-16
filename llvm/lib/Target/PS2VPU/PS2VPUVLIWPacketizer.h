//===- PS2VPUPacketizer.h - VLIW packetizer --------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_PS2VPUVLIWPACKETIZER_H
#define LLVM_LIB_TARGET_PS2VPU_PS2VPUVLIWPACKETIZER_H

#include "llvm/CodeGen/DFAPacketizer.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include <vector>

namespace llvm {

class PS2VPUInstrInfo;
class PS2VPURegisterInfo;
class MachineBranchProbabilityInfo;
class MachineFunction;
class MachineInstr;
class MachineLoopInfo;
class TargetRegisterClass;

class PS2VPUPacketizerList : public VLIWPacketizerList {
  // Vector of instructions assigned to the packet that has just been created.
  std::vector<MachineInstr *> OldPacketMIs;

  // Has the instruction been promoted to a dot-new instruction.
  bool PromotedToDotNew;

  // Has the instruction been glued to allocframe.
  bool GlueAllocframeStore;

  // Has the feeder instruction been glued to new value jump.
  bool GlueToNewValueJump;

  // This holds the offset value, when pruning the dependences.
  int64_t ChangedOffset;

  // Check if there is a dependence between some instruction already in this
  // packet and this instruction.
  bool Dependence;

  // Only check for dependence if there are resources available to
  // schedule this instruction.
  bool FoundSequentialDependence;

  bool MemShufDisabled = false;

  // Track MIs with ignored dependence.
  std::vector<MachineInstr *> IgnoreDepMIs;

  // Set to true if the packet contains an instruction that stalls with an
  // instruction from the previous packet.
  bool PacketStalls = false;
  // Set to the number of cycles of stall a given instruction will incur
  // because of dependence on instruction in previous packet.
  unsigned int PacketStallCycles = 0;

  // Set to true if the packet has a duplex pair of sub-instructions.
  bool PacketHasDuplex = false;

  // Set to true if the packet has a instruction that can only be executed
  // in SLOT0.
  bool PacketHasSLOT0OnlyInsn = false;

protected:
  /// A handle to the branch probability pass.
  const MachineBranchProbabilityInfo *MBPI;
  const MachineLoopInfo *MLI;

private:
  const PS2VPUInstrInfo *HII;
  const PS2VPURegisterInfo *HRI;
  const bool Minimal;

public:
  PS2VPUPacketizerList(MachineFunction &MF, MachineLoopInfo &MLI,
                        AAResults *AA, const MachineBranchProbabilityInfo *MBPI,
                        bool Minimal);

  // initPacketizerState - initialize some internal flags.
  void initPacketizerState() override;

  // ignorePseudoInstruction - Ignore bundling of pseudo instructions.
  bool ignorePseudoInstruction(const MachineInstr &MI,
                               const MachineBasicBlock *MBB) override;

  // isSoloInstruction - return true if instruction MI can not be packetized
  // with any other instruction, which means that MI itself is a packet.
  bool isSoloInstruction(const MachineInstr &MI) override;

  // isLegalToPacketizeTogether - Is it legal to packetize SUI and SUJ
  // together.
  bool isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ) override;

  // isLegalToPruneDependencies - Is it legal to prune dependece between SUI
  // and SUJ.
  bool isLegalToPruneDependencies(SUnit *SUI, SUnit *SUJ) override;

  MachineBasicBlock::iterator addToPacket(MachineInstr &MI) override;
  void endPacket(MachineBasicBlock *MBB,
                 MachineBasicBlock::iterator MI) override;
  bool shouldAddToPacket(const MachineInstr &MI) override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_PS2VPU_PS2VPUVLIWPACKETIZER_H
