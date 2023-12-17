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
protected:
  /// A handle to the branch probability pass.
  const MachineBranchProbabilityInfo *MBPI;
  const MachineLoopInfo *MLI;

private:
  const PS2VPUInstrInfo *TII;
  const PS2VPURegisterInfo *TRI;
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
