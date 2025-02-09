//===-- PS2VPUInstrInfo.cpp - PS2VPU Instruction Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the PS2VPU implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//#include "PS2VPU.h"
#include "PS2VPUInstrInfo.h"
//#include "PS2VPUMachineFunctionInfo.h"
#include "PS2VPUSubtarget.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
//#include "PS2VPUDepTimingClasses.h"
#include "PS2VPUGenDFAPacketizer.inc"
#include "PS2VPUGenInstrInfo.inc"

// Pin the vtable to this file.
void PS2VPUInstrInfo::anchor() {}

PS2VPUInstrInfo::PS2VPUInstrInfo(PS2VPUSubtarget &ST)
    : PS2VPUGenInstrInfo(), RI(),
      Subtarget(ST) {}

///// isLoadFromStackSlot - If the specified machine instruction is a direct
///// load from a stack slot, return the virtual or physical register number of
///// the destination along with the FrameIndex of the loaded stack slot.  If
///// not, return 0.  This predicate must return 0 if the instruction has
///// any side effects other than loading from the stack slot.
unsigned PS2VPUInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                             int &FrameIndex) const {
  /*if (MI.getOpcode() == SP::LDri || MI.getOpcode() == SP::LDXri ||
      MI.getOpcode() == SP::LDFri || MI.getOpcode() == SP::LDDFri ||
      MI.getOpcode() == SP::LDQFri) {
    if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
        MI.getOperand(2).getImm() == 0) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
  }*/
  return 0;
}
//
///// isStoreToStackSlot - If the specified machine instruction is a direct
///// store to a stack slot, return the virtual or physical register number of
///// the source reg along with the FrameIndex of the loaded stack slot.  If
///// not, return 0.  This predicate must return 0 if the instruction has
///// any side effects other than storing to the stack slot.
unsigned PS2VPUInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                            int &FrameIndex) const {
  /*if (MI.getOpcode() == SP::STri || MI.getOpcode() == SP::STXri ||
      MI.getOpcode() == SP::STFri || MI.getOpcode() == SP::STDFri ||
      MI.getOpcode() == SP::STQFri) {
    if (MI.getOperand(0).isFI() && MI.getOperand(1).isImm() &&
        MI.getOperand(1).getImm() == 0) {
      FrameIndex = MI.getOperand(0).getIndex();
      return MI.getOperand(2).getReg();
    }
  }*/
  return 0;
}
//
//static bool IsIntegerCC(unsigned CC) { return (CC <= SPCC::ICC_VC); }
//
//static SPCC::CondCodes GetOppositeBranchCondition(SPCC::CondCodes CC) {
//  switch (CC) {
//  case SPCC::ICC_A:
//    return SPCC::ICC_N;
//  case SPCC::ICC_N:
//    return SPCC::ICC_A;
//  case SPCC::ICC_NE:
//    return SPCC::ICC_E;
//  case SPCC::ICC_E:
//    return SPCC::ICC_NE;
//  case SPCC::ICC_G:
//    return SPCC::ICC_LE;
//  case SPCC::ICC_LE:
//    return SPCC::ICC_G;
//  case SPCC::ICC_GE:
//    return SPCC::ICC_L;
//  case SPCC::ICC_L:
//    return SPCC::ICC_GE;
//  case SPCC::ICC_GU:
//    return SPCC::ICC_LEU;
//  case SPCC::ICC_LEU:
//    return SPCC::ICC_GU;
//  case SPCC::ICC_CC:
//    return SPCC::ICC_CS;
//  case SPCC::ICC_CS:
//    return SPCC::ICC_CC;
//  case SPCC::ICC_POS:
//    return SPCC::ICC_NEG;
//  case SPCC::ICC_NEG:
//    return SPCC::ICC_POS;
//  case SPCC::ICC_VC:
//    return SPCC::ICC_VS;
//  case SPCC::ICC_VS:
//    return SPCC::ICC_VC;
//
//  case SPCC::FCC_A:
//    return SPCC::FCC_N;
//  case SPCC::FCC_N:
//    return SPCC::FCC_A;
//  case SPCC::FCC_U:
//    return SPCC::FCC_O;
//  case SPCC::FCC_O:
//    return SPCC::FCC_U;
//  case SPCC::FCC_G:
//    return SPCC::FCC_ULE;
//  case SPCC::FCC_LE:
//    return SPCC::FCC_UG;
//  case SPCC::FCC_UG:
//    return SPCC::FCC_LE;
//  case SPCC::FCC_ULE:
//    return SPCC::FCC_G;
//  case SPCC::FCC_L:
//    return SPCC::FCC_UGE;
//  case SPCC::FCC_GE:
//    return SPCC::FCC_UL;
//  case SPCC::FCC_UL:
//    return SPCC::FCC_GE;
//  case SPCC::FCC_UGE:
//    return SPCC::FCC_L;
//  case SPCC::FCC_LG:
//    return SPCC::FCC_UE;
//  case SPCC::FCC_UE:
//    return SPCC::FCC_LG;
//  case SPCC::FCC_NE:
//    return SPCC::FCC_E;
//  case SPCC::FCC_E:
//    return SPCC::FCC_NE;
//
//  case SPCC::CPCC_A:
//    return SPCC::CPCC_N;
//  case SPCC::CPCC_N:
//    return SPCC::CPCC_A;
//  case SPCC::CPCC_3:
//    [[fallthrough]];
//  case SPCC::CPCC_2:
//    [[fallthrough]];
//  case SPCC::CPCC_23:
//    [[fallthrough]];
//  case SPCC::CPCC_1:
//    [[fallthrough]];
//  case SPCC::CPCC_13:
//    [[fallthrough]];
//  case SPCC::CPCC_12:
//    [[fallthrough]];
//  case SPCC::CPCC_123:
//    [[fallthrough]];
//  case SPCC::CPCC_0:
//    [[fallthrough]];
//  case SPCC::CPCC_03:
//    [[fallthrough]];
//  case SPCC::CPCC_02:
//    [[fallthrough]];
//  case SPCC::CPCC_023:
//    [[fallthrough]];
//  case SPCC::CPCC_01:
//    [[fallthrough]];
//  case SPCC::CPCC_013:
//    [[fallthrough]];
//  case SPCC::CPCC_012:
//    // "Opposite" code is not meaningful, as we don't know
//    // what the CoProc condition means here. The cond-code will
//    // only be used in inline assembler, so this code should
//    // not be reached in a normal compilation pass.
//    llvm_unreachable("Meaningless inversion of co-processor cond code");
//  }
//  llvm_unreachable("Invalid cond code");
//}
//
//static bool isUncondBranchOpcode(int Opc) { return Opc == SP::BA; }
//
//static bool isCondBranchOpcode(int Opc) {
//  return Opc == SP::FBCOND || Opc == SP::BCOND;
//}
//
//static bool isIndirectBranchOpcode(int Opc) {
//  return Opc == SP::BINDrr || Opc == SP::BINDri;
//}
//
//static void parseCondBranch(MachineInstr *LastInst, MachineBasicBlock *&Target,
//                            SmallVectorImpl<MachineOperand> &Cond) {
//  Cond.push_back(MachineOperand::CreateImm(LastInst->getOperand(1).getImm()));
//  Target = LastInst->getOperand(0).getMBB();
//}
//
bool PS2VPUInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return false;

  if (!isUnpredicatedTerminator(*I))
    return false;

  // Get the last instruction in the block.
  //MachineInstr *LastInst = &*I;
  //unsigned LastOpc = LastInst->getOpcode();

  //// If there is only one terminator instruction, process it.
  //if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
  //  if (isUncondBranchOpcode(LastOpc)) {
  //    TBB = LastInst->getOperand(0).getMBB();
  //    return false;
  //  }
  //  if (isCondBranchOpcode(LastOpc)) {
  //    // Block ends with fall-through condbranch.
  //    parseCondBranch(LastInst, TBB, Cond);
  //    return false;
  //  }
  //  return true; // Can't handle indirect branch.
  //}

  //// Get the instruction before it if it is a terminator.
  //MachineInstr *SecondLastInst = &*I;
  //unsigned SecondLastOpc = SecondLastInst->getOpcode();

  //// If AllowModify is true and the block ends with two or more unconditional
  //// branches, delete all but the first unconditional branch.
  //if (AllowModify && isUncondBranchOpcode(LastOpc)) {
  //  while (isUncondBranchOpcode(SecondLastOpc)) {
  //    LastInst->eraseFromParent();
  //    LastInst = SecondLastInst;
  //    LastOpc = LastInst->getOpcode();
  //    if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
  //      // Return now the only terminator is an unconditional branch.
  //      TBB = LastInst->getOperand(0).getMBB();
  //      return false;
  //    } else {
  //      SecondLastInst = &*I;
  //      SecondLastOpc = SecondLastInst->getOpcode();
  //    }
  //  }
  //}

  //// If there are three terminators, we don't know what sort of block this is.
  //if (SecondLastInst && I != MBB.begin() && isUnpredicatedTerminator(*--I))
  //  return true;

  //// If the block ends with a B and a Bcc, handle it.
  //if (isCondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
  //  parseCondBranch(SecondLastInst, TBB, Cond);
  //  FBB = LastInst->getOperand(0).getMBB();
  //  return false;
  //}

  //// If the block ends with two unconditional branches, handle it.  The second
  //// one is not executed.
  //if (isUncondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
  //  TBB = SecondLastInst->getOperand(0).getMBB();
  //  return false;
  //}

  //// ...likewise if it ends with an indirect branch followed by an unconditional
  //// branch.
  //if (isIndirectBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
  //  I = LastInst;
  //  if (AllowModify)
  //    I->eraseFromParent();
  //  return true;
  //}

  // Otherwise, can't handle this.
  return true;
}
//
unsigned PS2VPUInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "PS2VPU branch conditions should have one component!");
  assert(!BytesAdded && "code size not handled");

  //if (Cond.empty()) {
  //  assert(!FBB && "Unconditional branch with multiple successors!");
  //  BuildMI(&MBB, DL, get(SP::BA)).addMBB(TBB);
  //  return 1;
  //}

  //// Conditional branch
  //unsigned CC = Cond[0].getImm();

  //if (IsIntegerCC(CC))
  //  BuildMI(&MBB, DL, get(SP::BCOND)).addMBB(TBB).addImm(CC);
  //else
  //  BuildMI(&MBB, DL, get(SP::FBCOND)).addMBB(TBB).addImm(CC);
  //if (!FBB)
  //  return 1;

  //BuildMI(&MBB, DL, get(SP::BA)).addMBB(FBB);
  //return 2;
  return PS2VPUGenInstrInfo::insertBranch(MBB, TBB, FBB, Cond, DL, BytesAdded);
}

unsigned PS2VPUInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                      int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  //MachineBasicBlock::iterator I = MBB.end();
  //unsigned Count = 0;
  //while (I != MBB.begin()) {
  //  --I;

  //  if (I->isDebugInstr())
  //    continue;

  //  if (I->getOpcode() != SP::BA && I->getOpcode() != SP::BCOND &&
  //      I->getOpcode() != SP::FBCOND)
  //    break; // Not a branch

  //  I->eraseFromParent();
  //  I = MBB.end();
  //  ++Count;
  //}
  //return Count;
  return PS2VPUGenInstrInfo::removeBranch(MBB, BytesRemoved);
}

bool PS2VPUInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 1);
  //SPCC::CondCodes CC = static_cast<SPCC::CondCodes>(Cond[0].getImm());
  //Cond[0].setImm(GetOppositeBranchCondition(CC));
  //return false;
  return PS2VPUGenInstrInfo::reverseBranchCondition(Cond);
}

void PS2VPUInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I,
                                 const DebugLoc &DL, MCRegister DestReg,
                                 MCRegister SrcReg, bool KillSrc) const {
  // INT16 <= INT16 
    if (PS2VPUNS::IntRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(PS2VPUNS::IORrr), DestReg)
        .addReg(PS2VPUNS::VI0)
        .addReg(SrcReg, getKillRegState(KillSrc));
  // FP32.xyzw <= FP32.xyzw 
  else if (PS2VPUNS::VFRegsRegClass.contains(DestReg, SrcReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::MOVEv4), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    // ACC.xyzw <= FP32.xyzw 
  } else if (DestReg == PS2VPUNS::ACC &&
             PS2VPUNS::VFRegsRegClass.contains(SrcReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDAbcv4), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc))
        .addReg(PS2VPUNS::VF0x);
    // FP32.xyzw <= I
  } else if (PS2VPUNS::VFRegsRegClass.contains(DestReg) &&
             SrcReg == PS2VPUNS::I) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDiv4), DestReg)
        .addReg(PS2VPUNS::VF0)
        .addReg(SrcReg, getKillRegState(KillSrc));

  // FP32.x/y/z/w <= FP32.x/y/z/w
  } else if (PS2VPUNS::FloatRegsRegClass.contains(DestReg, SrcReg)) {
    if (PS2VPUNS::FloatXRegsRegClass.contains(DestReg, SrcReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::MOVEx), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatYRegsRegClass.contains(DestReg, SrcReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::MOVEy), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatZRegsRegClass.contains(DestReg, SrcReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::MOVEz), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatWRegsRegClass.contains(DestReg, SrcReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::MOVEw), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatXRegsRegClass.contains(DestReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::ADDbcx), DestReg)
          .addReg(PS2VPUNS::VF0x)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatYRegsRegClass.contains(DestReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::ADDbcy), DestReg)
          .addReg(PS2VPUNS::VF0y)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } else if (PS2VPUNS::FloatZRegsRegClass.contains(DestReg)) {
      BuildMI(MBB, I, DL, get(PS2VPUNS::ADDbcz), DestReg)
          .addReg(PS2VPUNS::VF0z)
          .addReg(SrcReg, getKillRegState(KillSrc));
    } //
    // FIXME: dont have FP32.w <= FP32.x/y/z because its difficult.
    // we cannot use ADDbc with VF0 because VF0.w is 1.0.
    // MR32 only works for z
  }
  // ACC.x/y/z/w <= FP32.x/y/z/w
  else if (PS2VPUNS::ACCxRegsRegClass.contains(DestReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDAbcx), DestReg)
        .addReg(PS2VPUNS::VF0x)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else if (PS2VPUNS::ACCyRegsRegClass.contains(DestReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDAbcy), DestReg)
        .addReg(PS2VPUNS::VF0y)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else if (PS2VPUNS::ACCzRegsRegClass.contains(DestReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDAbcz), DestReg)
        .addReg(PS2VPUNS::VF0z)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } /*else if (PS2VPUNS::ACCwRegsRegClass.contains(DestReg)) {
    BuildMI(MBB, I, DL, get(PS2VPUNS::ADDAwbc), DestReg)
        .addReg(PS2VPUNS::VF0w)
        .addReg(SrcReg, getKillRegState(KillSrc));
  }*/
  // FIXME: cannot do ACC.w <= FPR32.x/y/z, need to do reg A.w <= 0, ACC.w <=
  // ADDbc(A.w, SrcReg.x/y/z)
  else
    llvm_unreachable("Impossible reg-to-reg copy");
}

void PS2VPUInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I,
                                          Register SrcReg, bool isKill, int FI,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI,
                                          Register VReg) const {
  //DebugLoc DL;
  //if (I != MBB.end())
  //  DL = I->getDebugLoc();

  //MachineFunction *MF = MBB.getParent();
  //const MachineFrameInfo &MFI = MF->getFrameInfo();
  //MachineMemOperand *MMO = MF->getMachineMemOperand(
  //    MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOStore,
  //    MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  //// On the order of operands here: think "[FrameIdx + 0] = SrcReg".
  //if (RC == &SP::I64RegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::STXri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else if (RC == &SP::IntRegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::STri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else if (RC == &SP::IntPairRegClass)
  //  BuildMI(MBB, I, DL, get(SP::STDri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else if (RC == &SP::FPRegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::STFri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else if (SP::DFPRegsRegClass.hasSubClassEq(RC))
  //  BuildMI(MBB, I, DL, get(SP::STDFri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else if (SP::QFPRegsRegClass.hasSubClassEq(RC))
  //  // Use STQFri irrespective of its legality. If STQ is not legal, it will be
  //  // lowered into two STDs in eliminateFrameIndex.
  //  BuildMI(MBB, I, DL, get(SP::STQFri))
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addReg(SrcReg, getKillRegState(isKill))
  //      .addMemOperand(MMO);
  //else
    llvm_unreachable("Can't store this register to stack slot");
}

void PS2VPUInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator I,
                                           Register DestReg, int FI,
                                           const TargetRegisterClass *RC,
                                           const TargetRegisterInfo *TRI,
                                           Register VReg) const {
  //DebugLoc DL;
  //if (I != MBB.end())
  //  DL = I->getDebugLoc();

  //MachineFunction *MF = MBB.getParent();
  //const MachineFrameInfo &MFI = MF->getFrameInfo();
  //MachineMemOperand *MMO = MF->getMachineMemOperand(
  //    MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOLoad,
  //    MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  //if (RC == &SP::I64RegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::LDXri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else if (RC == &SP::IntRegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::LDri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else if (RC == &SP::IntPairRegClass)
  //  BuildMI(MBB, I, DL, get(SP::LDDri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else if (RC == &SP::FPRegsRegClass)
  //  BuildMI(MBB, I, DL, get(SP::LDFri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else if (SP::DFPRegsRegClass.hasSubClassEq(RC))
  //  BuildMI(MBB, I, DL, get(SP::LDDFri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else if (SP::QFPRegsRegClass.hasSubClassEq(RC))
  //  // Use LDQFri irrespective of its legality. If LDQ is not legal, it will be
  //  // lowered into two LDDs in eliminateFrameIndex.
  //  BuildMI(MBB, I, DL, get(SP::LDQFri), DestReg)
  //      .addFrameIndex(FI)
  //      .addImm(0)
  //      .addMemOperand(MMO);
  //else
    llvm_unreachable("Can't load this register from stack slot");
}

//Register PS2VPUInstrInfo::getGlobalBaseReg(MachineFunction *MF) const {
//  PS2VPUMachineFunctionInfo *PS2VPUFI = MF->getInfo<PS2VPUMachineFunctionInfo>();
//  Register GlobalBaseReg = PS2VPUFI->getGlobalBaseReg();
//  if (GlobalBaseReg)
//    return GlobalBaseReg;
//
//  // Insert the set of GlobalBaseReg into the first MBB of the function
//  MachineBasicBlock &FirstMBB = MF->front();
//  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
//  MachineRegisterInfo &RegInfo = MF->getRegInfo();
//
//  const TargetRegisterClass *PtrRC =
//      Subtarget.is64Bit() ? &SP::I64RegsRegClass : &SP::IntRegsRegClass;
//  GlobalBaseReg = RegInfo.createVirtualRegister(PtrRC);
//
//  DebugLoc dl;
//
//  BuildMI(FirstMBB, MBBI, dl, get(SP::GETPCX), GlobalBaseReg);
//  PS2VPUFI->setGlobalBaseReg(GlobalBaseReg);
//  return GlobalBaseReg;
//}
//

bool PS2VPUInstrInfo::expandILW(MachineInstr &MI) const {
    MachineBasicBlock *MBB = MI.getParent();
    const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    DebugLoc dl = MI.getDebugLoc();

    llvm::Register dst = MI.getOperand(0).getReg();
    llvm::Register base = MI.getOperand(1).getReg();
    int64_t offset = MI.getOperand(2).getImm();

    // only support stack pointer, and assume it is aligned on 16
    if (base != PS2VPUNS::VI6)
        return false;

    int component = std::div(offset, 16ll).rem;
    offset = std::div(offset, 16ll).quot;
    if (component < 0) {
        component += 16;
        --offset;
    }

    switch (component) {
    case 0:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ILWx), dst)
            .addReg(base)
            .addImm(offset);
    break;
    case 4:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ILWy), dst)
            .addReg(base)
            .addImm(offset);
    break;
    case 8:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ILWz), dst)
            .addReg(base)
            .addImm(offset);
    break;
    case 12:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ILWw), dst)
            .addReg(base)
            .addImm(offset);
    break;
    default:
        return false;
    }

    MI.eraseFromParent();
    return true;
}
bool PS2VPUInstrInfo::expandISW(MachineInstr &MI) const {
    MachineBasicBlock *MBB = MI.getParent();
    const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    DebugLoc dl = MI.getDebugLoc();

    llvm::Register src = MI.getOperand(0).getReg();
    llvm::Register base = MI.getOperand(1).getReg();
    int64_t offset = MI.getOperand(2).getImm();

    // only support stack pointer, and assume it is aligned on 16
    if (base != PS2VPUNS::VI6)
        return false;

    int component = std::div(offset, 16ll).rem;
    offset = std::div(offset, 16ll).quot;
    if (component < 0) {
        component += 16;
        --offset;
    }

    switch (component) {
    case 0:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ISWx))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 4:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ISWy))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 8:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ISWz))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 12:
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::ISWw))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    default:
        return false;
    }

    MI.eraseFromParent();
    return true;
}
static unsigned findScratchNonCalleeSaveRegister(MachineBasicBlock *MBB, const TargetRegisterClass* RC) {
    MachineFunction *MF = MBB->getParent();

    const PS2VPUSubtarget &Subtarget = MF->getSubtarget<PS2VPUSubtarget>();
    const PS2VPURegisterInfo &TRI = *Subtarget.getRegisterInfo();
    LivePhysRegs LiveRegs(TRI);
    LiveRegs.addLiveIns(*MBB);

    // Mark callee saved registers as used so we will not choose them.
    const MCPhysReg *CSRegs = MF->getRegInfo().getCalleeSavedRegs();
    for (unsigned i = 0; CSRegs[i]; ++i)
        LiveRegs.addReg(CSRegs[i]);

    // Prefer X9 since it was historically used for the prologue scratch reg.
    const MachineRegisterInfo &MRI = MF->getRegInfo();

    for (unsigned Reg : *RC) {
        if (LiveRegs.available(MRI, Reg))
      return Reg;
    }
    return PS2VPUNS::NoRegister;
}
bool PS2VPUInstrInfo::expandSQ(MachineInstr &MI) const {
    MachineBasicBlock *MBB = MI.getParent();
    const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    DebugLoc dl = MI.getDebugLoc();

    llvm::Register src = MI.getOperand(0).getReg();
    llvm::Register base = MI.getOperand(1).getReg();
    int64_t offset = MI.getOperand(2).getImm();

    // only support stack pointer, and assume it is aligned on 16
    if (base != PS2VPUNS::VI6)
        return false;

    int component = std::div(offset, 16ll).rem;
    offset = std::div(offset, 16ll).quot;
    if (component < 0)
    {
        component += 16;
        --offset;
    }

    switch (component) {
    case 0:
        if (!PS2VPUNS::FloatXRegsRegClass.contains(src)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(MBB, &PS2VPUNS::FloatXRegsRegClass);
            copyPhysReg(*MBB, MI.getIterator(), dl, scratch, src, MI.getOperand(0).isKill());
            src = scratch;
        }
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::SQx))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 4:
        if (!PS2VPUNS::FloatYRegsRegClass.contains(src)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatYRegsRegClass);
            copyPhysReg(*MBB, MI.getIterator(), dl, scratch, src,
                        MI.getOperand(0).isKill());
            src = scratch;
        }
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::SQy))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 8:
        if (!PS2VPUNS::FloatZRegsRegClass.contains(src)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatZRegsRegClass);
            copyPhysReg(*MBB, MI.getIterator(), dl, scratch, src,
                        MI.getOperand(0).isKill());
            src = scratch;
        }
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::SQz))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    case 12:
        if (!PS2VPUNS::FloatWRegsRegClass.contains(src)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatWRegsRegClass);
            copyPhysReg(*MBB, MI.getIterator(), dl, scratch, src,
                        MI.getOperand(0).isKill());
            src = scratch;
        }
        BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::SQw))
            .addReg(src)
            .addReg(base)
            .addImm(offset);
        break;
    default:
        return false;
    }

    MI.eraseFromParent();
    return true;
}
bool PS2VPUInstrInfo::expandLQ(MachineInstr &MI) const {
    MachineBasicBlock *MBB = MI.getParent();
    const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    DebugLoc dl = MI.getDebugLoc();

    llvm::Register dst = MI.getOperand(0).getReg();
    llvm::Register base = MI.getOperand(1).getReg();
    int64_t offset = MI.getOperand(2).getImm();

    // only support stack pointer, and assume it is aligned on 16
    if (base != PS2VPUNS::VI6)
        return false;

    int component = std::div(offset, 16ll).rem;
    offset = std::div(offset, 16ll).quot;
    if (component < 0) {
        component += 16;
        --offset;
    }

    switch (component) {
    case 0: {
        if (!PS2VPUNS::FloatXRegsRegClass.contains(dst)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatXRegsRegClass);
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQx), scratch)
                .addReg(base)
                .addImm(offset);
            copyPhysReg(*MBB, ++MI.getIterator(), dl, dst, scratch,
                        MI.getOperand(0).isKill());
        } else {
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQx), dst)
                .addReg(base)
                .addImm(offset);
        }
        break;
    }
    case 4:
        if (!PS2VPUNS::FloatYRegsRegClass.contains(dst)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatYRegsRegClass);
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQy), scratch)
                .addReg(base)
                .addImm(offset);
            copyPhysReg(*MBB, ++MI.getIterator(), dl, dst, scratch,
                        MI.getOperand(0).isKill());
        } else {
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQy), dst)
                .addReg(base)
                .addImm(offset);
        }
        break;
    case 8:
        if (!PS2VPUNS::FloatZRegsRegClass.contains(dst)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatZRegsRegClass);
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQz), scratch)
                .addReg(base)
                .addImm(offset);
            copyPhysReg(*MBB, ++MI.getIterator(), dl, dst, scratch,
                        MI.getOperand(0).isKill());
        } else {
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQz), dst)
                .addReg(base)
                .addImm(offset);
        }
        break;
    case 12:
        if (!PS2VPUNS::FloatWRegsRegClass.contains(dst)) {
            unsigned scratch = findScratchNonCalleeSaveRegister(
                MBB, &PS2VPUNS::FloatWRegsRegClass);
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQw), scratch)
                .addReg(base)
                .addImm(offset);
            copyPhysReg(*MBB, ++MI.getIterator(), dl, dst, scratch,
                        MI.getOperand(0).isKill());
        } else {
            BuildMI(*MBB, MI.getIterator(), dl, TII.get(PS2VPUNS::LQw), dst)
                .addReg(base)
                .addImm(offset);
        }
        break;
    default:
        return false;
    }

    MI.eraseFromParent();
    return true;
}

bool PS2VPUInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
    switch (MI.getOpcode()) {
    case PS2VPUNS::ILW_PSEUDO:
        return expandILW(MI);
    case PS2VPUNS::ISW_PSEUDO:
        return expandISW(MI);
    case PS2VPUNS::LQ_PSEUDO:
        return expandLQ(MI);
    case PS2VPUNS::SQ_PSEUDO:
        return expandSQ(MI);
  //case TargetOpcode::LOAD_STACK_GUARD: {
  //  assert(Subtarget.isTargetLinux() &&
  //         "Only Linux target is expected to contain LOAD_STACK_GUARD");
  //  // offsetof(tcbhead_t, stack_guard) from sysdeps/PS2VPU/nptl/tls.h in glibc.
  //  const int64_t Offset = Subtarget.is64Bit() ? 0x28 : 0x14;
  //  MI.setDesc(get(Subtarget.is64Bit() ? SP::LDXri : SP::LDri));
  //  MachineInstrBuilder(*MI.getParent()->getParent(), MI)
  //      .addReg(SP::G7)
  //      .addImm(Offset);
  //  return true;
  //}
  }
  return false;
}
DFAPacketizer *PS2VPUInstrInfo::CreateTargetScheduleState(
    const TargetSubtargetInfo &STI) const {
  const InstrItineraryData *II = STI.getInstrItineraryData();
  return static_cast<const PS2VPUSubtarget &>(STI).createDFAPacketizer(II);
}