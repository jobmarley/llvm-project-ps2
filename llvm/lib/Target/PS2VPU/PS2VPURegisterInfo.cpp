
#include "PS2VPU.h"
#include "PS2VPUMachineFunctionInfo.h"
#include "PS2VPURegisterInfo.h"
#include "PS2VPUSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "PS2VPUGenRegisterInfo.inc"

static cl::opt<bool>
    ReserveAppRegisters("PS2VPU-reserve-app-registers", cl::Hidden,
                        cl::init(false),
                        cl::desc("Reserve application registers (%VI2-%VI4)"));

PS2VPURegisterInfo::PS2VPURegisterInfo() : PS2VPUGenRegisterInfo(PS2VPUNS::VI15) {}

const MCPhysReg *
PS2VPURegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

const uint32_t *
PS2VPURegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                        CallingConv::ID CC) const {
  return CSR_RegMask;
}

const uint32_t *
PS2VPURegisterInfo::getRTCallPreservedMask(CallingConv::ID CC) const {
  return RTCSR_RegMask;
}

BitVector PS2VPURegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const PS2VPUSubtarget &Subtarget = MF.getSubtarget<PS2VPUSubtarget>();
   // FIXME: G1 reserved for now for large imm generation by frame code.
  markSuperRegs(Reserved, PS2VPUNS::VF0);
  markSuperRegs(Reserved, PS2VPUNS::VI0);
  Reserved.set(PS2VPUNS::VF0x);
  Reserved.set(PS2VPUNS::VF0y);
  Reserved.set(PS2VPUNS::VF0z);
  Reserved.set(PS2VPUNS::VF0w);
  Reserved.set(PS2VPUNS::VI6);
  Reserved.set(PS2VPUNS::VI15);

   // G1-G4 can be used in applications.
   if (ReserveAppRegisters) {
     Reserved.set(PS2VPUNS::VI2);
     Reserved.set(PS2VPUNS::VI3);
     Reserved.set(PS2VPUNS::VI4);
   }
   
  // FIXME: G1 reserved for now for large imm generation by frame code.
  //Reserved.set(SP::G1);

  //// G1-G4 can be used in applications.
  //if (ReserveAppRegisters) {
  //  Reserved.set(SP::G2);
  //  Reserved.set(SP::G3);
  //  Reserved.set(SP::G4);
  //}
  //// G5 is not reserved in 64 bit mode.
  //if (!Subtarget.is64Bit())
  //  Reserved.set(SP::G5);

  //Reserved.set(SP::O6);
  //Reserved.set(SP::I6);
  //Reserved.set(SP::I7);
  //Reserved.set(SP::G0);
  //Reserved.set(SP::G6);
  //Reserved.set(SP::G7);

  //// Also reserve the register pair aliases covering the above
  //// registers, with the same conditions.
  //Reserved.set(SP::G0_G1);
  //if (ReserveAppRegisters)
  //  Reserved.set(SP::G2_G3);
  //if (ReserveAppRegisters || !Subtarget.is64Bit())
  //  Reserved.set(SP::G4_G5);

  //Reserved.set(SP::O6_O7);
  //Reserved.set(SP::I6_I7);
  //Reserved.set(SP::G6_G7);

  //// Unaliased double registers are not available in non-V9 targets.
  //if (!Subtarget.isV9()) {
  //  for (unsigned n = 0; n != 16; ++n) {
  //    for (MCRegAliasIterator AI(SP::D16 + n, this, true); AI.isValid(); ++AI)
  //      Reserved.set(*AI);
  //  }
  //}

  //// Reserve ASR1-ASR31
  //for (unsigned n = 0; n < 31; n++)
  //  Reserved.set(SP::ASR1 + n);

  return Reserved;
}

const TargetRegisterClass *
PS2VPURegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const {
  const PS2VPUSubtarget &Subtarget = MF.getSubtarget<PS2VPUSubtarget>();
  return &PS2VPUNS::IntRegsRegClass;
}

static void replaceFI(MachineFunction &MF, MachineBasicBlock::iterator II,
                      MachineInstr &MI, const DebugLoc &dl,
                      unsigned FIOperandNum, int Offset, unsigned FramePtr) {
  // Replace frame index with a frame pointer reference.
  if (Offset >= -4096 && Offset <= 4095) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(FIOperandNum).ChangeToRegister(FramePtr, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  // FIXME: it would be better to scavenge a register here instead of
  // reserving G1 all of the time.
  //if (Offset >= 0) {
  //  // Emit nonnegaive immediates with sethi + or.
  //  // sethi %hi(Offset), %g1
  //  // add %g1, %fp, %g1
  //  // Insert G1+%lo(offset) into the user.
  //  BuildMI(*MI.getParent(), II, dl, TII.get(SP::SETHIi), SP::G1)
  //      .addImm(HI22(Offset));

  //  // Emit G1 = G1 + I6
  //  BuildMI(*MI.getParent(), II, dl, TII.get(SP::ADDrr), SP::G1)
  //      .addReg(SP::G1)
  //      .addReg(FramePtr);
  //  // Insert: G1+%lo(offset) into the user.
  //  MI.getOperand(FIOperandNum).ChangeToRegister(SP::G1, false);
  //  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(LO10(Offset));
  //  return;
  //}

  //// Emit Negative numbers with sethi + xor
  //// sethi %hix(Offset), %g1
  //// xor  %g1, %lox(offset), %g1
  //// add %g1, %fp, %g1
  //// Insert: G1 + 0 into the user.
  //BuildMI(*MI.getParent(), II, dl, TII.get(SP::SETHIi), SP::G1)
  //    .addImm(HIX22(Offset));
  //BuildMI(*MI.getParent(), II, dl, TII.get(SP::XORri), SP::G1)
  //    .addReg(SP::G1)
  //    .addImm(LOX10(Offset));

  //BuildMI(*MI.getParent(), II, dl, TII.get(SP::ADDrr), SP::G1)
  //    .addReg(SP::G1)
  //    .addReg(FramePtr);
  //// Insert: G1+%lo(offset) into the user.
  //MI.getOperand(FIOperandNum).ChangeToRegister(SP::G1, false);
  //MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
}

bool PS2VPURegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                            int SPAdj, unsigned FIOperandNum,
                                            RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const PS2VPUSubtarget &Subtarget = MF.getSubtarget<PS2VPUSubtarget>();
  const PS2VPUFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  /*if (!Subtarget.isV9() || !Subtarget.hasHardQuad()) {
    if (MI.getOpcode() == SP::STQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register SrcReg = MI.getOperand(2).getReg();
      Register SrcEvenReg = getSubReg(SrcReg, SP::sub_even64);
      Register SrcOddReg = getSubReg(SrcReg, SP::sub_odd64);
      MachineInstr *StMI = BuildMI(*MI.getParent(), II, dl, TII.get(SP::STDFri))
                               .addReg(FrameReg)
                               .addImm(0)
                               .addReg(SrcEvenReg);
      replaceFI(MF, *StMI, *StMI, dl, 0, Offset, FrameReg);
      MI.setDesc(TII.get(SP::STDFri));
      MI.getOperand(2).setReg(SrcOddReg);
      Offset += 8;
    } else if (MI.getOpcode() == SP::LDQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register DestReg = MI.getOperand(0).getReg();
      Register DestEvenReg = getSubReg(DestReg, SP::sub_even64);
      Register DestOddReg = getSubReg(DestReg, SP::sub_odd64);
      MachineInstr *LdMI =
          BuildMI(*MI.getParent(), II, dl, TII.get(SP::LDDFri), DestEvenReg)
              .addReg(FrameReg)
              .addImm(0);
      replaceFI(MF, *LdMI, *LdMI, dl, 1, Offset, FrameReg);

      MI.setDesc(TII.get(SP::LDDFri));
      MI.getOperand(0).setReg(DestOddReg);
      Offset += 8;
    }
  }*/

  replaceFI(MF, II, MI, dl, FIOperandNum, Offset, FrameReg);
  // replaceFI never removes II
  return false;
}

Register PS2VPURegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return PS2VPUNS::VI6;
}

// PS2VPU has no architectural need for stack realignment support,
// except that LLVM unfortunately currently implements overaligned
// stack objects by depending upon stack realignment support.
// If that ever changes, this can probably be deleted.
bool PS2VPURegisterInfo::canRealignStack(const MachineFunction &MF) const {
  //if (!TargetRegisterInfo::canRealignStack(MF))
  //  return false;

  //// PS2VPU always has a fixed frame pointer register, so don't need to
  //// worry about needing to reserve it. [even if we don't have a frame
  //// pointer for our frame, it still cannot be used for other things,
  //// or register window traps will be SADNESS.]

  //// If there's a reserved call frame, we can use SP to access locals.
  //if (getFrameLowering(MF)->hasReservedCallFrame(MF))
  //  return true;

  //// Otherwise, we'd need a base pointer, but those aren't implemented
  //// for PS2VPU at the moment.

  return false;
}
