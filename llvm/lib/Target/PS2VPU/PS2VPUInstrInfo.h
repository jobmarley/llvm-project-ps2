
#ifndef LLVM_LIB_TARGET_PS2VPU_PS2VPUINSTRINFO_H
#define LLVM_LIB_TARGET_PS2VPU_PS2VPUINSTRINFO_H

#include "PS2VPURegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "PS2VPUGenInstrInfo.inc"

namespace llvm {

class PS2VPUSubtarget;

/// SPII - This namespace holds all of the target specific flags that
/// instruction info tracks.
///
//namespace SPII {
//enum {
//  Pseudo = (1 << 0),
//  Load = (1 << 1),
//  Store = (1 << 2),
//  DelaySlot = (1 << 3)
//};
//}

class PS2VPUInstrInfo : public PS2VPUGenInstrInfo {
  const PS2VPURegisterInfo RI;
  const PS2VPUSubtarget &Subtarget;
  virtual void anchor();

public:
  explicit PS2VPUInstrInfo(PS2VPUSubtarget &ST);

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  ///
  const PS2VPURegisterInfo &getRegisterInfo() const { return RI; }

  /// isLoadFromStackSlot - If the specified machine instruction is a direct
  /// load from a stack slot, return the virtual or physical register number of
  /// the destination along with the FrameIndex of the loaded stack slot.  If
  /// not, return 0.  This predicate must return 0 if the instruction has
  /// any side effects other than loading from the stack slot.
  unsigned isLoadFromStackSlot(const MachineInstr &MI,
                               int &FrameIndex) const override;

  ///// isStoreToStackSlot - If the specified machine instruction is a direct
  ///// store to a stack slot, return the virtual or physical register number of
  ///// the source reg along with the FrameIndex of the loaded stack slot.  If
  ///// not, return 0.  This predicate must return 0 if the instruction has
  ///// any side effects other than storing to the stack slot.
  unsigned isStoreToStackSlot(const MachineInstr &MI,
                              int &FrameIndex) const override;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify = false) const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;

  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                   const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg,
                   bool KillSrc) const override;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI, Register SrcReg,
                           bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI,
                           Register VReg) const override;

  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MBBI, Register DestReg,
                            int FrameIndex, const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI,
                            Register VReg) const override;

  //Register getGlobalBaseReg(MachineFunction *MF) const;

  //// Lower pseudo instructions after register allocation.
  bool expandPostRAPseudo(MachineInstr &MI) const override;

  DFAPacketizer *
  CreateTargetScheduleState(const TargetSubtargetInfo &STI) const override;
};

} // namespace llvm

#endif
