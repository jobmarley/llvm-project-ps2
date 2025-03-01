//===- PS2VPUMachineFunctionInfo.h - PS2VPU Machine Function Info -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares  PS2VPU specific per-machine-function information.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_PS2VPU_PS2VPUMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_PS2VPU_PS2VPUMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

class PS2VPUMachineFunctionInfo : public MachineFunctionInfo {
  virtual void anchor();

private:
  Register GlobalBaseReg;

  /// VarArgsFrameOffset - Frame offset to start of varargs area.
  int VarArgsFrameOffset;

  /// SRetReturnReg - Holds the virtual register into which the sret
  /// argument is passed.
  Register SRetReturnReg;

  /// IsLeafProc - True if the function is a leaf procedure.
  bool IsLeafProc;

public:
  PS2VPUMachineFunctionInfo()
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}
  PS2VPUMachineFunctionInfo(const Function &F, const TargetSubtargetInfo *STI)
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}

  MachineFunctionInfo *
  clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
        const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
      const override;

  Register getGlobalBaseReg() const { return GlobalBaseReg; }
  void setGlobalBaseReg(Register Reg) { GlobalBaseReg = Reg; }

  int getVarArgsFrameOffset() const { return VarArgsFrameOffset; }
  void setVarArgsFrameOffset(int Offset) { VarArgsFrameOffset = Offset; }

  Register getSRetReturnReg() const { return SRetReturnReg; }
  void setSRetReturnReg(Register Reg) { SRetReturnReg = Reg; }

  void setLeafProc(bool rhs) { IsLeafProc = rhs; }
  bool isLeafProc() const { return IsLeafProc; }
};
} // namespace llvm

#endif
