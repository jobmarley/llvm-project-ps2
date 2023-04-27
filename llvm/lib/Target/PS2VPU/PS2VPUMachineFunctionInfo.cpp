//===-- PS2VPUMachineFunctionInfo.cpp - PS2VPU Machine Function Info --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "PS2VPUMachineFunctionInfo.h"

using namespace llvm;

void PS2VPUMachineFunctionInfo::anchor() {}

MachineFunctionInfo *PS2VPUMachineFunctionInfo::clone(
    BumpPtrAllocator &Allocator, MachineFunction &DestMF,
    const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
    const {
  return DestMF.cloneInfo<PS2VPUMachineFunctionInfo>(*this);
}
