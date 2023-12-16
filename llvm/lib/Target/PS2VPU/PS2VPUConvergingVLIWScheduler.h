//===- PS2VPUMachineScheduler.h - Custom PS2VPU MI scheduler --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Custom PS2VPU MI scheduler.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_PS2VPUMACHINESCHEDULER_H
#define LLVM_LIB_TARGET_PS2VPU_PS2VPUMACHINESCHEDULER_H

#include "llvm/CodeGen/MachineScheduler.h"
#include "llvm/CodeGen/RegisterPressure.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/VLIWMachineScheduler.h"

namespace llvm {

class SUnit;

class PS2VPUVLIWResourceModel : public VLIWResourceModel {
public:
  using VLIWResourceModel::VLIWResourceModel;
  bool hasDependence(const SUnit *SUd, const SUnit *SUu) override;
};

class PS2VPUConvergingVLIWScheduler : public ConvergingVLIWScheduler {
protected:
  VLIWResourceModel *
  createVLIWResourceModel(const TargetSubtargetInfo &STI,
                          const TargetSchedModel *SchedModel) const override;
  int SchedulingCost(ReadyQueue &Q, SUnit *SU, SchedCandidate &Candidate,
                     RegPressureDelta &Delta, bool verbose) override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_PS2VPU_PS2VPUMACHINESCHEDULER_H
