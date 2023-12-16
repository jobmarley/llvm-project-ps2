//===- PS2VPUMachineScheduler.cpp - MI Scheduler for PS2VPU -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// MachineScheduler schedules machine instructions after phi elimination. It
// preserves LiveIntervals so it can be invoked before register allocation.
//
//===----------------------------------------------------------------------===//

#include "PS2VPUInstrInfo.h"
#include "PS2VPUConvergingVLIWScheduler.h"
#include "PS2VPUSubtarget.h"
#include "llvm/CodeGen/MachineScheduler.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include "llvm/CodeGen/VLIWMachineScheduler.h"

using namespace llvm;

#define DEBUG_TYPE "machine-scheduler"

/// Return true if there is a dependence between SUd and SUu.
bool PS2VPUVLIWResourceModel::hasDependence(const SUnit *SUd,
                                             const SUnit *SUu) {
  const auto *QII = static_cast<const PS2VPUInstrInfo *>(TII);

  // Enable .cur formation.
  //if (QII->mayBeCurLoad(*SUd->getInstr()))
  //  return false;

  //if (QII->canExecuteInBundle(*SUd->getInstr(), *SUu->getInstr()))
  //  return false;

  return VLIWResourceModel::hasDependence(SUd, SUu);
}

VLIWResourceModel *PS2VPUConvergingVLIWScheduler::createVLIWResourceModel(
    const TargetSubtargetInfo &STI, const TargetSchedModel *SchedModel) const {
  return new PS2VPUVLIWResourceModel(STI, SchedModel);
}

int PS2VPUConvergingVLIWScheduler::SchedulingCost(ReadyQueue &Q, SUnit *SU,
                                                   SchedCandidate &Candidate,
                                                   RegPressureDelta &Delta,
                                                   bool verbose) {
  int ResCount =
      ConvergingVLIWScheduler::SchedulingCost(Q, SU, Candidate, Delta, verbose);

  if (!SU || SU->isScheduled)
    return ResCount;

  auto &QST = DAG->MF.getSubtarget<PS2VPUSubtarget>();
  auto &QII = *QST.getInstrInfo();
  /*if (SU->isInstr() && QII.mayBeCurLoad(*SU->getInstr())) {
    if (Q.getID() == TopQID &&
        Top.ResourceModel->isResourceAvailable(SU, true)) {
      ResCount += PriorityTwo;
      LLVM_DEBUG(if (verbose) dbgs() << "C|");
    } else if (Q.getID() == BotQID &&
               Bot.ResourceModel->isResourceAvailable(SU, false)) {
      ResCount += PriorityTwo;
      LLVM_DEBUG(if (verbose) dbgs() << "C|");
    }
  }*/

  return ResCount;
}
