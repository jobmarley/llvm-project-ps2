//==-- PS2VPUSchedule.td - PS2VPU Scheduling Definitions -*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Define TII for use in SchedVariant Predicates.
// const MachineInstr *MI and const TargetSchedModel *SchedModel
// are defined by default.
def : PredicateProlog<[{
  const PS2VPUInstrInfo *TII =
    static_cast<const PS2VPUInstrInfo*>(SchedModel->getInstrInfo());
  (void)TII;
}]>;

// PS2VPU Scheduler Definitions
//def WriteFPReg     : SchedWrite; // FMAC
////def ReadFPReg      : SchedRead;  // FMAC
//def WriteIReg      : SchedWrite;  // IALU
//def WriteLS        : SchedWrite;  // LSU
//def WriteRand      : SchedWrite;  // RANDU
//def WriteWaitQ     : SchedWrite;  // FDIV
//def WriteQ         : SchedWrite;  // FDIV
//def WriteWaitP     : SchedWrite;  // EFU
//def WriteEATAN     : SchedWrite;  // EFU
//def WriteEEXP      : SchedWrite;  // EFU
//def WriteERCPR     : SchedWrite;  // EFU
//def WriteERSQRT    : SchedWrite;  // EFU (and SQRT and ESUM)
//def WriteESIN      : SchedWrite;  // EFU
//def WriteBR        : SchedWrite;  // BRU

def IIC_fmac_instr : InstrItinClass;
def IIC_ialu_instr : InstrItinClass;
def IIC_ldst_instr : InstrItinClass;
def IIC_branch_instr : InstrItinClass;
def IIC_loi_instr : InstrItinClass;


def VU_FMAC : FuncUnit;
def VU_FDIV : FuncUnit;
def VU_LSU : FuncUnit;
def VU_IALU : FuncUnit;
def VU_BRU : FuncUnit;
def VU_RANDU : FuncUnit;
def VU_EFU : FuncUnit;
def VU_NOP : FuncUnit;

def EEVU_Itineraries : ProcessorItineraries<
[VU_FMAC, VU_FDIV, VU_LSU, VU_IALU, VU_BRU, VU_RANDU, VU_EFU, VU_NOP], [], [
  InstrItinData<IIC_fmac_instr, [InstrStage<1, [VU_FMAC]>], [4, 1]>,
  InstrItinData<IIC_ialu_instr, [InstrStage<1, [VU_IALU]>], [1, 1]>,
  InstrItinData<IIC_ldst_instr, [InstrStage<1, [VU_LSU]>], [4, 1]>,
  InstrItinData<IIC_branch_instr, [InstrStage<1, [VU_BRU]>], [2, 1]>,
  InstrItinData<IIC_loi_instr, [InstrStage<1, [VU_NOP]>], [1, 1]>,

]>;