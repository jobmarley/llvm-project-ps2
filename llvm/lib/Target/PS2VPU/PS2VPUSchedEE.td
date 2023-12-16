//=- PS2VPUSchedA57.td - ARM Cortex-A57 Scheduling Defs -----*- tablegen -*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the machine model for ARM Cortex-A57 to support
// instruction scheduling and other instruction cost heuristics.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// The Cortex-A57 is a traditional superscalar microprocessor with a
// conservative 3-wide in-order stage for decode and dispatch. Combined with the
// much wider out-of-order issue stage, this produced a need to carefully
// schedule micro-ops so that all three decoded each cycle are successfully
// issued as the reservation station(s) simply don't stay occupied for long.
// Therefore, IssueWidth is set to the narrower of the two at three, while still
// modeling the machine as out-of-order.

def PS2VPUEEModel : SchedMachineModel {
  let IssueWidth        =   1;
  let MicroOpBufferSize = 0; 
  let LoadLatency       =   4; // Optimistic load latency
  let MispredictPenalty =  4; // Fetch + Decode/Rename/Dispatch + Branch
  
  let Itineraries = EEVU_Itineraries;
  // Enable partial & runtime unrolling. The magic number is chosen based on
  // experiments and benchmarking data.
  let LoopMicroOpBufferSize = 0;
  let CompleteModel = 0;            // in dev, dont report missing scheduling for instr
  
  //list<Predicate> UnsupportedFeatures = !listconcat(SVEUnsupported.F,
  //                                                  PAUnsupported.F,
  //                                                  SMEUnsupported.F,
  //                                                  [HasMTE]);
}

////===----------------------------------------------------------------------===//
//// Define each kind of processor resource and number available on the Emotion engine VU.
//
//def EEVUUnitFMAC : ProcResource<1>;
//def EEVUUnitFDIV : ProcResource<1>;
//def EEVUUnitLSU : ProcResource<1>;
//def EEVUUnitIALU : ProcResource<1>;
//def EEVUUnitBRU : ProcResource<1>;
//def EEVUUnitRANDU : ProcResource<1>;
//def EEVUUnitEFU : ProcResource<1>;
//
//
//
//
//let SchedModel = PS2VPUEEModel in {
//
//def EEVU_4cycle_FMAC  : SchedWriteRes<[EEVUUnitFMAC]> { let Latency = 4;  }
//def EEVU_7cycle_FDIV  : SchedWriteRes<[EEVUUnitFDIV]> { let Latency = 7;
//                                                        let ReleaseAtCycles = [7]; }
//def EEVU_4cycle_LSU  : SchedWriteRes<[EEVUUnitLSU]> { let Latency = 4;  }
//def EEVU_1cycle_IALU  : SchedWriteRes<[EEVUUnitIALU]> { let Latency = 1;  }
//def EEVU_2cycle_BRU  : SchedWriteRes<[EEVUUnitBRU]> { let Latency = 2;  }
//def EEVU_4cycle_RANDU  : SchedWriteRes<[EEVUUnitRANDU]> { let Latency = 4;  }
//def EEVU_44cycle_EFU  : SchedWriteRes<[EEVUUnitEFU]> { let Latency = 44;  
//                                                        let ReleaseAtCycles = [43]; }
//def EEVU_29cycle_EFU  : SchedWriteRes<[EEVUUnitEFU]> { let Latency = 29;  
//                                                        let ReleaseAtCycles = [28]; }
//
//  def : WriteRes<WriteFPReg, [EEVUUnitFMAC]> { let Latency = 4; }
//  def : WriteRes<WriteIReg, [EEVUUnitIALU]> { let Latency = 1; }
//  def : WriteRes<WriteLS, [EEVUUnitLSU]> { let Latency = 4; }
//  def : WriteRes<WriteRand, [EEVUUnitRANDU]> { let Latency = 4; }
//  def : WriteRes<WriteWaitQ, [EEVUUnitFDIV]> { let Latency = 0; }
//  def : WriteRes<WriteQ, [EEVUUnitFDIV]> { let Latency = 7;
//											let ReleaseAtCycles = [7]; }
//  def : WriteRes<WriteWaitP, [EEVUUnitEFU]> { let Latency = 0; }
//  def : WriteRes<WriteEATAN, [EEVUUnitEFU]> { let Latency = 53;
//											let ReleaseAtCycles = [54]; }
//  def : WriteRes<WriteEEXP, [EEVUUnitEFU]> { let Latency = 43;
//											let ReleaseAtCycles = [44]; }
//  def : WriteRes<WriteERCPR, [EEVUUnitEFU]> { let Latency = 11;
//											let ReleaseAtCycles = [12]; }
//  def : WriteRes<WriteERSQRT, [EEVUUnitEFU]> { let Latency = 17;
//											let ReleaseAtCycles = [18]; }
//  def : WriteRes<WriteESIN, [EEVUUnitEFU]> { let Latency = 28;
//											let ReleaseAtCycles = [29]; }
//  def : WriteRes<WriteESIN, [EEVUUnitEFU]> { let Latency = 28;
//											let ReleaseAtCycles = [29]; }
//  def : WriteRes<WriteBR, [EEVUUnitBRU]> { let Latency = 2;
//											let ReleaseAtCycles = [2]; }
//}