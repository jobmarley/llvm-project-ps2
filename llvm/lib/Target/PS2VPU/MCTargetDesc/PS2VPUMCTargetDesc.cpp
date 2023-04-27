//===-- PS2VPUMCTargetDesc.cpp - PS2VPU Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides PS2VPU specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "PS2VPUInstPrinter.h"
#include "PS2VPUMCAsmInfo.h"
#include "PS2VPUMCTargetDesc.h"
#include "PS2VPUTargetStreamer.h"
#include "TargetInfo/PS2VPUTargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/MC/MCDwarf.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "PS2VPUGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "PS2VPUGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "PS2VPUGenRegisterInfo.inc"

static MCAsmInfo *createPS2VPUMCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT,
                                       const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new PS2VPUELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(PS2VPUNS::VI6, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

//static MCAsmInfo *createPS2VPUV9MCAsmInfo(const MCRegisterInfo &MRI,
//                                         const Triple &TT,
//                                         const MCTargetOptions &Options) {
//  MCAsmInfo *MAI = new PS2VPUELFMCAsmInfo(TT);
//  unsigned Reg = MRI.getDwarfRegNum(SP::O6, true);
//  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 2047);
//  MAI->addInitialFrameState(Inst);
//  return MAI;
//}

static MCInstrInfo *createPS2VPUMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitPS2VPUMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createPS2VPUMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitPS2VPUMCRegisterInfo(X, PS2VPUNS::VI7);
  return X;
}

static MCSubtargetInfo *
createPS2VPUMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  //if (CPU.empty())
  //  CPU = (TT.getArch() == Triple::ps2vpu) ? "v9" : "v8";
  return createPS2VPUMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new PS2VPUTargetELFStreamer(S);
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new PS2VPUTargetAsmStreamer(S, OS);
}

static MCInstPrinter *createPS2VPUMCInstPrinter(const Triple &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new PS2VPUInstPrinter(MAI, MII, MRI);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePS2VPUTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(getThePS2VPUTarget(), createPS2VPUMCAsmInfo);
  //RegisterMCAsmInfoFn Y(getThePS2VPUV9Target(), createPS2VPUV9MCAsmInfo);
  //RegisterMCAsmInfoFn Z(getThePS2VPUelTarget(), createPS2VPUMCAsmInfo);

  for (Target *T :
       {&getThePS2VPUTarget()}) {
    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createPS2VPUMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createPS2VPUMCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createPS2VPUMCSubtargetInfo);

    // Register the MC Code Emitter.
    TargetRegistry::RegisterMCCodeEmitter(*T, createPS2VPUMCCodeEmitter);

    // Register the asm backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createPS2VPUAsmBackend);

    // Register the object target streamer.
    TargetRegistry::RegisterObjectTargetStreamer(*T,
                                                 createObjectTargetStreamer);

    //// Register the asm streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createTargetAsmStreamer);

    //// Register the MCInstPrinter
    TargetRegistry::RegisterMCInstPrinter(*T, createPS2VPUMCInstPrinter);
  }
}
