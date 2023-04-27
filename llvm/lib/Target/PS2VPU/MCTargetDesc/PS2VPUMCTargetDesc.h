//===-- PS2VPUMCTargetDesc.h - PS2VPU Target Descriptions ---------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCTARGETDESC_H
#define LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class Target;

MCCodeEmitter *createPS2VPUMCCodeEmitter(const MCInstrInfo &MCII,
                                        MCContext &Ctx);
MCAsmBackend *createPS2VPUAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                    const MCRegisterInfo &MRI,
                                    const MCTargetOptions &Options);
std::unique_ptr<MCObjectTargetWriter> createPS2VPUELFObjectWriter(bool Is64Bit,
                                                                 uint8_t OSABI);
} // namespace llvm

// Defines symbolic names for PS2VPU registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "PS2VPUGenRegisterInfo.inc"

// Defines symbolic names for the PS2VPU instructions.
//
#define GET_INSTRINFO_ENUM
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "PS2VPUGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "PS2VPUGenSubtargetInfo.inc"

#endif
