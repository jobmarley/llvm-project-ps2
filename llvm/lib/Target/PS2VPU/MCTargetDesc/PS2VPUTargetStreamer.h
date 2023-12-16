//===-- PS2VPUTargetStreamer.h - PS2VPU Target Streamer ----------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUTARGETSTREAMER_H
#define LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUTARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

class formatted_raw_ostream;

class PS2VPUTargetStreamer : public MCTargetStreamer {
  virtual void anchor();

public:
  PS2VPUTargetStreamer(MCStreamer &S);
  /// Emit ".register <reg>, #ignore".
  virtual void emitPS2VPURegisterIgnore(unsigned reg) = 0;
  /// Emit ".register <reg>, #scratch".
  virtual void emitPS2VPURegisterScratch(unsigned reg) = 0;
};

// This part is for ascii assembly output
class PS2VPUTargetAsmStreamer : public PS2VPUTargetStreamer {
  formatted_raw_ostream &OS;

public:
  PS2VPUTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
  void emitPS2VPURegisterIgnore(unsigned reg) override;
  void emitPS2VPURegisterScratch(unsigned reg) override;
  void prettyPrintAsm(MCInstPrinter &InstPrinter, uint64_t Address,
                              const MCInst &Inst, const MCSubtargetInfo &STI,
                              raw_ostream &OS) override;
};

// This part is for ELF object output
class PS2VPUTargetELFStreamer : public PS2VPUTargetStreamer {
public:
  PS2VPUTargetELFStreamer(MCStreamer &S);
  MCELFStreamer &getStreamer();
  void emitPS2VPURegisterIgnore(unsigned reg) override {}
  void emitPS2VPURegisterScratch(unsigned reg) override {}
};
} // end namespace llvm

#endif
