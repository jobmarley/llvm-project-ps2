//===- PS2VPUMCAsmInfo.cpp - PS2VPU asm properties --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the PS2VPUMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "PS2VPUMCAsmInfo.h"
#include "PS2VPUMCExpr.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/TargetParser/Triple.h"

using namespace llvm;

void PS2VPUELFMCAsmInfo::anchor() {}

PS2VPUELFMCAsmInfo::PS2VPUELFMCAsmInfo(const Triple &TheTriple) {
  Data16bitsDirective = "\t.half\t";
  Data32bitsDirective = "\t.word\t";
  // .xword is only supported by V9.
  Data64bitsDirective = nullptr;
  ZeroDirective = "\t.skip\t";
  CommentString = "!";
  SupportsDebugInformation = true;

  ExceptionsType = ExceptionHandling::DwarfCFI;

  UsesELFSectionDirectiveForBSS = true;
}

const MCExpr *PS2VPUELFMCAsmInfo::getExprForPersonalitySymbol(
    const MCSymbol *Sym, unsigned Encoding, MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return PS2VPUMCExpr::create(PS2VPUMCExpr::VK_PS2VPU_R_DISP32,
                               MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }

  return MCAsmInfo::getExprForPersonalitySymbol(Sym, Encoding, Streamer);
}

const MCExpr *
PS2VPUELFMCAsmInfo::getExprForFDESymbol(const MCSymbol *Sym, unsigned Encoding,
                                       MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return PS2VPUMCExpr::create(PS2VPUMCExpr::VK_PS2VPU_R_DISP32,
                               MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }
  return MCAsmInfo::getExprForFDESymbol(Sym, Encoding, Streamer);
}
