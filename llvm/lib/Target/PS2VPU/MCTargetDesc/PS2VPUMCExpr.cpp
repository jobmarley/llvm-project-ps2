//===-- PS2VPUMCExpr.cpp - PS2VPU specific MC expression classes --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the PS2VPU architecture (e.g. "%hi", "%lo", ...).
//
//===----------------------------------------------------------------------===//

#include "PS2VPUMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCObjectStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

#define DEBUG_TYPE "PS2VPUmcexpr"

const PS2VPUMCExpr *PS2VPUMCExpr::create(VariantKind Kind, const MCExpr *Expr,
                                       MCContext &Ctx) {
  return new (Ctx) PS2VPUMCExpr(Kind, Expr);
}

void PS2VPUMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {

  bool closeParen = printVariantKind(OS, Kind);

  const MCExpr *Expr = getSubExpr();
  Expr->print(OS, MAI);

  if (closeParen)
    OS << ')';
}

bool PS2VPUMCExpr::printVariantKind(raw_ostream &OS, VariantKind Kind) {
  switch (Kind) {
  case VK_PS2VPU_None:
    return false;
  case VK_PS2VPU_LO:
    OS << "%lo(";
    return true;
  case VK_PS2VPU_HI:
    OS << "%hi(";
    return true;
  case VK_PS2VPU_H44:
    OS << "%h44(";
    return true;
  case VK_PS2VPU_M44:
    OS << "%m44(";
    return true;
  case VK_PS2VPU_L44:
    OS << "%l44(";
    return true;
  case VK_PS2VPU_HH:
    OS << "%hh(";
    return true;
  case VK_PS2VPU_HM:
    OS << "%hm(";
    return true;
  case VK_PS2VPU_LM:
    OS << "%lm(";
    return true;
    // FIXME: use %pc22/%pc10, if system assembler supports them.
  case VK_PS2VPU_PC22:
    OS << "%hi(";
    return true;
  case VK_PS2VPU_PC10:
    OS << "%lo(";
    return true;
    // FIXME: use %got22/%got10, if system assembler supports them.
  case VK_PS2VPU_GOT22:
    OS << "%hi(";
    return true;
  case VK_PS2VPU_GOT10:
    OS << "%lo(";
    return true;
  case VK_PS2VPU_GOT13:
    return false;
  case VK_PS2VPU_13:
    return false;
  case VK_PS2VPU_WDISP30:
    return false;
  case VK_PS2VPU_WPLT30:
    return false;
  case VK_PS2VPU_R_DISP32:
    OS << "%r_disp32(";
    return true;
  case VK_PS2VPU_TLS_GD_HI22:
    OS << "%tgd_hi22(";
    return true;
  case VK_PS2VPU_TLS_GD_LO10:
    OS << "%tgd_lo10(";
    return true;
  case VK_PS2VPU_TLS_GD_ADD:
    OS << "%tgd_add(";
    return true;
  case VK_PS2VPU_TLS_GD_CALL:
    OS << "%tgd_call(";
    return true;
  case VK_PS2VPU_TLS_LDM_HI22:
    OS << "%tldm_hi22(";
    return true;
  case VK_PS2VPU_TLS_LDM_LO10:
    OS << "%tldm_lo10(";
    return true;
  case VK_PS2VPU_TLS_LDM_ADD:
    OS << "%tldm_add(";
    return true;
  case VK_PS2VPU_TLS_LDM_CALL:
    OS << "%tldm_call(";
    return true;
  case VK_PS2VPU_TLS_LDO_HIX22:
    OS << "%tldo_hix22(";
    return true;
  case VK_PS2VPU_TLS_LDO_LOX10:
    OS << "%tldo_lox10(";
    return true;
  case VK_PS2VPU_TLS_LDO_ADD:
    OS << "%tldo_add(";
    return true;
  case VK_PS2VPU_TLS_IE_HI22:
    OS << "%tie_hi22(";
    return true;
  case VK_PS2VPU_TLS_IE_LO10:
    OS << "%tie_lo10(";
    return true;
  case VK_PS2VPU_TLS_IE_LD:
    OS << "%tie_ld(";
    return true;
  case VK_PS2VPU_TLS_IE_LDX:
    OS << "%tie_ldx(";
    return true;
  case VK_PS2VPU_TLS_IE_ADD:
    OS << "%tie_add(";
    return true;
  case VK_PS2VPU_TLS_LE_HIX22:
    OS << "%tle_hix22(";
    return true;
  case VK_PS2VPU_TLS_LE_LOX10:
    OS << "%tle_lox10(";
    return true;
  case VK_PS2VPU_HIX22:
    OS << "%hix(";
    return true;
  case VK_PS2VPU_LOX10:
    OS << "%lox(";
    return true;
  case VK_PS2VPU_GOTDATA_HIX22:
    OS << "%gdop_hix22(";
    return true;
  case VK_PS2VPU_GOTDATA_LOX10:
    OS << "%gdop_lox10(";
    return true;
  case VK_PS2VPU_GOTDATA_OP:
    OS << "%gdop(";
    return true;
  }
  llvm_unreachable("Unhandled PS2VPUMCExpr::VariantKind");
}

PS2VPUMCExpr::VariantKind PS2VPUMCExpr::parseVariantKind(StringRef name) {
  return StringSwitch<PS2VPUMCExpr::VariantKind>(name)
      .Case("lo", VK_PS2VPU_LO)
      .Case("hi", VK_PS2VPU_HI)
      .Case("h44", VK_PS2VPU_H44)
      .Case("m44", VK_PS2VPU_M44)
      .Case("l44", VK_PS2VPU_L44)
      .Case("hh", VK_PS2VPU_HH)
      .Case("hm", VK_PS2VPU_HM)
      .Case("lm", VK_PS2VPU_LM)
      .Case("pc22", VK_PS2VPU_PC22)
      .Case("pc10", VK_PS2VPU_PC10)
      .Case("got22", VK_PS2VPU_GOT22)
      .Case("got10", VK_PS2VPU_GOT10)
      .Case("got13", VK_PS2VPU_GOT13)
      .Case("r_disp32", VK_PS2VPU_R_DISP32)
      .Case("tgd_hi22", VK_PS2VPU_TLS_GD_HI22)
      .Case("tgd_lo10", VK_PS2VPU_TLS_GD_LO10)
      .Case("tgd_add", VK_PS2VPU_TLS_GD_ADD)
      .Case("tgd_call", VK_PS2VPU_TLS_GD_CALL)
      .Case("tldm_hi22", VK_PS2VPU_TLS_LDM_HI22)
      .Case("tldm_lo10", VK_PS2VPU_TLS_LDM_LO10)
      .Case("tldm_add", VK_PS2VPU_TLS_LDM_ADD)
      .Case("tldm_call", VK_PS2VPU_TLS_LDM_CALL)
      .Case("tldo_hix22", VK_PS2VPU_TLS_LDO_HIX22)
      .Case("tldo_lox10", VK_PS2VPU_TLS_LDO_LOX10)
      .Case("tldo_add", VK_PS2VPU_TLS_LDO_ADD)
      .Case("tie_hi22", VK_PS2VPU_TLS_IE_HI22)
      .Case("tie_lo10", VK_PS2VPU_TLS_IE_LO10)
      .Case("tie_ld", VK_PS2VPU_TLS_IE_LD)
      .Case("tie_ldx", VK_PS2VPU_TLS_IE_LDX)
      .Case("tie_add", VK_PS2VPU_TLS_IE_ADD)
      .Case("tle_hix22", VK_PS2VPU_TLS_LE_HIX22)
      .Case("tle_lox10", VK_PS2VPU_TLS_LE_LOX10)
      .Case("hix", VK_PS2VPU_HIX22)
      .Case("lox", VK_PS2VPU_LOX10)
      .Case("gdop_hix22", VK_PS2VPU_GOTDATA_HIX22)
      .Case("gdop_lox10", VK_PS2VPU_GOTDATA_LOX10)
      .Case("gdop", VK_PS2VPU_GOTDATA_OP)
      .Default(VK_PS2VPU_None);
}

PS2VPU::Fixups PS2VPUMCExpr::getFixupKind(PS2VPUMCExpr::VariantKind Kind) {
  switch (Kind) {
  default:
    llvm_unreachable("Unhandled PS2VPUMCExpr::VariantKind");
  case VK_PS2VPU_LO:
    return PS2VPU::fixup_PS2VPU_lo10;
  case VK_PS2VPU_HI:
    return PS2VPU::fixup_PS2VPU_hi22;
  case VK_PS2VPU_H44:
    return PS2VPU::fixup_PS2VPU_h44;
  case VK_PS2VPU_M44:
    return PS2VPU::fixup_PS2VPU_m44;
  case VK_PS2VPU_L44:
    return PS2VPU::fixup_PS2VPU_l44;
  case VK_PS2VPU_HH:
    return PS2VPU::fixup_PS2VPU_hh;
  case VK_PS2VPU_HM:
    return PS2VPU::fixup_PS2VPU_hm;
  case VK_PS2VPU_LM:
    return PS2VPU::fixup_PS2VPU_lm;
  case VK_PS2VPU_PC22:
    return PS2VPU::fixup_PS2VPU_pc22;
  case VK_PS2VPU_PC10:
    return PS2VPU::fixup_PS2VPU_pc10;
  case VK_PS2VPU_GOT22:
    return PS2VPU::fixup_PS2VPU_got22;
  case VK_PS2VPU_GOT10:
    return PS2VPU::fixup_PS2VPU_got10;
  case VK_PS2VPU_GOT13:
    return PS2VPU::fixup_PS2VPU_got13;
  case VK_PS2VPU_13:
    return PS2VPU::fixup_PS2VPU_13;
  case VK_PS2VPU_WPLT30:
    return PS2VPU::fixup_PS2VPU_wplt30;
  case VK_PS2VPU_WDISP30:
    return PS2VPU::fixup_PS2VPU_call30;
  case VK_PS2VPU_TLS_GD_HI22:
    return PS2VPU::fixup_PS2VPU_tls_gd_hi22;
  case VK_PS2VPU_TLS_GD_LO10:
    return PS2VPU::fixup_PS2VPU_tls_gd_lo10;
  case VK_PS2VPU_TLS_GD_ADD:
    return PS2VPU::fixup_PS2VPU_tls_gd_add;
  case VK_PS2VPU_TLS_GD_CALL:
    return PS2VPU::fixup_PS2VPU_tls_gd_call;
  case VK_PS2VPU_TLS_LDM_HI22:
    return PS2VPU::fixup_PS2VPU_tls_ldm_hi22;
  case VK_PS2VPU_TLS_LDM_LO10:
    return PS2VPU::fixup_PS2VPU_tls_ldm_lo10;
  case VK_PS2VPU_TLS_LDM_ADD:
    return PS2VPU::fixup_PS2VPU_tls_ldm_add;
  case VK_PS2VPU_TLS_LDM_CALL:
    return PS2VPU::fixup_PS2VPU_tls_ldm_call;
  case VK_PS2VPU_TLS_LDO_HIX22:
    return PS2VPU::fixup_PS2VPU_tls_ldo_hix22;
  case VK_PS2VPU_TLS_LDO_LOX10:
    return PS2VPU::fixup_PS2VPU_tls_ldo_lox10;
  case VK_PS2VPU_TLS_LDO_ADD:
    return PS2VPU::fixup_PS2VPU_tls_ldo_add;
  case VK_PS2VPU_TLS_IE_HI22:
    return PS2VPU::fixup_PS2VPU_tls_ie_hi22;
  case VK_PS2VPU_TLS_IE_LO10:
    return PS2VPU::fixup_PS2VPU_tls_ie_lo10;
  case VK_PS2VPU_TLS_IE_LD:
    return PS2VPU::fixup_PS2VPU_tls_ie_ld;
  case VK_PS2VPU_TLS_IE_LDX:
    return PS2VPU::fixup_PS2VPU_tls_ie_ldx;
  case VK_PS2VPU_TLS_IE_ADD:
    return PS2VPU::fixup_PS2VPU_tls_ie_add;
  case VK_PS2VPU_TLS_LE_HIX22:
    return PS2VPU::fixup_PS2VPU_tls_le_hix22;
  case VK_PS2VPU_TLS_LE_LOX10:
    return PS2VPU::fixup_PS2VPU_tls_le_lox10;
  case VK_PS2VPU_HIX22:
    return PS2VPU::fixup_PS2VPU_hix22;
  case VK_PS2VPU_LOX10:
    return PS2VPU::fixup_PS2VPU_lox10;
  case VK_PS2VPU_GOTDATA_HIX22:
    return PS2VPU::fixup_PS2VPU_gotdata_hix22;
  case VK_PS2VPU_GOTDATA_LOX10:
    return PS2VPU::fixup_PS2VPU_gotdata_lox10;
  case VK_PS2VPU_GOTDATA_OP:
    return PS2VPU::fixup_PS2VPU_gotdata_op;
  }
}

bool PS2VPUMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                            const MCAsmLayout *Layout,
                                            const MCFixup *Fixup) const {
  return getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup);
}

static void fixELFSymbolsInTLSFixupsImpl(const MCExpr *Expr, MCAssembler &Asm) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    llvm_unreachable("Can't handle nested target expr!");
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    fixELFSymbolsInTLSFixupsImpl(BE->getLHS(), Asm);
    fixELFSymbolsInTLSFixupsImpl(BE->getRHS(), Asm);
    break;
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    cast<MCSymbolELF>(SymRef.getSymbol()).setType(ELF::STT_TLS);
    break;
  }

  case MCExpr::Unary:
    fixELFSymbolsInTLSFixupsImpl(cast<MCUnaryExpr>(Expr)->getSubExpr(), Asm);
    break;
  }
}

void PS2VPUMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  switch (getKind()) {
  default:
    return;
  case VK_PS2VPU_TLS_GD_CALL:
  case VK_PS2VPU_TLS_LDM_CALL: {
    // The corresponding relocations reference __tls_get_addr, as they call it,
    // but this is only implicit; we must explicitly add it to our symbol table
    // to bind it for these uses.
    MCSymbol *Symbol = Asm.getContext().getOrCreateSymbol("__tls_get_addr");
    Asm.registerSymbol(*Symbol);
    auto ELFSymbol = cast<MCSymbolELF>(Symbol);
    if (!ELFSymbol->isBindingSet())
      ELFSymbol->setBinding(ELF::STB_GLOBAL);
    [[fallthrough]];
  }
  case VK_PS2VPU_TLS_GD_HI22:
  case VK_PS2VPU_TLS_GD_LO10:
  case VK_PS2VPU_TLS_GD_ADD:
  case VK_PS2VPU_TLS_LDM_HI22:
  case VK_PS2VPU_TLS_LDM_LO10:
  case VK_PS2VPU_TLS_LDM_ADD:
  case VK_PS2VPU_TLS_LDO_HIX22:
  case VK_PS2VPU_TLS_LDO_LOX10:
  case VK_PS2VPU_TLS_LDO_ADD:
  case VK_PS2VPU_TLS_IE_HI22:
  case VK_PS2VPU_TLS_IE_LO10:
  case VK_PS2VPU_TLS_IE_LD:
  case VK_PS2VPU_TLS_IE_LDX:
  case VK_PS2VPU_TLS_IE_ADD:
  case VK_PS2VPU_TLS_LE_HIX22:
  case VK_PS2VPU_TLS_LE_LOX10:
    break;
  }
  fixELFSymbolsInTLSFixupsImpl(getSubExpr(), Asm);
}

void PS2VPUMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}
