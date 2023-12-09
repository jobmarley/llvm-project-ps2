//===-- PS2VPUELFObjectWriter.cpp - PS2VPU ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUFixupKinds.h"
#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "MCTargetDesc/PS2VPUMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class PS2VPUELFObjectWriter : public MCELFObjectTargetWriter {
public:
  PS2VPUELFObjectWriter(bool Is64Bit, uint8_t OSABI)
      : MCELFObjectTargetWriter(Is64Bit, OSABI,
                                ELF::EM_PS2VPU,
                                /*HasRelocationAddend*/ true) {}

  ~PS2VPUELFObjectWriter() override = default;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;

  bool needsRelocateWithSymbol(const MCValue &Val, const MCSymbol &Sym,
                               unsigned Type) const override;
};
} // namespace

unsigned PS2VPUELFObjectWriter::getRelocType(MCContext &Ctx,
                                            const MCValue &Target,
                                            const MCFixup &Fixup,
                                            bool IsPCRel) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  //if (const PS2VPUMCExpr *SExpr = dyn_cast<PS2VPUMCExpr>(Fixup.getValue())) {
  //  if (SExpr->getKind() == PS2VPUMCExpr::VK_PS2VPU_R_DISP32)
  //    return ELF::R_PS2VPU_DISP32;
  //}

  //if (IsPCRel) {
  //  switch (Fixup.getTargetKind()) {
  //  default:
  //    llvm_unreachable("Unimplemented fixup -> relocation");
  //  case FK_Data_1:
  //    return ELF::R_PS2VPU_DISP8;
  //  case FK_Data_2:
  //    return ELF::R_PS2VPU_DISP16;
  //  case FK_Data_4:
  //    return ELF::R_PS2VPU_DISP32;
  //  case FK_Data_8:
  //    return ELF::R_PS2VPU_DISP64;
  //  case PS2VPU::fixup_PS2VPU_call30:
  //    return ELF::R_PS2VPU_WDISP30;
  //  case PS2VPU::fixup_PS2VPU_br22:
  //    return ELF::R_PS2VPU_WDISP22;
  //  case PS2VPU::fixup_PS2VPU_br19:
  //    return ELF::R_PS2VPU_WDISP19;
  //  case PS2VPU::fixup_PS2VPU_pc22:
  //    return ELF::R_PS2VPU_PC22;
  //  case PS2VPU::fixup_PS2VPU_pc10:
  //    return ELF::R_PS2VPU_PC10;
  //  case PS2VPU::fixup_PS2VPU_wplt30:
  //    return ELF::R_PS2VPU_WPLT30;
  //  }
  //}

  //switch (Fixup.getTargetKind()) {
  //default:
  //  llvm_unreachable("Unimplemented fixup -> relocation");
  //case FK_NONE:
  //  return ELF::R_PS2VPU_NONE;
  //case FK_Data_1:
  //  return ELF::R_PS2VPU_8;
  //case FK_Data_2:
  //  return ((Fixup.getOffset() % 2) ? ELF::R_PS2VPU_UA16 : ELF::R_PS2VPU_16);
  //case FK_Data_4:
  //  return ((Fixup.getOffset() % 4) ? ELF::R_PS2VPU_UA32 : ELF::R_PS2VPU_32);
  //case FK_Data_8:
  //  return ((Fixup.getOffset() % 8) ? ELF::R_PS2VPU_UA64 : ELF::R_PS2VPU_64);
  //case PS2VPU::fixup_PS2VPU_13:
  //  return ELF::R_PS2VPU_13;
  //case PS2VPU::fixup_PS2VPU_hi22:
  //  return ELF::R_PS2VPU_HI22;
  //case PS2VPU::fixup_PS2VPU_lo10:
  //  return ELF::R_PS2VPU_LO10;
  //case PS2VPU::fixup_PS2VPU_h44:
  //  return ELF::R_PS2VPU_H44;
  //case PS2VPU::fixup_PS2VPU_m44:
  //  return ELF::R_PS2VPU_M44;
  //case PS2VPU::fixup_PS2VPU_l44:
  //  return ELF::R_PS2VPU_L44;
  //case PS2VPU::fixup_PS2VPU_hh:
  //  return ELF::R_PS2VPU_HH22;
  //case PS2VPU::fixup_PS2VPU_hm:
  //  return ELF::R_PS2VPU_HM10;
  //case PS2VPU::fixup_PS2VPU_lm:
  //  return ELF::R_PS2VPU_LM22;
  //case PS2VPU::fixup_PS2VPU_got22:
  //  return ELF::R_PS2VPU_GOT22;
  //case PS2VPU::fixup_PS2VPU_got10:
  //  return ELF::R_PS2VPU_GOT10;
  //case PS2VPU::fixup_PS2VPU_got13:
  //  return ELF::R_PS2VPU_GOT13;
  //case PS2VPU::fixup_PS2VPU_tls_gd_hi22:
  //  return ELF::R_PS2VPU_TLS_GD_HI22;
  //case PS2VPU::fixup_PS2VPU_tls_gd_lo10:
  //  return ELF::R_PS2VPU_TLS_GD_LO10;
  //case PS2VPU::fixup_PS2VPU_tls_gd_add:
  //  return ELF::R_PS2VPU_TLS_GD_ADD;
  //case PS2VPU::fixup_PS2VPU_tls_gd_call:
  //  return ELF::R_PS2VPU_TLS_GD_CALL;
  //case PS2VPU::fixup_PS2VPU_tls_ldm_hi22:
  //  return ELF::R_PS2VPU_TLS_LDM_HI22;
  //case PS2VPU::fixup_PS2VPU_tls_ldm_lo10:
  //  return ELF::R_PS2VPU_TLS_LDM_LO10;
  //case PS2VPU::fixup_PS2VPU_tls_ldm_add:
  //  return ELF::R_PS2VPU_TLS_LDM_ADD;
  //case PS2VPU::fixup_PS2VPU_tls_ldm_call:
  //  return ELF::R_PS2VPU_TLS_LDM_CALL;
  //case PS2VPU::fixup_PS2VPU_tls_ldo_hix22:
  //  return ELF::R_PS2VPU_TLS_LDO_HIX22;
  //case PS2VPU::fixup_PS2VPU_tls_ldo_lox10:
  //  return ELF::R_PS2VPU_TLS_LDO_LOX10;
  //case PS2VPU::fixup_PS2VPU_tls_ldo_add:
  //  return ELF::R_PS2VPU_TLS_LDO_ADD;
  //case PS2VPU::fixup_PS2VPU_tls_ie_hi22:
  //  return ELF::R_PS2VPU_TLS_IE_HI22;
  //case PS2VPU::fixup_PS2VPU_tls_ie_lo10:
  //  return ELF::R_PS2VPU_TLS_IE_LO10;
  //case PS2VPU::fixup_PS2VPU_tls_ie_ld:
  //  return ELF::R_PS2VPU_TLS_IE_LD;
  //case PS2VPU::fixup_PS2VPU_tls_ie_ldx:
  //  return ELF::R_PS2VPU_TLS_IE_LDX;
  //case PS2VPU::fixup_PS2VPU_tls_ie_add:
  //  return ELF::R_PS2VPU_TLS_IE_ADD;
  //case PS2VPU::fixup_PS2VPU_tls_le_hix22:
  //  return ELF::R_PS2VPU_TLS_LE_HIX22;
  //case PS2VPU::fixup_PS2VPU_tls_le_lox10:
  //  return ELF::R_PS2VPU_TLS_LE_LOX10;
  //case PS2VPU::fixup_PS2VPU_hix22:
  //  return ELF::R_PS2VPU_HIX22;
  //case PS2VPU::fixup_PS2VPU_lox10:
  //  return ELF::R_PS2VPU_LOX10;
  //case PS2VPU::fixup_PS2VPU_gotdata_hix22:
  //  return ELF::R_PS2VPU_GOTDATA_HIX22;
  //case PS2VPU::fixup_PS2VPU_gotdata_lox10:
  //  return ELF::R_PS2VPU_GOTDATA_LOX10;
  //case PS2VPU::fixup_PS2VPU_gotdata_op:
  //  return ELF::R_PS2VPU_GOTDATA_OP;
  //}

  //return ELF::R_PS2VPU_NONE;

return 0;
}

bool PS2VPUELFObjectWriter::needsRelocateWithSymbol(const MCValue &Val,
                                                    const MCSymbol &Sym,
                                                   unsigned Type) const {
  //switch (Type) {
  //default:
  //  return false;

  //// All relocations that use a GOT need a symbol, not an offset, as
  //// the offset of the symbol within the section is irrelevant to
  //// where the GOT entry is. Don't need to list all the TLS entries,
  //// as they're all marked as requiring a symbol anyways.
  //case ELF::R_PS2VPU_GOT10:
  //case ELF::R_PS2VPU_GOT13:
  //case ELF::R_PS2VPU_GOT22:
  //case ELF::R_PS2VPU_GOTDATA_HIX22:
  //case ELF::R_PS2VPU_GOTDATA_LOX10:
  //case ELF::R_PS2VPU_GOTDATA_OP_HIX22:
  //case ELF::R_PS2VPU_GOTDATA_OP_LOX10:
  //  return true;
  //}
return false;
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createPS2VPUELFObjectWriter(bool Is64Bit, uint8_t OSABI) {
  return std::make_unique<PS2VPUELFObjectWriter>(Is64Bit, OSABI);
}
