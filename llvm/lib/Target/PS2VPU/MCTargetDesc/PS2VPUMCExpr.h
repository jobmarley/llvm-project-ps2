//====- PS2VPUMCExpr.h - PS2VPU specific MC expression classes --*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes PS2VPU-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCEXPR_H
#define LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUMCEXPR_H

#include "PS2VPUFixupKinds.h"
#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class PS2VPUMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_PS2VPU_None,
    VK_PS2VPU_LO,
    VK_PS2VPU_HI,
    VK_PS2VPU_H44,
    VK_PS2VPU_M44,
    VK_PS2VPU_L44,
    VK_PS2VPU_HH,
    VK_PS2VPU_HM,
    VK_PS2VPU_LM,
    VK_PS2VPU_PC22,
    VK_PS2VPU_PC10,
    VK_PS2VPU_GOT22,
    VK_PS2VPU_GOT10,
    VK_PS2VPU_GOT13,
    VK_PS2VPU_13,
    VK_PS2VPU_WPLT30,
    VK_PS2VPU_WDISP30,
    VK_PS2VPU_R_DISP32,
    VK_PS2VPU_TLS_GD_HI22,
    VK_PS2VPU_TLS_GD_LO10,
    VK_PS2VPU_TLS_GD_ADD,
    VK_PS2VPU_TLS_GD_CALL,
    VK_PS2VPU_TLS_LDM_HI22,
    VK_PS2VPU_TLS_LDM_LO10,
    VK_PS2VPU_TLS_LDM_ADD,
    VK_PS2VPU_TLS_LDM_CALL,
    VK_PS2VPU_TLS_LDO_HIX22,
    VK_PS2VPU_TLS_LDO_LOX10,
    VK_PS2VPU_TLS_LDO_ADD,
    VK_PS2VPU_TLS_IE_HI22,
    VK_PS2VPU_TLS_IE_LO10,
    VK_PS2VPU_TLS_IE_LD,
    VK_PS2VPU_TLS_IE_LDX,
    VK_PS2VPU_TLS_IE_ADD,
    VK_PS2VPU_TLS_LE_HIX22,
    VK_PS2VPU_TLS_LE_LOX10,
    VK_PS2VPU_HIX22,
    VK_PS2VPU_LOX10,
    VK_PS2VPU_GOTDATA_HIX22,
    VK_PS2VPU_GOTDATA_LOX10,
    VK_PS2VPU_GOTDATA_OP,
  };

private:
  const VariantKind Kind;
  const MCExpr *Expr;

  explicit PS2VPUMCExpr(VariantKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  /// @name Construction
  /// @{

  static const PS2VPUMCExpr *create(VariantKind Kind, const MCExpr *Expr,
                                   MCContext &Ctx);
  /// @}
  /// @name Accessors
  /// @{

  /// getOpcode - Get the kind of this expression.
  VariantKind getKind() const { return Kind; }

  /// getSubExpr - Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  /// getFixupKind - Get the fixup kind of this expression.
  PS2VPU::Fixups getFixupKind() const { return getFixupKind(Kind); }

  /// @}
  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const PS2VPUMCExpr *) { return true; }

  static VariantKind parseVariantKind(StringRef name);
  static bool printVariantKind(raw_ostream &OS, VariantKind Kind);
  static PS2VPU::Fixups getFixupKind(VariantKind Kind);
};

} // end namespace llvm.

#endif
