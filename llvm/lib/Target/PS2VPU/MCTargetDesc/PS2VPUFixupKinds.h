//===-- PS2VPUFixupKinds.h - PS2VPU Specific Fixup Entries --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUFIXUPKINDS_H
#define LLVM_LIB_TARGET_PS2VPU_MCTARGETDESC_PS2VPUFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace PS2VPU {
enum Fixups {
  // fixup_PS2VPU_call30 - 30-bit PC relative relocation for call
  fixup_PS2VPU_call30 = FirstTargetFixupKind,

  /// fixup_PS2VPU_br22 - 22-bit PC relative relocation for
  /// branches
  fixup_PS2VPU_br22,

  /// fixup_PS2VPU_br19 - 19-bit PC relative relocation for
  /// branches on icc/xcc
  fixup_PS2VPU_br19,

  /// fixup_PS2VPU_bpr  - 16-bit fixup for bpr
  fixup_PS2VPU_br16_2,
  fixup_PS2VPU_br16_14,

  /// fixup_PS2VPU_13 - 13-bit fixup
  fixup_PS2VPU_13,

  /// fixup_PS2VPU_hi22  - 22-bit fixup corresponding to %hi(foo)
  /// for sethi
  fixup_PS2VPU_hi22,

  /// fixup_PS2VPU_lo10  - 10-bit fixup corresponding to %lo(foo)
  fixup_PS2VPU_lo10,

  /// fixup_PS2VPU_h44  - 22-bit fixup corresponding to %h44(foo)
  fixup_PS2VPU_h44,

  /// fixup_PS2VPU_m44  - 10-bit fixup corresponding to %m44(foo)
  fixup_PS2VPU_m44,

  /// fixup_PS2VPU_l44  - 12-bit fixup corresponding to %l44(foo)
  fixup_PS2VPU_l44,

  /// fixup_PS2VPU_hh  -  22-bit fixup corresponding to %hh(foo)
  fixup_PS2VPU_hh,

  /// fixup_PS2VPU_hm  -  10-bit fixup corresponding to %hm(foo)
  fixup_PS2VPU_hm,

  /// fixup_PS2VPU_lm  -  22-bit fixup corresponding to %lm(foo)
  fixup_PS2VPU_lm,

  /// fixup_PS2VPU_pc22 - 22-bit fixup corresponding to %pc22(foo)
  fixup_PS2VPU_pc22,

  /// fixup_PS2VPU_pc10 - 10-bit fixup corresponding to %pc10(foo)
  fixup_PS2VPU_pc10,

  /// fixup_PS2VPU_got22 - 22-bit fixup corresponding to %got22(foo)
  fixup_PS2VPU_got22,

  /// fixup_PS2VPU_got10 - 10-bit fixup corresponding to %got10(foo)
  fixup_PS2VPU_got10,

  /// fixup_PS2VPU_got13 - 13-bit fixup corresponding to %got13(foo)
  fixup_PS2VPU_got13,

  /// fixup_PS2VPU_wplt30
  fixup_PS2VPU_wplt30,

  /// fixups for Thread Local Storage
  fixup_PS2VPU_tls_gd_hi22,
  fixup_PS2VPU_tls_gd_lo10,
  fixup_PS2VPU_tls_gd_add,
  fixup_PS2VPU_tls_gd_call,
  fixup_PS2VPU_tls_ldm_hi22,
  fixup_PS2VPU_tls_ldm_lo10,
  fixup_PS2VPU_tls_ldm_add,
  fixup_PS2VPU_tls_ldm_call,
  fixup_PS2VPU_tls_ldo_hix22,
  fixup_PS2VPU_tls_ldo_lox10,
  fixup_PS2VPU_tls_ldo_add,
  fixup_PS2VPU_tls_ie_hi22,
  fixup_PS2VPU_tls_ie_lo10,
  fixup_PS2VPU_tls_ie_ld,
  fixup_PS2VPU_tls_ie_ldx,
  fixup_PS2VPU_tls_ie_add,
  fixup_PS2VPU_tls_le_hix22,
  fixup_PS2VPU_tls_le_lox10,

  /// 22-bit fixup corresponding to %hix(foo)
  fixup_PS2VPU_hix22,
  /// 13-bit fixup corresponding to %lox(foo)
  fixup_PS2VPU_lox10,

  /// 22-bit fixup corresponding to %gdop_hix22(foo)
  fixup_PS2VPU_gotdata_hix22,
  /// 13-bit fixup corresponding to %gdop_lox10(foo)
  fixup_PS2VPU_gotdata_lox10,
  /// 32-bit fixup corresponding to %gdop(foo)
  fixup_PS2VPU_gotdata_op,

  // Marker
  LastTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};
}
} // namespace llvm

#endif
