//===--- PS2VPU.h - declare PS2VPU target feature support ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares PS2VPU TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_PS2VPU_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_PS2VPU_H
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Support/Compiler.h"
namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY PS2VPUTargetInfo : public TargetInfo {
  static const TargetInfo::GCCRegAlias GCCRegAliases[];
  static const char *const GCCRegNames[];

public:
  PS2VPUTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple) {
      // Not sure about that, it might cause issues when mixing code.
      // Lets start like that for now cause int16 is the only supported natively
    TLSSupported = false;
    PointerWidth = 16;
    PointerAlign = 32;
    IntWidth = 16;
    IntAlign = 32;
    LongWidth = 32;
    LongAlign = 32;
    LongLongWidth = 64;
    LongLongAlign = 64;
    SuitableAlign = 32;
    DefaultAlignForAttributeAligned = 32;
    HalfWidth = 16;
    HalfAlign = 32;
    FloatWidth = 32;
    FloatAlign = 32;
    FloatFormat = &llvm::APFloat::IEEEsingle();
    DoubleWidth = 64;
    DoubleAlign = 64;
    DoubleFormat = &llvm::APFloat::IEEEdouble();
    LongDoubleWidth = 64;
    LongDoubleAlign = 64;
    LongDoubleFormat = &llvm::APFloat::IEEEdouble();
    SizeType = UnsignedInt;
    PtrDiffType = SignedInt;
    IntPtrType = SignedInt;
    Char16Type = UnsignedInt;
    WIntType = SignedInt;
    Int16Type = SignedInt;
    Char32Type = UnsignedLong;
    SigAtomicType = SignedChar;
    MaxVectorAlign = 128;
    resetDataLayout("e-m:e-p:16:16-f128:128-n16-S64");
  }

  int getEHDataRegisterNumber(unsigned RegNo) const override {
    if (RegNo == 0)
      return 24;
    if (RegNo == 1)
      return 25;
    return -1;
  }

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) override {
    // Check if software floating point is enabled
    /*if (llvm::is_contained(Features, "+soft-float"))
      SoftFloat = true;*/
    return true;
  }
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  bool hasFeature(StringRef Feature) const override;

  ArrayRef<Builtin::Info> getTargetBuiltins() const override {
    // FIXME: Implement!
    return std::nullopt;
  }
  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::VoidPtrBuiltinVaList;
  }
  ArrayRef<const char *> getGCCRegNames() const override;
  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override;
  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &info) const override {
    //// FIXME: Implement!
    //switch (*Name) {
    //case 'I': // Signed 13-bit constant
    //case 'J': // Zero
    //case 'K': // 32-bit constant with the low 12 bits clear
    //case 'L': // A constant in the range supported by movcc (11-bit signed imm)
    //case 'M': // A constant in the range supported by movrcc (19-bit signed imm)
    //case 'N': // Same as 'K' but zext (required for SIMode)
    //case 'O': // The constant 4096
    //  return true;

    //case 'f':
    //case 'e':
    //  info.setAllowsRegister();
    //  return true;
    //}
    return false;
  }
  std::string_view getClobbers() const override {
    // FIXME: Implement!
    return "";
  }

  enum CPUKind {
    CK_GENERIC,
  } CPU = CK_GENERIC;

  enum CPUGeneration {
    CG_V8,
    CG_V9,
  };

  CPUGeneration getCPUGeneration(CPUKind Kind) const;

  CPUKind getCPUKind(StringRef Name) const;

  bool isValidCPUName(StringRef Name) const override {
    return getCPUKind(Name) != CK_GENERIC;
  }

  void fillValidCPUList(SmallVectorImpl<StringRef> &Values) const override;

  bool setCPU(const std::string &Name) override {
    CPU = getCPUKind(Name);
    return CPU != CK_GENERIC;
  }
};


} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_PS2VPU_H
