//===--- PS2VPU.cpp - Implement PS2VPU target feature support ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements PS2VPU TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "PS2VPU.h"
#include "Targets.h"
#include "clang/Basic/MacroBuilder.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const char *const PS2VPUTargetInfo::GCCRegNames[] = {
    // Integer registers
    "r0",
    "r1",
    "r2",
    "r3",
    "r4",
    "r5",
    "r6",
    "r7",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15",
    "r16",
    "r17",
    "r18",
    "r19",
    "r20",
    "r21",
    "r22",
    "r23",
    "r24",
    "r25",
    "r26",
    "r27",
    "r28",
    "r29",
    "r30",
    "r31",

    // Floating-point registers
    "f0",
    "f1",
    "f2",
    "f3",
    "f4",
    "f5",
    "f6",
    "f7",
    "f8",
    "f9",
    "f10",
    "f11",
    "f12",
    "f13",
    "f14",
    "f15",
    "f16",
    "f17",
    "f18",
    "f19",
    "f20",
    "f21",
    "f22",
    "f23",
    "f24",
    "f25",
    "f26",
    "f27",
    "f28",
    "f29",
    "f30",
    "f31",
    "f32",
    "f34",
    "f36",
    "f38",
    "f40",
    "f42",
    "f44",
    "f46",
    "f48",
    "f50",
    "f52",
    "f54",
    "f56",
    "f58",
    "f60",
    "f62",
};

ArrayRef<const char *> PS2VPUTargetInfo::getGCCRegNames() const {
  return llvm::makeArrayRef(GCCRegNames);
}

const TargetInfo::GCCRegAlias PS2VPUTargetInfo::GCCRegAliases[] = {
    {{"g0"}, "r0"},  {{"g1"}, "r1"},  {{"g2"}, "r2"},        {{"g3"}, "r3"},
    {{"g4"}, "r4"},  {{"g5"}, "r5"},  {{"g6"}, "r6"},        {{"g7"}, "r7"},
    {{"o0"}, "r8"},  {{"o1"}, "r9"},  {{"o2"}, "r10"},       {{"o3"}, "r11"},
    {{"o4"}, "r12"}, {{"o5"}, "r13"}, {{"o6", "sp"}, "r14"}, {{"o7"}, "r15"},
    {{"l0"}, "r16"}, {{"l1"}, "r17"}, {{"l2"}, "r18"},       {{"l3"}, "r19"},
    {{"l4"}, "r20"}, {{"l5"}, "r21"}, {{"l6"}, "r22"},       {{"l7"}, "r23"},
    {{"i0"}, "r24"}, {{"i1"}, "r25"}, {{"i2"}, "r26"},       {{"i3"}, "r27"},
    {{"i4"}, "r28"}, {{"i5"}, "r29"}, {{"i6", "fp"}, "r30"}, {{"i7"}, "r31"},
};

ArrayRef<TargetInfo::GCCRegAlias> PS2VPUTargetInfo::getGCCRegAliases() const {
  return llvm::makeArrayRef(GCCRegAliases);
}

bool PS2VPUTargetInfo::hasFeature(StringRef Feature) const {
  return llvm::StringSwitch<bool>(Feature)
      .Case("PS2VPU", true)
      .Default(false);
}

struct PS2VPUCPUInfo {
  llvm::StringLiteral Name;
  PS2VPUTargetInfo::CPUKind Kind;
  PS2VPUTargetInfo::CPUGeneration Generation;
};

//static constexpr PS2VPUCPUInfo CPUInfo[] = {
//};
//
//PS2VPUTargetInfo::CPUGeneration
//PS2VPUTargetInfo::getCPUGeneration(CPUKind Kind) const {
//  if (Kind == CK_GENERIC)
//    return CG_V8;
//  const PS2VPUCPUInfo *Item = llvm::find_if(
//      CPUInfo, [Kind](const PS2VPUCPUInfo &Info) { return Info.Kind == Kind; });
//  if (Item == std::end(CPUInfo))
//    llvm_unreachable("Unexpected CPU kind");
//  return Item->Generation;
//}

PS2VPUTargetInfo::CPUKind PS2VPUTargetInfo::getCPUKind(StringRef Name) const {
  //const PS2VPUCPUInfo *Item = llvm::find_if(
  //    CPUInfo, [Name](const PS2VPUCPUInfo &Info) { return Info.Name == Name; });

  //if (Item == std::end(CPUInfo))
    return CK_GENERIC;
  /*return Item->Kind;*/
}

void PS2VPUTargetInfo::fillValidCPUList(
    SmallVectorImpl<StringRef> &Values) const {
  //for (const PS2VPUCPUInfo &Info : CPUInfo)
  //  Values.push_back(Info.Name);
}

void PS2VPUTargetInfo::getTargetDefines(const LangOptions &Opts,
                                       MacroBuilder &Builder) const {
  DefineStd(Builder, "PS2VPU", Opts);
  Builder.defineMacro("__REGISTER_PREFIX__", "");

  //if (SoftFloat)
  //  Builder.defineMacro("SOFT_FLOAT", "1");
}
