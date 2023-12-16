//===-- PS2VPUTargetStreamer.cpp - PS2VPU Target Streamer Methods -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides PS2VPU specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "PS2VPUInstPrinter.h"
#include "PS2VPUTargetStreamer.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
PS2VPUTargetStreamer::PS2VPUTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void PS2VPUTargetStreamer::anchor() {}

PS2VPUTargetAsmStreamer::PS2VPUTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : PS2VPUTargetStreamer(S), OS(OS) {}

void PS2VPUTargetAsmStreamer::emitPS2VPURegisterIgnore(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(PS2VPUInstPrinter::getRegisterName(reg)).lower()
     << ", #ignore\n";
}

void PS2VPUTargetAsmStreamer::emitPS2VPURegisterScratch(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(PS2VPUInstPrinter::getRegisterName(reg)).lower()
     << ", #scratch\n";
}
void PS2VPUTargetAsmStreamer::prettyPrintAsm(MCInstPrinter &InstPrinter,
                                           uint64_t Address,
    const MCInst& Inst, const MCSubtargetInfo& STI,
    raw_ostream& OS)
{
  std::string Buffer;
  {
    raw_string_ostream TempStream(Buffer);
    InstPrinter.printInst(&Inst, Address, "", STI, TempStream);
  }
  StringRef Contents(Buffer);
  auto PacketBundle = Contents.rsplit('\n');
  auto HeadTail = PacketBundle.first.split('\n');
  StringRef Separator = "\n";
  StringRef Indent = "\t";
  OS << "\t{\n";
  while (!HeadTail.first.empty()) {
    OS << Indent << HeadTail.first << Separator;
    HeadTail = HeadTail.second.split('\n');
  }

    OS << "\t}" << PacketBundle.second;
}

PS2VPUTargetELFStreamer::PS2VPUTargetELFStreamer(MCStreamer &S)
    : PS2VPUTargetStreamer(S) {}

MCELFStreamer &PS2VPUTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
