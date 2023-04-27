#include "TargetInfo/PS2VPUTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getThePS2VPUTarget() {
  static Target ThePS2VPUTarget;
  return ThePS2VPUTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePS2VPUTargetInfo() {
  RegisterTarget<Triple::ps2vpu, /*HasJIT=*/false> X(getThePS2VPUTarget(),
                                                    "ps2vpu", "PS2VPU", "PS2VPU");
}
