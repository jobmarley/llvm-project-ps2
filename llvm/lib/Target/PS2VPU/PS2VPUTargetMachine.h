//===----------------------------------------------------------------------===//
//
// This file declares the PS2VPU specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PS2VPU_PS2VPUTARGETMACHINE_H
#define LLVM_LIB_TARGET_PS2VPU_PS2VPUTARGETMACHINE_H

#include "PS2VPUInstrInfo.h"
#include "PS2VPUSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class PS2VPUTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  PS2VPUSubtarget Subtarget;
  bool is64Bit;
  mutable StringMap<std::unique_ptr<PS2VPUSubtarget>> SubtargetMap;

public:
  PS2VPUTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                     StringRef FS, const TargetOptions &Options,
                      std::optional<Reloc::Model> RM,
                      std::optional<CodeModel::Model> CM,
                     CodeGenOptLevel OL, bool JIT);
  ~PS2VPUTargetMachine() override;

  const PS2VPUSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const PS2VPUSubtarget *getSubtargetImpl(const Function &) const override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetTransformInfo getTargetTransformInfo(const Function &F) const override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

   MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                            const TargetSubtargetInfo *STI) const override;
};

} // end namespace llvm

#endif
