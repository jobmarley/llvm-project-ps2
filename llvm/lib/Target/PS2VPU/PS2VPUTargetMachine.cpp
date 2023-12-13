
//#include "PS2VPU.h"
#include "PS2VPUTargetMachine.h"
#include "PS2VPUMachineFunctionInfo.h"
#include "PS2VPUTargetObjectFile.h"
#include "TargetInfo/ps2vpuTargetInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "PS2VPUTargetTransformInfo.h"
using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePS2VPUTarget() {
  // Register the target.
  RegisterTargetMachine<PS2VPUTargetMachine> X(getThePS2VPUTarget());
}

static std::string computeDataLayout(const Triple &T) {
  // PS2VPU is typically big endian, but some are little.
  std::string Ret = "e";
  Ret += "-m:e";

  // Some ABIs have 32bit pointers.
  //if (!is64Bit)
  Ret += "-p:16:16";
  Ret += "-f128:128";
  Ret += "-n16";
  // Alignments for 64 bit integers.
  //Ret += "-i64:64";

  // On PS2VPUV9 128 floats are aligned to 128 bits, on others only to 64.
  // On PS2VPUV9 registers can hold 64 or 32 bits, on others only 32.
  /*if (is64Bit)
    Ret += "-n32:64";
  else*/
    //Ret += "-f128:64-n32";

  /*if (is64Bit)
    Ret += "-S128";
  else*/
    Ret += "-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(std::optional<Reloc::Model> RM) {
  return RM.value_or(Reloc::Static);
}

// Code models. Some only make sense for 64-bit code.
//
// SunCC  Reloc   CodeModel  Constraints
// abs32  Static  Small      text+data+bss linked below 2^32 bytes
// abs44  Static  Medium     text+data+bss linked below 2^44 bytes
// abs64  Static  Large      text smaller than 2^31 bytes
// pic13  PIC_    Small      GOT < 2^13 bytes
// pic32  PIC_    Medium     GOT < 2^32 bytes
//
// All code models require that the text segment is smaller than 2GB.
static CodeModel::Model
getEffectivePS2VPUCodeModel(std::optional<CodeModel::Model> CM, Reloc::Model RM,
                           bool Is64Bit, bool JIT) {
  if (CM) {
    if (*CM == CodeModel::Tiny)
      report_fatal_error("Target does not support the tiny CodeModel", false);
    if (*CM == CodeModel::Kernel)
      report_fatal_error("Target does not support the kernel CodeModel", false);
    return *CM;
  }
  if (Is64Bit) {
    if (JIT)
      return CodeModel::Large;
    return RM == Reloc::PIC_ ? CodeModel::Small : CodeModel::Medium;
  }
  return CodeModel::Small;
}

/// Create an ILP32 architecture model
PS2VPUTargetMachine::PS2VPUTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         std::optional<Reloc::Model> RM,
                                         std::optional<CodeModel::Model> CM,
                                         CodeGenOptLevel OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        getEffectiveRelocModel(RM),
                        getEffectivePS2VPUCodeModel(
                            CM, getEffectiveRelocModel(RM), false, JIT),
                        OL),
      TLOF(std::make_unique<PS2VPUELFTargetObjectFile>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this, false),
      is64Bit(false) {
  initAsmInfo();
}

PS2VPUTargetMachine::~PS2VPUTargetMachine() = default;

const PS2VPUSubtarget *
PS2VPUTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function, so we can enable it as a subtarget feature.
  bool softFloat = F.getFnAttribute("use-soft-float").getValueAsBool();

  if (softFloat)
    FS += FS.empty() ? "+soft-float" : ",+soft-float";

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<PS2VPUSubtarget>(TargetTriple, CPU, FS, *this,
                                         this->is64Bit);
  }
  return I.get();
}

MachineFunctionInfo *PS2VPUTargetMachine::createMachineFunctionInfo(
    BumpPtrAllocator &Allocator, const Function &F,
    const TargetSubtargetInfo *STI) const {
  return PS2VPUMachineFunctionInfo::create<PS2VPUMachineFunctionInfo>(Allocator,
                                                                    F, STI);
}

namespace {
/// PS2VPU Code Generator Pass Configuration Options.
class PS2VPUPassConfig : public TargetPassConfig {
public:
  PS2VPUPassConfig(PS2VPUTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  PS2VPUTargetMachine &getPS2VPUTargetMachine() const {
    return getTM<PS2VPUTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *PS2VPUTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PS2VPUPassConfig(*this, PM);
}


TargetTransformInfo
PS2VPUTargetMachine::getTargetTransformInfo(const Function &F) const {
  return TargetTransformInfo(PS2VPUTTIImpl(this, F));
}

void PS2VPUPassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());

  TargetPassConfig::addIRPasses();
}

bool PS2VPUPassConfig::addInstSelector() {
  addPass(createPS2VPUISelDag(getPS2VPUTargetMachine()));
  return false;
}

void PS2VPUPassConfig::addPreEmitPass() {
  //addPass(createPS2VPUDelaySlotFillerPass());

  //if (this->getPS2VPUTargetMachine().getSubtargetImpl()->insertNOPLoad()) {
  //  addPass(new InsertNOPLoad());
  //}
  //if (this->getPS2VPUTargetMachine().getSubtargetImpl()->detectRoundChange()) {
  //  addPass(new DetectRoundChange());
  //}
  //if (this->getPS2VPUTargetMachine().getSubtargetImpl()->fixAllFDIVSQRT()) {
  //  addPass(new FixAllFDIVSQRT());
  //}
}

