//===----------------------------------------------------------------------===//
//
// This file implements the PS2VPU specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

//#include "PS2VPU.h"
#include "PS2VPUSubtarget.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "PS2VPU-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "PS2VPUGenSubtargetInfo.inc"

void PS2VPUSubtarget::anchor() {}

PS2VPUSubtarget &PS2VPUSubtarget::initializeSubtargetDependencies(StringRef CPU,
                                                                StringRef FS) {
  // Determine default and user specified characteristics
  std::string CPUName = std::string(CPU);
  //if (CPUName.empty())
  //  CPUName = (Is64Bit) ? "v9" : "v8";

  // Parse features string.
  ParseSubtargetFeatures(CPUName, /*TuneCPU*/ CPUName, FS);

  return *this;
}

PS2VPUSubtarget::PS2VPUSubtarget(const Triple &TT, const std::string &CPU,
                               const std::string &FS, const TargetMachine &TM,
                               bool is64Bit)
    : PS2VPUGenSubtargetInfo(TT, CPU, /*TuneCPU*/ CPU, FS), TargetTriple(TT),
      InstrInfo(initializeSubtargetDependencies(CPU, FS)),
      TLInfo(TM, *this), FrameLowering(*this) {}

int PS2VPUSubtarget::getAdjustedFrameSize(int frameSize) const {

  //if (is64Bit()) {
  //  // All 64-bit stack frames must be 16-byte aligned, and must reserve space
  //  // for spilling the 16 window registers at %sp+BIAS..%sp+BIAS+128.
  //  frameSize += 128;
  //  // Frames with calls must also reserve space for 6 outgoing arguments
  //  // whether they are used or not. LowerCall_64 takes care of that.
  //  frameSize = alignTo(frameSize, 16);
  //} else {
  //  // Emit the correct save instruction based on the number of bytes in
  //  // the frame. Minimum stack frame size according to V8 ABI is:
  //  //   16 words for register window spill
  //  //    1 word for address of returned aggregate-value
  //  // +  6 words for passing parameters on the stack
  //  // ----------
  //  //   23 words * 4 bytes per word = 92 bytes
  //  frameSize += 92;

  //  // Round up to next doubleword boundary -- a double-word boundary
  //  // is required by the ABI.
  //  frameSize = alignTo(frameSize, 8);
  //}
  frameSize = alignTo(frameSize, 16);
  return frameSize;
}

bool PS2VPUSubtarget::enableMachineScheduler() const { return true; }
