//===- PS2VPUPacketizer.cpp - VLIW packetizer ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This implements a simple VLIW packetizer using DFA. The packetizer works on
// machine basic blocks. For each instruction I in BB, the packetizer consults
// the DFA to see if machine resources are available to execute I. If so, the
// packetizer checks if I depends on any instruction J in the current packet.
// If no dependency is found, I is added to current packet and machine resource
// is marked as taken. If any dependency is found, a target API call is made to
// prune the dependence.
//
//===----------------------------------------------------------------------===//

#include "PS2VPU.h"
#include "PS2VPUInstrInfo.h"
#include "PS2VPURegisterInfo.h"
#include "PS2VPUSubtarget.h"
#include "PS2VPUVLIWPacketizer.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineBranchProbabilityInfo.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBundle.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>
#include <iterator>

using namespace llvm;

#define DEBUG_TYPE "packets"

//static cl::opt<bool>
//    DisablePacketizer("disable-packetizer", cl::Hidden,
//                      cl::desc("Disable PS2VPU packetizer pass"));
//
//static cl::opt<bool> Slot1Store("slot1-store-slot0-load", cl::Hidden,
//                                cl::init(true),
//                                cl::desc("Allow slot1 store and slot0 load"));
//
//static cl::opt<bool> PacketizeVolatiles(
//    "PS2VPU-packetize-volatiles", cl::Hidden, cl::init(true),
//    cl::desc("Allow non-solo packetization of volatile memory references"));
//
//static cl::opt<bool>
//    EnableGenAllInsnClass("enable-gen-insn", cl::Hidden,
//                          cl::desc("Generate all instruction with TC"));
//
//static cl::opt<bool>
//    DisableVecDblNVStores("disable-vecdbl-nv-stores", cl::Hidden,
//                          cl::desc("Disable vector double new-value-stores"));
//
//extern cl::opt<bool> ScheduleInlineAsm;

namespace llvm {

FunctionPass *createPS2VPUPacketizer(bool Minimal);
void initializePS2VPUPacketizerPass(PassRegistry &);

} // end namespace llvm

namespace {

class PS2VPUPacketizer : public MachineFunctionPass {
public:
  static char ID;

  PS2VPUPacketizer(bool Min = false) : MachineFunctionPass(ID), Minimal(Min) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addRequired<AAResultsWrapperPass>();
    AU.addRequired<MachineBranchProbabilityInfo>();
    AU.addRequired<MachineDominatorTree>();
    AU.addRequired<MachineLoopInfo>();
    AU.addPreserved<MachineDominatorTree>();
    AU.addPreserved<MachineLoopInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override { return "PS2VPU Packetizer"; }
  bool runOnMachineFunction(MachineFunction &Fn) override;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties().set(
        MachineFunctionProperties::Property::NoVRegs);
  }

private:
  const PS2VPUInstrInfo *HII = nullptr;
  const PS2VPURegisterInfo *HRI = nullptr;
  const bool Minimal = false;
};

} // end anonymous namespace

char PS2VPUPacketizer::ID = 0;

INITIALIZE_PASS_BEGIN(PS2VPUPacketizer, "PS2VPU-packetizer",
                      "PS2VPU Packetizer", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_DEPENDENCY(MachineBranchProbabilityInfo)
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfo)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(PS2VPUPacketizer, "PS2VPU-packetizer",
                    "PS2VPU Packetizer", false, false)

PS2VPUPacketizerList::PS2VPUPacketizerList(
    MachineFunction &MF, MachineLoopInfo &MLI, AAResults *AA,
    const MachineBranchProbabilityInfo *MBPI, bool Minimal)
    : VLIWPacketizerList(MF, MLI, AA), MBPI(MBPI), MLI(&MLI), Minimal(Minimal) {
  TII = MF.getSubtarget<PS2VPUSubtarget>().getInstrInfo();
  TRI = MF.getSubtarget<PS2VPUSubtarget>().getRegisterInfo();

  //addMutation(std::make_unique<PS2VPUSubtarget::UsrOverflowMutation>());
  //addMutation(std::make_unique<PS2VPUSubtarget::HVXMemLatencyMutation>());
  //addMutation(std::make_unique<PS2VPUSubtarget::BankConflictMutation>());
}

// Check if FirstI modifies a register that SecondI reads.
static bool hasWriteToReadDep(const MachineInstr &FirstI,
                              const MachineInstr &SecondI,
                              const TargetRegisterInfo *TRI) {
  for (auto &MO : FirstI.operands()) {
    if (!MO.isReg() || !MO.isDef())
      continue;
    Register R = MO.getReg();
    if (SecondI.readsRegister(R, TRI))
      return true;
  }
  return false;
}

static MachineBasicBlock::iterator
moveInstrOut(MachineInstr &MI, MachineBasicBlock::iterator BundleIt,
             bool Before) {
  MachineBasicBlock::instr_iterator InsertPt;
  if (Before)
    InsertPt = BundleIt.getInstrIterator();
  else
    InsertPt = std::next(BundleIt).getInstrIterator();

  MachineBasicBlock &B = *MI.getParent();
  // The instruction should at least be bundled with the preceding instruction
  // (there will always be one, i.e. BUNDLE, if nothing else).
  assert(MI.isBundledWithPred());
  if (MI.isBundledWithSucc()) {
    MI.clearFlag(MachineInstr::BundledSucc);
    MI.clearFlag(MachineInstr::BundledPred);
  } else {
    // If it's not bundled with the successor (i.e. it is the last one
    // in the bundle), then we can simply unbundle it from the predecessor,
    // which will take care of updating the predecessor's flag.
    MI.unbundleFromPred();
  }
  B.splice(InsertPt, &B, MI.getIterator());

  // Get the size of the bundle without asserting.
  MachineBasicBlock::const_instr_iterator I = BundleIt.getInstrIterator();
  MachineBasicBlock::const_instr_iterator E = B.instr_end();
  unsigned Size = 0;
  for (++I; I != E && I->isBundledWithPred(); ++I)
    ++Size;

  // If there are still two or more instructions, then there is nothing
  // else to be done.
  if (Size > 1)
    return BundleIt;

  // Otherwise, extract the single instruction out and delete the bundle.
  MachineBasicBlock::iterator NextIt = std::next(BundleIt);
  MachineInstr &SingleI = *BundleIt->getNextNode();
  SingleI.unbundleFromPred();
  assert(!SingleI.isBundledWithSucc());
  BundleIt->eraseFromParent();
  return NextIt;
}

bool PS2VPUPacketizer::runOnMachineFunction(MachineFunction &MF) {
  // FIXME: This pass causes verification failures.
  MF.getProperties().set(
      MachineFunctionProperties::Property::FailsVerification);

  auto &HST = MF.getSubtarget<PS2VPUSubtarget>();
  HII = HST.getInstrInfo();
  HRI = HST.getRegisterInfo();
  auto &MLI = getAnalysis<MachineLoopInfo>();
  auto *AA = &getAnalysis<AAResultsWrapperPass>().getAAResults();
  auto *MBPI = &getAnalysis<MachineBranchProbabilityInfo>();

  /*if (EnableGenAllInsnClass)
    HII->genAllInsnTimingClasses(MF);*/

  // Instantiate the packetizer.
  bool MinOnly = Minimal || /*DisablePacketizer ||*/ /*!HST.usePackets() ||*/
                 skipFunction(MF.getFunction());
  PS2VPUPacketizerList Packetizer(MF, MLI, AA, MBPI, MinOnly);

  // DFA state table should not be empty.
  assert(Packetizer.getResourceTracker() && "Empty DFA table!");

  // Loop over all basic blocks and remove KILL pseudo-instructions
  // These instructions confuse the dependence analysis. Consider:
  // D0 = ...   (Insn 0)
  // R0 = KILL R0, D0 (Insn 1)
  // R0 = ... (Insn 2)
  // Here, Insn 1 will result in the dependence graph not emitting an output
  // dependence between Insn 0 and Insn 2. This can lead to incorrect
  // packetization
  for (MachineBasicBlock &MB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MB))
      if (MI.isKill())
        MB.erase(&MI);
  }

  // TinyCore with Duplexes: Translate to big-instructions.
  //if (HST.isTinyCoreWithDuplex())
  //  HII->translateInstrsForDup(MF, true);

  // Loop over all of the basic blocks.
  for (auto &MB : MF) {
    auto Begin = MB.begin(), End = MB.end();
    while (Begin != End) {
      // Find the first non-boundary starting from the end of the last
      // scheduling region.
      MachineBasicBlock::iterator RB = Begin;
      while (RB != End && HII->isSchedulingBoundary(*RB, &MB, MF))
        ++RB;
      // Find the first boundary starting from the beginning of the new
      // region.
      MachineBasicBlock::iterator RE = RB;
      while (RE != End && !HII->isSchedulingBoundary(*RE, &MB, MF))
        ++RE;
      // Add the scheduling boundary if it's not block end.
      if (RE != End)
        ++RE;
      // If RB == End, then RE == End.
      if (RB != End)
        Packetizer.PacketizeMIs(&MB, RB, RE);

      Begin = RE;
    }
  }

  // TinyCore with Duplexes: Translate to tiny-instructions.
  //if (HST.isTinyCoreWithDuplex())
  //  HII->translateInstrsForDup(MF, false);

  /*Packetizer.unpacketizeSoloInstrs(MF);*/
  return true;
}

// Initialize packetizer flags.
void PS2VPUPacketizerList::initPacketizerState() {
}

// Ignore bundling of pseudo instructions.
bool PS2VPUPacketizerList::ignorePseudoInstruction(const MachineInstr &MI,
                                                    const MachineBasicBlock *) {
  if (MI.isDebugInstr())
    return true;

  if (MI.isCFIInstruction())
    return false;

  // We must print out inline assembly.
  if (MI.isInlineAsm())
    return false;

  if (MI.isImplicitDef())
    return false;

  return false;
}
bool PS2VPUPacketizerList::isSoloInstruction(const MachineInstr &MI) {
  if (MI.isEHLabel() || MI.isCFIInstruction())
    return true;

  return false;
}
bool isUpperInstruction(const MachineInstr &MI) {
  return MI.getDesc().TSFlags & (((uint64_t)1) << 63);
}
bool isLowerInstruction(const MachineInstr &MI) { return !isUpperInstruction(MI); }

    // SUI is the current instruction that is outside of the current packet.
// SUJ is the current instruction inside the current packet against which that
// SUI will be packetized.
bool PS2VPUPacketizerList::isLegalToPacketizeTogether(SUnit *SUI, SUnit *SUJ) {

    return isLowerInstruction(*SUJ->getInstr()) &&
         isUpperInstruction(*SUI->getInstr());
  //return true;
}

bool PS2VPUPacketizerList::isLegalToPruneDependencies(SUnit *SUI, SUnit *SUJ) {
  return false;
}

MachineBasicBlock::iterator
PS2VPUPacketizerList::addToPacket(MachineInstr &MI) {
  return VLIWPacketizerList::addToPacket(MI);
  //MachineBasicBlock::iterator MII = MI.getIterator();
  //MachineBasicBlock *MBB = MI.getParent();

  //if (CurrentPacketMIs.empty()) {
  //  PacketStalls = false;
  //  PacketStallCycles = 0;
  //}
  //PacketStalls |= producesStall(MI);
  //PacketStallCycles = std::max(PacketStallCycles, calcStall(MI));

  //if (MI.isImplicitDef()) {
  //  // Add to the packet to allow subsequent instructions to be checked
  //  // properly.
  //  CurrentPacketMIs.push_back(&MI);
  //  return MII;
  //}
  //assert(ResourceTracker->canReserveResources(MI));

  ///*bool ExtMI = HII->isExtended(MI) || HII->isConstExtended(MI);*/
  //bool Good = true;

  //if (GlueToNewValueJump) {
  //  MachineInstr &NvjMI = *++MII;
  //  // We need to put both instructions in the same packet: MI and NvjMI.
  //  // Either of them can require a constant extender. Try to add both to
  //  // the current packet, and if that fails, end the packet and start a
  //  // new one.
  //  ResourceTracker->reserveResources(MI);
  //  /*if (ExtMI)
  //    Good = tryAllocateResourcesForConstExt(true);*/

  //  /*bool ExtNvjMI = HII->isExtended(NvjMI) || HII->isConstExtended(NvjMI);*/
  //  if (Good) {
  //    if (ResourceTracker->canReserveResources(NvjMI))
  //      ResourceTracker->reserveResources(NvjMI);
  //    else
  //      Good = false;
  //  }
  //  /*if (Good && ExtNvjMI)
  //    Good = tryAllocateResourcesForConstExt(true);*/

  //  if (!Good) {
  //    endPacket(MBB, MI);
  //    assert(ResourceTracker->canReserveResources(MI));
  //    ResourceTracker->reserveResources(MI);
  //    /*if (ExtMI) {
  //      assert(canReserveResourcesForConstExt());
  //      tryAllocateResourcesForConstExt(true);
  //    }*/
  //    assert(ResourceTracker->canReserveResources(NvjMI));
  //    ResourceTracker->reserveResources(NvjMI);
  //    /*if (ExtNvjMI) {
  //      assert(canReserveResourcesForConstExt());
  //      reserveResourcesForConstExt();
  //    }*/
  //  }
  //  CurrentPacketMIs.push_back(&MI);
  //  CurrentPacketMIs.push_back(&NvjMI);
  //  return MII;
  //}

  //ResourceTracker->reserveResources(MI);
  ///*if (ExtMI && !tryAllocateResourcesForConstExt(true)) {
  //  endPacket(MBB, MI);
  //  if (PromotedToDotNew)
  //    demoteToDotOld(MI);
  //  if (GlueAllocframeStore) {
  //    useCalleesSP(MI);
  //    GlueAllocframeStore = false;
  //  }
  //  ResourceTracker->reserveResources(MI);
  //  reserveResourcesForConstExt();
  //}*/

  //CurrentPacketMIs.push_back(&MI);
  //return MII;
}

void PS2VPUPacketizerList::endPacket(MachineBasicBlock *MBB,
                                      MachineBasicBlock::iterator EndMI) {

    LLVM_DEBUG({
    if (!CurrentPacketMIs.empty()) {
      dbgs() << "Finalizing packet:\n";
      unsigned Idx = 0;
      for (MachineInstr *MI : CurrentPacketMIs) {
        unsigned R = ResourceTracker->getUsedResources(Idx++);
        dbgs() << " * [res:0x" << utohexstr(R) << "] " << *MI;
      }
    }
  });

  // Add NOP instructions if bundle is incomplete
  if (CurrentPacketMIs.size() == 1) {
    llvm::MachineInstr &MI1 = *CurrentPacketMIs[0];
    const llvm::DebugLoc &dl = CurrentPacketMIs[0]->getDebugLoc();
    if (isLowerInstruction(MI1)) {
      auto MI2 = BuildMI(*MBB, MI1.getIterator(), dl, TII->get(PS2VPUNS::NOP));
      CurrentPacketMIs.insert(CurrentPacketMIs.begin(), MI2);
    } else {
      auto MI2 =
          BuildMI(*MBB, EndMI, dl, TII->get(PS2VPUNS::MOVEv4), PS2VPUNS::VF0)
              .addReg(PS2VPUNS::VF0);
      CurrentPacketMIs.push_back(MI2);
    }
  }

  // Reorder if in wrong order
  if (CurrentPacketMIs.size() == 2) {
    if (isLowerInstruction(*CurrentPacketMIs[0])) {
      MBB->splice(CurrentPacketMIs[0]->getIterator(), MBB,
                  CurrentPacketMIs[1]->getIterator());
      std::swap(CurrentPacketMIs[0], CurrentPacketMIs[1]);
    }
  }

  if (CurrentPacketMIs.size() > 1) {
    MachineBasicBlock::instr_iterator FirstMI(CurrentPacketMIs.front());
    MachineBasicBlock::instr_iterator LastMI(EndMI.getInstrIterator());
    finalizeBundle(*MBB, FirstMI, LastMI);
  }

  CurrentPacketMIs.clear();
  ResourceTracker->clearResources();
  LLVM_DEBUG(dbgs() << "End packet\n");
}

bool PS2VPUPacketizerList::shouldAddToPacket(const MachineInstr &MI) {
  if (Minimal)
    return false;
  return true;
}

//===----------------------------------------------------------------------===//
//                         Public Constructor Functions
//===----------------------------------------------------------------------===//

FunctionPass *llvm::createPS2VPUPacketizer(bool Minimal) {
  return new PS2VPUPacketizer(Minimal);
}
