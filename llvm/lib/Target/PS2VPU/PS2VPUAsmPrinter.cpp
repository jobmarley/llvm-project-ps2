//===-- PS2VPUAsmPrinter.cpp - PS2VPU LLVM assembly writer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format PS2VPU assembly language.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUInstPrinter.h"
#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "MCTargetDesc/PS2VPUTargetStreamer.h"
#include "MCTargetDesc/PS2VPUMCInstrInfo.h"
#include "PS2VPU.h"
#include "PS2VPUInstrInfo.h"
#include "PS2VPUTargetMachine.h"
#include "TargetInfo/PS2VPUTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
class PS2VPUAsmPrinter : public AsmPrinter {
  PS2VPUTargetStreamer &getTargetStreamer() {
    return static_cast<PS2VPUTargetStreamer &>(
        *OutStreamer->getTargetStreamer());
  }
  const PS2VPUSubtarget *Subtarget = nullptr;



public:
  explicit PS2VPUAsmPrinter(TargetMachine &TM,
                           std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)) {}

  bool runOnMachineFunction(MachineFunction &Fn) override {
    Subtarget = &Fn.getSubtarget<PS2VPUSubtarget>();
    const bool Modified = AsmPrinter::runOnMachineFunction(Fn);
    // Emit the XRay table for this function.
    emitXRayTable();

    return Modified;
  }

  StringRef getPassName() const override { return "PS2VPU Assembly Printer"; }

  void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
  void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                       const char *Modifier = nullptr);

  void emitFunctionBodyStart() override;
  void emitInstruction(const MachineInstr *MI) override;

  static const char *getRegisterName(unsigned RegNo) {
    return PS2VPUInstPrinter::getRegisterName(RegNo);
  }

  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &O) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                             const char *ExtraCode, raw_ostream &O) override;

  void LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                 const MCSubtargetInfo &STI);
};
} // end of anonymous namespace

static MCOperand createPS2VPUMCOperand(PS2VPUMCExpr::VariantKind Kind,
                                      MCSymbol *Sym, MCContext &OutContext) {
  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::create(Sym, OutContext);
  const PS2VPUMCExpr *expr = PS2VPUMCExpr::create(Kind, MCSym, OutContext);
  return MCOperand::createExpr(expr);
}
static MCOperand createPCXCallOP(MCSymbol *Label, MCContext &OutContext) {
  return createPS2VPUMCOperand(PS2VPUMCExpr::VK_PS2VPU_WDISP30, Label, OutContext);
}

static MCOperand createPCXRelExprOp(PS2VPUMCExpr::VariantKind Kind,
                                    MCSymbol *GOTLabel, MCSymbol *StartLabel,
                                    MCSymbol *CurLabel, MCContext &OutContext) {
  const MCSymbolRefExpr *GOT = MCSymbolRefExpr::create(GOTLabel, OutContext);
  const MCSymbolRefExpr *Start =
      MCSymbolRefExpr::create(StartLabel, OutContext);
  const MCSymbolRefExpr *Cur = MCSymbolRefExpr::create(CurLabel, OutContext);

  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Cur, Start, OutContext);
  const MCBinaryExpr *Add = MCBinaryExpr::createAdd(GOT, Sub, OutContext);
  const PS2VPUMCExpr *expr = PS2VPUMCExpr::create(Kind, Add, OutContext);
  return MCOperand::createExpr(expr);
}

//static void EmitCall(MCStreamer &OutStreamer, MCOperand &Callee,
//                     const MCSubtargetInfo &STI) {
//  MCInst CallInst;
//  CallInst.setOpcode(SP::CALL);
//  CallInst.addOperand(Callee);
//  OutStreamer.emitInstruction(CallInst, STI);
//}
//
//static void EmitSETHI(MCStreamer &OutStreamer, MCOperand &Imm, MCOperand &RD,
//                      const MCSubtargetInfo &STI) {
//  MCInst SETHIInst;
//  SETHIInst.setOpcode(SP::SETHIi);
//  SETHIInst.addOperand(RD);
//  SETHIInst.addOperand(Imm);
//  OutStreamer.emitInstruction(SETHIInst, STI);
//}

static void EmitBinary(MCStreamer &OutStreamer, unsigned Opcode, MCOperand &RS1,
                       MCOperand &Src2, MCOperand &RD,
                       const MCSubtargetInfo &STI) {
  MCInst Inst;
  Inst.setOpcode(Opcode);
  Inst.addOperand(RD);
  Inst.addOperand(RS1);
  Inst.addOperand(Src2);
  OutStreamer.emitInstruction(Inst, STI);
}

//static void EmitOR(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &Imm,
//                   MCOperand &RD, const MCSubtargetInfo &STI) {
//  EmitBinary(OutStreamer, SP::ORri, RS1, Imm, RD, STI);
//}
//
//static void EmitADD(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &RS2,
//                    MCOperand &RD, const MCSubtargetInfo &STI) {
//  EmitBinary(OutStreamer, SP::ADDrr, RS1, RS2, RD, STI);
//}
//
//static void EmitSHL(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &Imm,
//                    MCOperand &RD, const MCSubtargetInfo &STI) {
//  EmitBinary(OutStreamer, SP::SLLri, RS1, Imm, RD, STI);
//}

//static void EmitHiLo(MCStreamer &OutStreamer, MCSymbol *GOTSym,
//                     PS2VPUMCExpr::VariantKind HiKind,
//                     PS2VPUMCExpr::VariantKind LoKind, MCOperand &RD,
//                     MCContext &OutContext, const MCSubtargetInfo &STI) {
//
//  MCOperand hi = createPS2VPUMCOperand(HiKind, GOTSym, OutContext);
//  MCOperand lo = createPS2VPUMCOperand(LoKind, GOTSym, OutContext);
//  EmitSETHI(OutStreamer, hi, RD, STI);
//  EmitOR(OutStreamer, RD, lo, RD, STI);
//}

//void PS2VPUAsmPrinter::LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
//                                                const MCSubtargetInfo &STI) {
//  MCSymbol *GOTLabel =
//      OutContext.getOrCreateSymbol(Twine("_GLOBAL_OFFSET_TABLE_"));
//
//  const MachineOperand &MO = MI->getOperand(0);
//  assert(MO.getReg() != SP::O7 && "%o7 is assigned as destination for getpcx!");
//
//  MCOperand MCRegOP = MCOperand::createReg(MO.getReg());
//
//  if (!isPositionIndependent()) {
//    // Just load the address of GOT to MCRegOP.
//    switch (TM.getCodeModel()) {
//    default:
//      llvm_unreachable("Unsupported absolute code model");
//    case CodeModel::Small:
//      EmitHiLo(*OutStreamer, GOTLabel, PS2VPUMCExpr::VK_PS2VPU_HI,
//               PS2VPUMCExpr::VK_PS2VPU_LO, MCRegOP, OutContext, STI);
//      break;
//    case CodeModel::Medium: {
//      EmitHiLo(*OutStreamer, GOTLabel, PS2VPUMCExpr::VK_PS2VPU_H44,
//               PS2VPUMCExpr::VK_PS2VPU_M44, MCRegOP, OutContext, STI);
//      MCOperand imm =
//          MCOperand::createExpr(MCConstantExpr::create(12, OutContext));
//      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
//      MCOperand lo =
//          createPS2VPUMCOperand(PS2VPUMCExpr::VK_PS2VPU_L44, GOTLabel, OutContext);
//      EmitOR(*OutStreamer, MCRegOP, lo, MCRegOP, STI);
//      break;
//    }
//    case CodeModel::Large: {
//      EmitHiLo(*OutStreamer, GOTLabel, PS2VPUMCExpr::VK_PS2VPU_HH,
//               PS2VPUMCExpr::VK_PS2VPU_HM, MCRegOP, OutContext, STI);
//      MCOperand imm =
//          MCOperand::createExpr(MCConstantExpr::create(32, OutContext));
//      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
//      // Use register %o7 to load the lower 32 bits.
//      MCOperand RegO7 = MCOperand::createReg(SP::O7);
//      EmitHiLo(*OutStreamer, GOTLabel, PS2VPUMCExpr::VK_PS2VPU_HI,
//               PS2VPUMCExpr::VK_PS2VPU_LO, RegO7, OutContext, STI);
//      EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
//    }
//    }
//    return;
//  }
//
//  MCSymbol *StartLabel = OutContext.createTempSymbol();
//  MCSymbol *EndLabel = OutContext.createTempSymbol();
//  MCSymbol *SethiLabel = OutContext.createTempSymbol();
//
//  MCOperand RegO7 = MCOperand::createReg(SP::O7);
//
//  // <StartLabel>:
//  //   call <EndLabel>
//  // <SethiLabel>:
//  //     sethi %hi(_GLOBAL_OFFSET_TABLE_+(<SethiLabel>-<StartLabel>)), <MO>
//  // <EndLabel>:
//  //   or  <MO>, %lo(_GLOBAL_OFFSET_TABLE_+(<EndLabel>-<StartLabel>))), <MO>
//  //   add <MO>, %o7, <MO>
//
//  OutStreamer->emitLabel(StartLabel);
//  MCOperand Callee = createPCXCallOP(EndLabel, OutContext);
//  EmitCall(*OutStreamer, Callee, STI);
//  OutStreamer->emitLabel(SethiLabel);
//  MCOperand hiImm = createPCXRelExprOp(PS2VPUMCExpr::VK_PS2VPU_PC22, GOTLabel,
//                                       StartLabel, SethiLabel, OutContext);
//  EmitSETHI(*OutStreamer, hiImm, MCRegOP, STI);
//  OutStreamer->emitLabel(EndLabel);
//  MCOperand loImm = createPCXRelExprOp(PS2VPUMCExpr::VK_PS2VPU_PC10, GOTLabel,
//                                       StartLabel, EndLabel, OutContext);
//  EmitOR(*OutStreamer, MCRegOP, loImm, MCRegOP, STI);
//  EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
//}

// Create an MCInst from a MachineInstr
void PS2VPULowerToMC(const MCInstrInfo &MCII, const MachineInstr *MI,
                            MCInst &MCB, PS2VPUAsmPrinter &AP) {
  MCInst *MCI = AP.OutContext.createMCInst();
  MCI->setOpcode(MI->getOpcode());
  assert(MCI->getOpcode() == static_cast<unsigned>(MI->getOpcode()) &&
         "MCI opcode should have been set on construction");

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCO;

    switch (MO.getType()) {
    default:
      MI->print(errs());
      llvm_unreachable("unknown operand type");
    case MachineOperand::MO_RegisterMask:
      continue;
    case MachineOperand::MO_Register:
      // Ignore all implicit register operands.
      if (MO.isImplicit())
        continue;
      MCO = MCOperand::createReg(MO.getReg());
      break;
    case MachineOperand::MO_FPImmediate: {
      APFloat Val = MO.getFPImm()->getValueAPF();
      MCO = MCOperand::createSFPImm(*Val.bitcastToAPInt().getRawData());
      break;
    }
    case MachineOperand::MO_Immediate: {
      MCO = MCOperand::createImm(MO.getImm());
      break;
    }
    case MachineOperand::MO_MachineBasicBlock: {
      MCExpr const *Expr =
          MCSymbolRefExpr::create(MO.getMBB()->getSymbol(), AP.OutContext);
      Expr = PS2VPUMCExpr::create(PS2VPUMCExpr::VariantKind::VK_PS2VPU_None,
                                  Expr, AP.OutContext);
      MCO = MCOperand::createExpr(Expr);
      break;
    }
    /*case MachineOperand::MO_GlobalAddress:
      MCO = GetSymbolRef(MO, AP.getSymbol(MO.getGlobal()), AP, MustExtend);
      break;
    case MachineOperand::MO_ExternalSymbol:
      MCO = GetSymbolRef(MO, AP.GetExternalSymbolSymbol(MO.getSymbolName()), AP,
                         MustExtend);
      break;
    case MachineOperand::MO_JumpTableIndex:
      MCO = GetSymbolRef(MO, AP.GetJTISymbol(MO.getIndex()), AP, MustExtend);
      break;
    case MachineOperand::MO_ConstantPoolIndex:
      MCO = GetSymbolRef(MO, AP.GetCPISymbol(MO.getIndex()), AP, MustExtend);
      break;
    case MachineOperand::MO_BlockAddress:
      MCO = GetSymbolRef(MO, AP.GetBlockAddressSymbol(MO.getBlockAddress()), AP,
                         MustExtend);
      break;*/
    }

    MCI->addOperand(MCO);
  }
  /*AP.HexagonProcessInstruction(*MCI, *MI);*/
  MCB.addOperand(MCOperand::createInst(MCI));
}

void PS2VPUAsmPrinter::emitInstruction(const MachineInstr *MI) {
  PS2VPU_MC::verifyInstructionPredicates(MI->getOpcode(),
                                        getSubtargetInfo().getFeatureBits());
  MCInst MCB;
  MCB.setOpcode(PS2VPUNS::BUNDLE);
  MCB.addOperand(MCOperand::createImm(0));
  const MCInstrInfo &MCII = *Subtarget->getInstrInfo();

  if (MI->isBundle()) {
    const MachineBasicBlock *MBB = MI->getParent();
    MachineBasicBlock::const_instr_iterator MII = MI->getIterator();

    for (++MII; MII != MBB->instr_end() && MII->isInsideBundle(); ++MII)
      if (!MII->isDebugInstr() && !MII->isImplicitDef())
        PS2VPULowerToMC(MCII, &*MII, MCB, *this);
  } else {
    PS2VPULowerToMC(MCII, MI, MCB, *this);
  }

  const MachineFunction &MF = *MI->getParent()->getParent();
  const auto &HII = *MF.getSubtarget<PS2VPUSubtarget>().getInstrInfo();
  //if (MI->isBundle() && HII.getBundleNoShuf(*MI))
  //  PS2VPUMCInstrInfo::setMemReorderDisabled(MCB);

  MCContext &Ctx = OutStreamer->getContext();
  /*bool Ok = PS2VPUMCInstrInfo::canonicalizePacket(MCII, *Subtarget, Ctx, MCB,
                                                   nullptr);
  assert(Ok);
  (void)Ok;*/
  if (PS2VPUMCInstrInfo::bundleSize(MCB) == 0)
    return;
  OutStreamer->emitInstruction(MCB, getSubtargetInfo());
}

void PS2VPUAsmPrinter::emitFunctionBodyStart() {
  /*if (!MF->getSubtarget<PS2VPUSubtarget>().is64Bit())
    return;*/

 /* const MachineRegisterInfo &MRI = MF->getRegInfo();
  const unsigned globalRegs[] = {SP::G2, SP::G3, SP::G6, SP::G7, 0};
  for (unsigned i = 0; globalRegs[i] != 0; ++i) {
    unsigned reg = globalRegs[i];
    if (MRI.use_empty(reg))
      continue;

    if (reg == SP::G6 || reg == SP::G7)
      getTargetStreamer().emitPS2VPURegisterIgnore(reg);
    else
      getTargetStreamer().emitPS2VPURegisterScratch(reg);
  }*/
}

void PS2VPUAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                   raw_ostream &O) {
  const DataLayout &DL = getDataLayout();
  const MachineOperand &MO = MI->getOperand(opNum);
  PS2VPUMCExpr::VariantKind TF = (PS2VPUMCExpr::VariantKind)MO.getTargetFlags();

#ifndef NDEBUG
  // Verify the target flags.
  if (MO.isGlobal() || MO.isSymbol() || MO.isCPI()) {
    /*if (MI->getOpcode() == SP::CALL)
      assert(TF == PS2VPUMCExpr::VK_PS2VPU_None &&
             "Cannot handle target flags on call address");
    else if (MI->getOpcode() == SP::SETHIi || MI->getOpcode() == SP::SETHIXi)
      assert(
          (TF == PS2VPUMCExpr::VK_PS2VPU_HI || TF == PS2VPUMCExpr::VK_PS2VPU_H44 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_HH || TF == PS2VPUMCExpr::VK_PS2VPU_LM ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_GD_HI22 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_HI22 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDO_HIX22 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_IE_HI22 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LE_HIX22) &&
          "Invalid target flags for address operand on sethi");
    else if (MI->getOpcode() == SP::TLS_CALL)
      assert((TF == PS2VPUMCExpr::VK_PS2VPU_None ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_GD_CALL ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_CALL) &&
             "Cannot handle target flags on tls call address");
    else if (MI->getOpcode() == SP::TLS_ADDrr)
      assert((TF == PS2VPUMCExpr::VK_PS2VPU_TLS_GD_ADD ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_ADD ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDO_ADD ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_IE_ADD) &&
             "Cannot handle target flags on add for TLS");
    else if (MI->getOpcode() == SP::TLS_LDrr)
      assert(TF == PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LD &&
             "Cannot handle target flags on ld for TLS");
    else if (MI->getOpcode() == SP::TLS_LDXrr)
      assert(TF == PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LDX &&
             "Cannot handle target flags on ldx for TLS");
    else if (MI->getOpcode() == SP::XORri || MI->getOpcode() == SP::XORXri)
      assert((TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDO_LOX10 ||
              TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LE_LOX10) &&
             "Cannot handle target flags on xor for TLS");
    else
      assert(
          (TF == PS2VPUMCExpr::VK_PS2VPU_LO || TF == PS2VPUMCExpr::VK_PS2VPU_M44 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_L44 || TF == PS2VPUMCExpr::VK_PS2VPU_HM ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_GD_LO10 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_LO10 ||
           TF == PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LO10) &&
          "Invalid target flags for small address operand");*/
  }
#endif

  bool CloseParen = PS2VPUMCExpr::printVariantKind(O, TF);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << "%" << StringRef(getRegisterName(MO.getReg())).lower();
    break;

  case MachineOperand::MO_Immediate:
    O << MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MO.getMBB()->getSymbol()->print(O, MAI);
    return;
  case MachineOperand::MO_GlobalAddress:
    PrintSymbolOperand(MO, O);
    break;
  case MachineOperand::MO_BlockAddress:
    O << GetBlockAddressSymbol(MO.getBlockAddress())->getName();
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << MO.getSymbolName();
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << DL.getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << "_"
      << MO.getIndex();
    break;
  case MachineOperand::MO_Metadata:
    MO.getMetadata()->printAsOperand(O, MMI->getModule());
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }
  if (CloseParen)
    O << ")";
}

void PS2VPUAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                      raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, O);

  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    O << ", ";
    printOperand(MI, opNum + 1, O);
    return;
  }

  if (MI->getOperand(opNum + 1).isReg() &&
      MI->getOperand(opNum + 1).getReg() == PS2VPUNS::VI0)
    return; // don't print "+%g0"
  if (MI->getOperand(opNum + 1).isImm() &&
      MI->getOperand(opNum + 1).getImm() == 0)
    return; // don't print "+0"

  O << "+";
  printOperand(MI, opNum + 1, O);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool PS2VPUAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                      const char *ExtraCode, raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0)
      return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, O);
    case 'f':
    case 'r':
      break;
    }
  }

  printOperand(MI, OpNo, O);

  return false;
}

bool PS2VPUAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                            unsigned OpNo,
                                            const char *ExtraCode,
                                            raw_ostream &O) {
  if (ExtraCode && ExtraCode[0])
    return true; // Unknown modifier

  O << '[';
  printMemOperand(MI, OpNo, O);
  O << ']';

  return false;
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePS2VPUAsmPrinter() {
  RegisterAsmPrinter<PS2VPUAsmPrinter> X(getThePS2VPUTarget());
}
