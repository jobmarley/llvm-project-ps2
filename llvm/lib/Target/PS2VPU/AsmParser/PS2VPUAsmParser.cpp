//===-- PS2VPUAsmParser.cpp - Parse PS2VPU assembly to MCInst instructions --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "MCTargetDesc/PS2VPUMCTargetDesc.h"
#include "TargetInfo/PS2VPUTargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>

using namespace llvm;

// The generated AsmMatcher PS2VPUGenAsmMatcher uses "PS2VPU" as the target
// namespace. But PS2VPU backend uses "SP" as its namespace.
namespace llvm {
namespace PS2VPU {

using namespace PS2VPUNS;

} // end namespace PS2VPU
} // end namespace llvm

namespace {

class PS2VPUOperand;

class PS2VPUAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;

  enum class TailRelocKind { Load_GOT, Add_TLS, Load_TLS, Call_TLS };

  /// @name Auto-generated Match Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "PS2VPUGenAsmMatcher.inc"

  /// }

  // public interface of the MCTargetAsmParser.
  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;
  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;
  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;
  bool ParseDirective(AsmToken DirectiveID) override;

  unsigned validateTargetOperandClass(MCParsedAsmOperand &Op,
                                      unsigned Kind) override;

  // Custom parse functions for PS2VPU specific operands.
  OperandMatchResultTy parseMEMOperand(OperandVector &Operands);

  OperandMatchResultTy parseMembarTag(OperandVector &Operands);

  OperandMatchResultTy parseVectorField(OperandVector &Operands);

  template <TailRelocKind Kind>
  OperandMatchResultTy parseTailRelocSym(OperandVector &Operands);

  template <unsigned N>
  OperandMatchResultTy parseShiftAmtImm(OperandVector &Operands);

  OperandMatchResultTy parseCallTarget(OperandVector &Operands);

  OperandMatchResultTy parseOperand(OperandVector &Operands, StringRef Name);

  OperandMatchResultTy
  parsePS2VPUAsmOperand(std::unique_ptr<PS2VPUOperand> &Operand,
                       bool isCall = false);

  OperandMatchResultTy parseBranchModifiers(OperandVector &Operands);

  // Helper function for dealing with %lo / %hi in PIC mode.
  const PS2VPUMCExpr *adjustPICRelocation(PS2VPUMCExpr::VariantKind VK,
                                         const MCExpr *subExpr);

  // returns true if Tok is matched to a register and returns register in RegNo.
  bool matchRegisterName(const AsmToken &Tok, unsigned &RegNo,
                         unsigned &RegKind);

  bool matchPS2VPUAsmModifiers(const MCExpr *&EVal, SMLoc &EndLoc);

  bool expandSET(MCInst &Inst, SMLoc IDLoc,
                 SmallVectorImpl<MCInst> &Instructions);

  SMLoc getLoc() const { return getParser().getTok().getLoc(); }

public:
  PS2VPUAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser,
                 const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, sti, MII), Parser(parser) {
    Parser.addAliasForDirective(".half", ".2byte");
    Parser.addAliasForDirective(".uahalf", ".2byte");
    Parser.addAliasForDirective(".word", ".4byte");
    Parser.addAliasForDirective(".uaword", ".4byte");
    //Parser.addAliasForDirective(".nword", is64Bit() ? ".8byte" : ".4byte");
    //if (is64Bit())
    //  Parser.addAliasForDirective(".xword", ".8byte");

    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));
  }
};

} // end anonymous namespace

static const MCPhysReg IntRegs[16] = {
    PS2VPU::VI0,  PS2VPU::VI1,  PS2VPU::VI2,  PS2VPU::VI3,
    PS2VPU::VI4,  PS2VPU::VI5,  PS2VPU::VI6,  PS2VPU::VI7,
    PS2VPU::VI8,  PS2VPU::VI9,  PS2VPU::VI10, PS2VPU::VI11,
    PS2VPU::VI12, PS2VPU::VI13, PS2VPU::VI14, PS2VPU::VI15,
};
//
//static const MCPhysReg FloatRegs[32] = {
//    PS2VPU::F0,  PS2VPU::F1,  PS2VPU::F2,  PS2VPU::F3,  PS2VPU::F4,  PS2VPU::F5,
//    PS2VPU::F6,  PS2VPU::F7,  PS2VPU::F8,  PS2VPU::F9,  PS2VPU::F10, PS2VPU::F11,
//    PS2VPU::F12, PS2VPU::F13, PS2VPU::F14, PS2VPU::F15, PS2VPU::F16, PS2VPU::F17,
//    PS2VPU::F18, PS2VPU::F19, PS2VPU::F20, PS2VPU::F21, PS2VPU::F22, PS2VPU::F23,
//    PS2VPU::F24, PS2VPU::F25, PS2VPU::F26, PS2VPU::F27, PS2VPU::F28, PS2VPU::F29,
//    PS2VPU::F30, PS2VPU::F31};
//
//static const MCPhysReg DoubleRegs[32] = {
//    PS2VPU::D0,  PS2VPU::D1,  PS2VPU::D2,  PS2VPU::D3,  PS2VPU::D4,  PS2VPU::D5,
//    PS2VPU::D6,  PS2VPU::D7,  PS2VPU::D8,  PS2VPU::D9,  PS2VPU::D10, PS2VPU::D11,
//    PS2VPU::D12, PS2VPU::D13, PS2VPU::D14, PS2VPU::D15, PS2VPU::D16, PS2VPU::D17,
//    PS2VPU::D18, PS2VPU::D19, PS2VPU::D20, PS2VPU::D21, PS2VPU::D22, PS2VPU::D23,
//    PS2VPU::D24, PS2VPU::D25, PS2VPU::D26, PS2VPU::D27, PS2VPU::D28, PS2VPU::D29,
//    PS2VPU::D30, PS2VPU::D31};
//
//static const MCPhysReg QuadFPRegs[32] = {
//    PS2VPU::Q0,  PS2VPU::Q1,  PS2VPU::Q2,  PS2VPU::Q3, PS2VPU::Q4,  PS2VPU::Q5,
//    PS2VPU::Q6,  PS2VPU::Q7,  PS2VPU::Q8,  PS2VPU::Q9, PS2VPU::Q10, PS2VPU::Q11,
//    PS2VPU::Q12, PS2VPU::Q13, PS2VPU::Q14, PS2VPU::Q15};
//
//static const MCPhysReg ASRRegs[32] = {
//    SP::Y,     SP::ASR1,  SP::ASR2,  SP::ASR3,  SP::ASR4,  SP::ASR5,  SP::ASR6,
//    SP::ASR7,  SP::ASR8,  SP::ASR9,  SP::ASR10, SP::ASR11, SP::ASR12, SP::ASR13,
//    SP::ASR14, SP::ASR15, SP::ASR16, SP::ASR17, SP::ASR18, SP::ASR19, SP::ASR20,
//    SP::ASR21, SP::ASR22, SP::ASR23, SP::ASR24, SP::ASR25, SP::ASR26, SP::ASR27,
//    SP::ASR28, SP::ASR29, SP::ASR30, SP::ASR31};
//
//static const MCPhysReg IntPairRegs[] = {
//    PS2VPU::G0_G1, PS2VPU::G2_G3, PS2VPU::G4_G5, PS2VPU::G6_G7,
//    PS2VPU::O0_O1, PS2VPU::O2_O3, PS2VPU::O4_O5, PS2VPU::O6_O7,
//    PS2VPU::L0_L1, PS2VPU::L2_L3, PS2VPU::L4_L5, PS2VPU::L6_L7,
//    PS2VPU::I0_I1, PS2VPU::I2_I3, PS2VPU::I4_I5, PS2VPU::I6_I7};
//
//static const MCPhysReg CoprocRegs[32] = {
//    PS2VPU::C0,  PS2VPU::C1,  PS2VPU::C2,  PS2VPU::C3,  PS2VPU::C4,  PS2VPU::C5,
//    PS2VPU::C6,  PS2VPU::C7,  PS2VPU::C8,  PS2VPU::C9,  PS2VPU::C10, PS2VPU::C11,
//    PS2VPU::C12, PS2VPU::C13, PS2VPU::C14, PS2VPU::C15, PS2VPU::C16, PS2VPU::C17,
//    PS2VPU::C18, PS2VPU::C19, PS2VPU::C20, PS2VPU::C21, PS2VPU::C22, PS2VPU::C23,
//    PS2VPU::C24, PS2VPU::C25, PS2VPU::C26, PS2VPU::C27, PS2VPU::C28, PS2VPU::C29,
//    PS2VPU::C30, PS2VPU::C31};
//
//static const MCPhysReg CoprocPairRegs[] = {
//    PS2VPU::C0_C1,   PS2VPU::C2_C3,   PS2VPU::C4_C5,   PS2VPU::C6_C7,
//    PS2VPU::C8_C9,   PS2VPU::C10_C11, PS2VPU::C12_C13, PS2VPU::C14_C15,
//    PS2VPU::C16_C17, PS2VPU::C18_C19, PS2VPU::C20_C21, PS2VPU::C22_C23,
//    PS2VPU::C24_C25, PS2VPU::C26_C27, PS2VPU::C28_C29, PS2VPU::C30_C31};

namespace {

/// PS2VPUOperand - Instances of this class represent a parsed PS2VPU machine
/// instruction.
class PS2VPUOperand : public MCParsedAsmOperand {
public:
  enum RegisterKind {
    rk_None,
    rk_IntReg,
    rk_IntPairReg,
    rk_FloatReg,
    rk_DoubleReg,
    rk_QuadReg,
    rk_CoprocReg,
    rk_CoprocPairReg,
    rk_Special,
  };

private:
  enum KindTy {
    k_Token,
    k_Register,
    k_Immediate,
    k_MemoryReg,
    k_MemoryImm
  } Kind;

  SMLoc StartLoc, EndLoc;

  struct Token {
    const char *Data;
    unsigned Length;
  };

  struct RegOp {
    unsigned RegNum;
    RegisterKind Kind;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct MemOp {
    unsigned Base;
    unsigned OffsetReg;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct RegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };

public:
  PS2VPUOperand(KindTy K) : Kind(K) {}

  bool isToken() const override { return Kind == k_Token; }
  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isMem() const override { return isMEMrr() || isMEMri(); }
  bool isMEMrr() const { return Kind == k_MemoryReg; }
  bool isMEMri() const { return Kind == k_MemoryImm; }
  bool isMembarTag() const { return Kind == k_Immediate; }
  bool isTailRelocSym() const { return Kind == k_Immediate; }

  bool isCallTarget() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return CE->getValue() % 4 == 0;

    return true;
  }

  bool isShiftAmtImm5() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<5>(CE->getValue());

    return false;
  }

  bool isShiftAmtImm6() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<6>(CE->getValue());

    return false;
  }

  bool isIntReg() const {
    return (Kind == k_Register && Reg.Kind == rk_IntReg);
  }

  bool isFloatReg() const {
    return (Kind == k_Register && Reg.Kind == rk_FloatReg);
  }

  bool isFloatOrDoubleReg() const {
    return (Kind == k_Register &&
            (Reg.Kind == rk_FloatReg || Reg.Kind == rk_DoubleReg));
  }

  bool isCoprocReg() const {
    return (Kind == k_Register && Reg.Kind == rk_CoprocReg);
  }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert((Kind == k_Register) && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert((Kind == k_MemoryReg || Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Base;
  }

  unsigned getMemOffsetReg() const {
    assert((Kind == k_MemoryReg) && "Invalid access!");
    return Mem.OffsetReg;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Off;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override { return EndLoc; }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Token:
      OS << "Token: " << getToken() << "\n";
      break;
    case k_Register:
      OS << "Reg: #" << getReg() << "\n";
      break;
    case k_Immediate:
      OS << "Imm: " << getImm() << "\n";
      break;
    case k_MemoryReg:
      OS << "Mem: " << getMemBase() << "+" << getMemOffsetReg() << "\n";
      break;
    case k_MemoryImm:
      assert(getMemOff() != nullptr);
      OS << "Mem: " << getMemBase() << "+" << *getMemOff() << "\n";
      break;
    }
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addShiftAmtImm5Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }
  void addShiftAmtImm6Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    // Add as immediate when possible.  Null MCExpr = 0.
    if (!Expr)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addMEMrrOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    assert(getMemOffsetReg() != 0 && "Invalid offset");
    Inst.addOperand(MCOperand::createReg(getMemOffsetReg()));
  }

  void addMEMriOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  void addMembarTagOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addCallTargetOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addTailRelocSymOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  static std::unique_ptr<PS2VPUOperand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<PS2VPUOperand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<PS2VPUOperand> CreateReg(unsigned RegNum, unsigned Kind,
                                                 SMLoc S, SMLoc E) {
    auto Op = std::make_unique<PS2VPUOperand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->Reg.Kind = (PS2VPUOperand::RegisterKind)Kind;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<PS2VPUOperand> CreateImm(const MCExpr *Val, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<PS2VPUOperand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  /*static bool MorphToIntPairReg(PS2VPUOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_IntReg);
    unsigned regIdx = 32;
    if (Reg >= PS2VPU::G0 && Reg <= PS2VPU::G7)
      regIdx = Reg - PS2VPU::G0;
    else if (Reg >= PS2VPU::O0 && Reg <= PS2VPU::O7)
      regIdx = Reg - PS2VPU::O0 + 8;
    else if (Reg >= PS2VPU::L0 && Reg <= PS2VPU::L7)
      regIdx = Reg - PS2VPU::L0 + 16;
    else if (Reg >= PS2VPU::I0 && Reg <= PS2VPU::I7)
      regIdx = Reg - PS2VPU::I0 + 24;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = IntPairRegs[regIdx / 2];
    Op.Reg.Kind = rk_IntPairReg;
    return true;
  }

  static bool MorphToDoubleReg(PS2VPUOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_FloatReg);
    unsigned regIdx = Reg - PS2VPU::F0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = DoubleRegs[regIdx / 2];
    Op.Reg.Kind = rk_DoubleReg;
    return true;
  }*/

  /*static bool MorphToQuadReg(PS2VPUOperand &Op) {
    unsigned Reg = Op.getReg();
    unsigned regIdx = 0;
    switch (Op.Reg.Kind) {
    default:
      llvm_unreachable("Unexpected register kind!");
    case rk_FloatReg:
      regIdx = Reg - PS2VPU::F0;
      if (regIdx % 4 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 4];
      break;
    case rk_DoubleReg:
      regIdx = Reg - PS2VPU::D0;
      if (regIdx % 2 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 2];
      break;
    }
    Op.Reg.RegNum = Reg;
    Op.Reg.Kind = rk_QuadReg;
    return true;
  }

  static bool MorphToCoprocPairReg(PS2VPUOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_CoprocReg);
    unsigned regIdx = 32;
    if (Reg >= PS2VPU::C0 && Reg <= PS2VPU::C31)
      regIdx = Reg - PS2VPU::C0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = CoprocPairRegs[regIdx / 2];
    Op.Reg.Kind = rk_CoprocPairReg;
    return true;
  }*/

  static std::unique_ptr<PS2VPUOperand>
  MorphToMEMrr(unsigned Base, std::unique_ptr<PS2VPUOperand> Op) {
    unsigned offsetReg = Op->getReg();
    Op->Kind = k_MemoryReg;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = offsetReg;
    Op->Mem.Off = nullptr;
    return Op;
  }

  static std::unique_ptr<PS2VPUOperand> CreateMEMr(unsigned Base, SMLoc S,
                                                  SMLoc E) {
    auto Op = std::make_unique<PS2VPUOperand>(k_MemoryReg);
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = PS2VPU::VI0; // always 0
    Op->Mem.Off = nullptr;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<PS2VPUOperand>
  MorphToMEMri(unsigned Base, std::unique_ptr<PS2VPUOperand> Op) {
    const MCExpr *Imm = Op->getImm();
    Op->Kind = k_MemoryImm;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = 0;
    Op->Mem.Off = Imm;
    return Op;
  }
};

} // end anonymous namespace

//bool PS2VPUAsmParser::expandSET(MCInst &Inst, SMLoc IDLoc,
//                               SmallVectorImpl<MCInst> &Instructions) {
//  MCOperand MCRegOp = Inst.getOperand(0);
//  MCOperand MCValOp = Inst.getOperand(1);
//  assert(MCRegOp.isReg());
//  assert(MCValOp.isImm() || MCValOp.isExpr());
//
//  // the imm operand can be either an expression or an immediate.
//  bool IsImm = Inst.getOperand(1).isImm();
//  int64_t RawImmValue = IsImm ? MCValOp.getImm() : 0;
//
//  // Allow either a signed or unsigned 32-bit immediate.
//  if (RawImmValue < -2147483648LL || RawImmValue > 4294967295LL) {
//    return Error(IDLoc,
//                 "set: argument must be between -2147483648 and 4294967295");
//  }
//
//  // If the value was expressed as a large unsigned number, that's ok.
//  // We want to see if it "looks like" a small signed number.
//  int32_t ImmValue = RawImmValue;
//  // For 'set' you can't use 'or' with a negative operand on V9 because
//  // that would splat the sign bit across the upper half of the destination
//  // register, whereas 'set' is defined to zero the high 32 bits.
//  bool IsEffectivelyImm13 =
//      IsImm && (-4096 <= ImmValue && ImmValue < 4096);
//  const MCExpr *ValExpr;
//  if (IsImm)
//    ValExpr = MCConstantExpr::create(ImmValue, getContext());
//  else
//    ValExpr = MCValOp.getExpr();
//
//  MCOperand PrevReg = MCOperand::createReg(PS2VPU::VI0);
//
//  // If not just a signed imm13 value, then either we use a 'sethi' with a
//  // following 'or', or a 'sethi' by itself if there are no more 1 bits.
//  // In either case, start with the 'sethi'.
//  if (!IsEffectivelyImm13) {
//    MCInst TmpInst;
//    const MCExpr *Expr = adjustPICRelocation(PS2VPUMCExpr::VK_PS2VPU_HI, ValExpr);
//    TmpInst.setLoc(IDLoc);
//    TmpInst.setOpcode(PS2VPUNS::SETHIi);
//    TmpInst.addOperand(MCRegOp);
//    TmpInst.addOperand(MCOperand::createExpr(Expr));
//    Instructions.push_back(TmpInst);
//    PrevReg = MCRegOp;
//  }
//
//  // The low bits require touching in 3 cases:
//  // * A non-immediate value will always require both instructions.
//  // * An effectively imm13 value needs only an 'or' instruction.
//  // * Otherwise, an immediate that is not effectively imm13 requires the
//  //   'or' only if bits remain after clearing the 22 bits that 'sethi' set.
//  // If the low bits are known zeros, there's nothing to do.
//  // In the second case, and only in that case, must we NOT clear
//  // bits of the immediate value via the %lo() assembler function.
//  // Note also, the 'or' instruction doesn't mind a large value in the case
//  // where the operand to 'set' was 0xFFFFFzzz - it does exactly what you mean.
//  if (!IsImm || IsEffectivelyImm13 || (ImmValue & 0x3ff)) {
//    MCInst TmpInst;
//    const MCExpr *Expr;
//    if (IsEffectivelyImm13)
//      Expr = ValExpr;
//    else
//      Expr = adjustPICRelocation(PS2VPUMCExpr::VK_PS2VPU_LO, ValExpr);
//    TmpInst.setLoc(IDLoc);
//    TmpInst.setOpcode(SP::ORri);
//    TmpInst.addOperand(MCRegOp);
//    TmpInst.addOperand(PrevReg);
//    TmpInst.addOperand(MCOperand::createExpr(Expr));
//    Instructions.push_back(TmpInst);
//  }
//  return false;
//}

bool PS2VPUAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                             OperandVector &Operands,
                                             MCStreamer &Out,
                                             uint64_t &ErrorInfo,
                                             bool MatchingInlineAsm) {
  MCInst Inst;
  SmallVector<MCInst, 8> Instructions;
  unsigned MatchResult =
      MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm);
  switch (MatchResult) {
  case Match_Success: {
    switch (Inst.getOpcode()) {
    default:
      Inst.setLoc(IDLoc);
      Instructions.push_back(Inst);
      break;
    /*case SP::SET:
      if (expandSET(Inst, IDLoc, Instructions))
        return true;
      break;*/
    }

    for (const MCInst &I : Instructions) {
      Out.emitInstruction(I, getSTI());
    }
    return false;
  }

  case Match_MissingFeature:
    return Error(IDLoc,
                 "instruction requires a CPU feature not currently enabled");

  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((PS2VPUOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction mnemonic");
  }
  llvm_unreachable("Implement any new match types added!");
}

bool PS2VPUAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                   SMLoc &EndLoc) {
  if (tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success)
    return Error(StartLoc, "invalid register name");
  return false;
}

OperandMatchResultTy PS2VPUAsmParser::tryParseRegister(unsigned &RegNo,
                                                      SMLoc &StartLoc,
                                                      SMLoc &EndLoc) {
  const AsmToken &Tok = Parser.getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  if (getLexer().getKind() != AsmToken::Percent)
    return MatchOperand_NoMatch;
  Parser.Lex();
  unsigned regKind = PS2VPUOperand::rk_None;
  if (matchRegisterName(Tok, RegNo, regKind)) {
    Parser.Lex();
    return MatchOperand_Success;
  }

  getLexer().UnLex(Tok);
  return MatchOperand_NoMatch;
}

//static void applyMnemonicAliases(StringRef &Mnemonic,
//                                 const FeatureBitset &Features,
//                                 unsigned VariantID);

bool PS2VPUAsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                      StringRef Name, SMLoc NameLoc,
                                      OperandVector &Operands) {

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(PS2VPUOperand::CreateToken(Name, NameLoc));

  // apply mnemonic aliases, if any, so that we can parse operands correctly.
  /*applyMnemonicAliases(Name, getAvailableFeatures(), 0);*/

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (getLexer().is(AsmToken::Comma)) {
      if (parseBranchModifiers(Operands) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
    if (parseOperand(Operands, Name) != MatchOperand_Success) {
      SMLoc Loc = getLexer().getLoc();
      return Error(Loc, "unexpected token");
    }

    while (getLexer().is(AsmToken::Comma) || getLexer().is(AsmToken::Plus)) {
      if (getLexer().is(AsmToken::Plus)) {
        // Plus tokens are significant in software_traps (p83, PS2VPUv8.pdf). We
        // must capture them.
        Operands.push_back(
            PS2VPUOperand::CreateToken("+", Parser.getTok().getLoc()));
      }
      Parser.Lex(); // Eat the comma or plus.
      // Parse and remember the operand.
      if (parseOperand(Operands, Name) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
  }
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    return Error(Loc, "unexpected token");
  }
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool PS2VPUAsmParser::ParseDirective(AsmToken DirectiveID) {
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".register") {
    // For now, ignore .register directive.
    Parser.eatToEndOfStatement();
    return false;
  }
  if (IDVal == ".proc") {
    // For compatibility, ignore this directive.
    // (It's supposed to be an "optimization" in the Sun assembler)
    Parser.eatToEndOfStatement();
    return false;
  }

  // Let the MC layer to handle other directives.
  return true;
}

OperandMatchResultTy PS2VPUAsmParser::parseMEMOperand(OperandVector &Operands) {
  SMLoc S, E;

  std::unique_ptr<PS2VPUOperand> LHS;
  if (parsePS2VPUAsmOperand(LHS) != MatchOperand_Success)
    return MatchOperand_NoMatch;

  // Single immediate operand
  if (LHS->isImm()) {
    Operands.push_back(PS2VPUOperand::MorphToMEMri(PS2VPU::VI0, std::move(LHS)));
    return MatchOperand_Success;
  }

  if (!LHS->isIntReg()) {
    Error(LHS->getStartLoc(), "invalid register kind for this operand");
    return MatchOperand_ParseFail;
  }

  AsmToken Tok = getLexer().getTok();
  // The plus token may be followed by a register or an immediate value, the
  // minus one is always interpreted as sign for the immediate value
  if (Tok.is(AsmToken::Plus) || Tok.is(AsmToken::Minus)) {
    (void)Parser.parseOptionalToken(AsmToken::Plus);

    std::unique_ptr<PS2VPUOperand> RHS;
    if (parsePS2VPUAsmOperand(RHS) != MatchOperand_Success)
      return MatchOperand_NoMatch;

    if (RHS->isReg() && !RHS->isIntReg()) {
      Error(RHS->getStartLoc(), "invalid register kind for this operand");
      return MatchOperand_ParseFail;
    }

    Operands.push_back(
        RHS->isImm()
            ? PS2VPUOperand::MorphToMEMri(LHS->getReg(), std::move(RHS))
            : PS2VPUOperand::MorphToMEMrr(LHS->getReg(), std::move(RHS)));

    return MatchOperand_Success;
  }

  Operands.push_back(PS2VPUOperand::CreateMEMr(LHS->getReg(), S, E));
  return MatchOperand_Success;
}

template <unsigned N>
OperandMatchResultTy PS2VPUAsmParser::parseShiftAmtImm(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  // This is a register, not an immediate
  if (getLexer().getKind() == AsmToken::Percent)
    return MatchOperand_NoMatch;

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return MatchOperand_ParseFail;

  const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr);
  if (!CE) {
    Error(S, "constant expression expected");
    return MatchOperand_ParseFail;
  }

  if (!isUInt<N>(CE->getValue())) {
    Error(S, "immediate shift value out of range");
    return MatchOperand_ParseFail;
  }

  Operands.push_back(PS2VPUOperand::CreateImm(Expr, S, E));
  return MatchOperand_Success;
}

template <PS2VPUAsmParser::TailRelocKind Kind>
OperandMatchResultTy
PS2VPUAsmParser::parseTailRelocSym(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  auto MatchesKind = [](PS2VPUMCExpr::VariantKind VK) -> bool {
    switch (Kind) {
    case TailRelocKind::Load_GOT:
      // Non-TLS relocations on ld (or ldx).
      // ld [%rr + %rr], %rr, %rel(sym)
      return VK == PS2VPUMCExpr::VK_PS2VPU_GOTDATA_OP;
    case TailRelocKind::Add_TLS:
      // TLS relocations on add.
      // add %rr, %rr, %rr, %rel(sym)
      switch (VK) {
      case PS2VPUMCExpr::VK_PS2VPU_TLS_GD_ADD:
      case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_ADD:
      case PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_ADD:
      case PS2VPUMCExpr::VK_PS2VPU_TLS_LDO_ADD:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Load_TLS:
      // TLS relocations on ld (or ldx).
      // ld[x] %addr, %rr, %rel(sym)
      switch (VK) {
      case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LD:
      case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LDX:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Call_TLS:
      // TLS relocations on call.
      // call sym, %rel(sym)
      switch (VK) {
      case PS2VPUMCExpr::VK_PS2VPU_TLS_GD_CALL:
      case PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_CALL:
        return true;
      default:
        return false;
      }
    }
    llvm_unreachable("Unhandled PS2VPUAsmParser::TailRelocKind enum");
  };

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  const AsmToken Tok = Parser.getTok();
  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }

  StringRef Name = getParser().getTok().getIdentifier();
  PS2VPUMCExpr::VariantKind VK = PS2VPUMCExpr::parseVariantKind(Name);
  if (VK == PS2VPUMCExpr::VK_PS2VPU_None) {
    Error(getLoc(), "invalid operand modifier");
    return MatchOperand_ParseFail;
  }

  if (!MatchesKind(VK)) {
    // Did not match the specified set of relocation types, put '%' back.
    getLexer().UnLex(Tok);
    return MatchOperand_NoMatch;
  }

  Parser.Lex(); // Eat the identifier.
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '('
  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *Val = adjustPICRelocation(VK, SubExpr);
  Operands.push_back(PS2VPUOperand::CreateImm(Val, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy PS2VPUAsmParser::parseMembarTag(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  const MCExpr *EVal;
  int64_t ImmVal = 0;

  std::unique_ptr<PS2VPUOperand> Mask;
  if (parsePS2VPUAsmOperand(Mask) == MatchOperand_Success) {
    if (!Mask->isImm() || !Mask->getImm()->evaluateAsAbsolute(ImmVal) ||
        ImmVal < 0 || ImmVal > 127) {
      Error(S, "invalid membar mask number");
      return MatchOperand_ParseFail;
    }
  }

  while (getLexer().getKind() == AsmToken::Hash) {
    SMLoc TagStart = getLexer().getLoc();
    Parser.Lex(); // Eat the '#'.
    unsigned MaskVal = StringSwitch<unsigned>(Parser.getTok().getString())
                           .Case("LoadLoad", 0x1)
                           .Case("StoreLoad", 0x2)
                           .Case("LoadStore", 0x4)
                           .Case("StoreStore", 0x8)
                           .Case("Lookaside", 0x10)
                           .Case("MemIssue", 0x20)
                           .Case("Sync", 0x40)
                           .Default(0);

    Parser.Lex(); // Eat the identifier token.

    if (!MaskVal) {
      Error(TagStart, "unknown membar tag");
      return MatchOperand_ParseFail;
    }

    ImmVal |= MaskVal;

    if (getLexer().getKind() == AsmToken::Pipe)
      Parser.Lex(); // Eat the '|'.
  }

  EVal = MCConstantExpr::create(ImmVal, getContext());
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  Operands.push_back(PS2VPUOperand::CreateImm(EVal, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy PS2VPUAsmParser::parseVectorField(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  const MCExpr *EVal;
  int64_t ImmVal = 0;

  SMLoc TagStart = getLexer().getLoc();
  Parser.Lex(); // Eat the '.'.
  if (getLexer().getKind() != AsmToken::Dot) {
    Error(TagStart, "unknown vector field");
    return MatchOperand_ParseFail;
  }

  llvm::StringRef s = Parser.getTok().getString();
  if (s.size() > 4) {
    Error(TagStart, "unknown vector field");
    return MatchOperand_ParseFail;
  }
  for (size_t i = 0; i < s.size(); ++i) {
    char c = s[i];
    int v = 0;
    switch (c) {
    case 'x':
      v = 0;
      break;
    case 'y':
      v = 1;
      break;
    case 'z':
      v = 2;
      break;
    case 'w':
      v = 3;
      break;
    default:
      Error(TagStart, "unknown vector field");
      return MatchOperand_ParseFail;
    }
    ImmVal |= ((v << 1) | 1) << i * 3;
  }

  Parser.Lex(); // Eat the identifier token.

  EVal = MCConstantExpr::create(ImmVal, getContext());
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  Operands.push_back(PS2VPUOperand::CreateImm(EVal, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy PS2VPUAsmParser::parseCallTarget(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Integer:
  case AsmToken::Identifier:
  case AsmToken::Dot:
    break;
  }

  const MCExpr *DestValue;
  if (getParser().parseExpression(DestValue))
    return MatchOperand_NoMatch;

  bool IsPic = getContext().getObjectFileInfo()->isPositionIndependent();
  PS2VPUMCExpr::VariantKind Kind =
      IsPic ? PS2VPUMCExpr::VK_PS2VPU_WPLT30 : PS2VPUMCExpr::VK_PS2VPU_WDISP30;

  const MCExpr *DestExpr = PS2VPUMCExpr::create(Kind, DestValue, getContext());
  Operands.push_back(PS2VPUOperand::CreateImm(DestExpr, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy PS2VPUAsmParser::parseOperand(OperandVector &Operands,
                                                  StringRef Mnemonic) {

  OperandMatchResultTy ResTy =
      OperandMatchResultTy::MatchOperand_NoMatch; // MatchOperandParserImpl(Operands,
                                                  // Mnemonic);

  // If there wasn't a custom match, try the generic matcher below. Otherwise,
  // there was a match, but an error occurred, in which case, just return that
  // the operand parsing failed.
  if (ResTy == MatchOperand_Success || ResTy == MatchOperand_ParseFail)
    return ResTy;

  if (getLexer().is(AsmToken::LBrac)) {
    // Memory operand
    Operands.push_back(
        PS2VPUOperand::CreateToken("[", Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the [

    if (Mnemonic == "cas" || Mnemonic == "casx" || Mnemonic == "casa") {
      SMLoc S = Parser.getTok().getLoc();
      if (getLexer().getKind() != AsmToken::Percent)
        return MatchOperand_NoMatch;
      Parser.Lex(); // eat %

      unsigned RegNo, RegKind;
      if (!matchRegisterName(Parser.getTok(), RegNo, RegKind))
        return MatchOperand_NoMatch;

      Parser.Lex(); // Eat the identifier token.
      SMLoc E =
          SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Operands.push_back(PS2VPUOperand::CreateReg(RegNo, RegKind, S, E));
      ResTy = MatchOperand_Success;
    } else {
      ResTy = parseMEMOperand(Operands);
    }

    if (ResTy != MatchOperand_Success)
      return ResTy;

    if (!getLexer().is(AsmToken::RBrac))
      return MatchOperand_ParseFail;

    Operands.push_back(
        PS2VPUOperand::CreateToken("]", Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the ]

    // Parse an optional address-space identifier after the address.
    if (getLexer().is(AsmToken::Integer)) {
      std::unique_ptr<PS2VPUOperand> Op;
      ResTy = parsePS2VPUAsmOperand(Op, false);
      if (ResTy != MatchOperand_Success || !Op)
        return MatchOperand_ParseFail;
      Operands.push_back(std::move(Op));
    }
    return MatchOperand_Success;
  }

  std::unique_ptr<PS2VPUOperand> Op;

  ResTy = parsePS2VPUAsmOperand(Op, (Mnemonic == "call"));
  if (ResTy != MatchOperand_Success || !Op)
    return MatchOperand_ParseFail;

  // Push the parsed operand into the list of operands
  Operands.push_back(std::move(Op));

  return MatchOperand_Success;
}

OperandMatchResultTy
PS2VPUAsmParser::parsePS2VPUAsmOperand(std::unique_ptr<PS2VPUOperand> &Op,
                                     bool isCall) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  const MCExpr *EVal;

  Op = nullptr;
  switch (getLexer().getKind()) {
  default:
    break;

  case AsmToken::Percent:
    Parser.Lex(); // Eat the '%'.
    unsigned RegNo;
    unsigned RegKind;
    if (matchRegisterName(Parser.getTok(), RegNo, RegKind)) {
      StringRef name = Parser.getTok().getString();
      Parser.Lex(); // Eat the identifier token.
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      switch (RegNo) {
      default:
        Op = PS2VPUOperand::CreateReg(RegNo, RegKind, S, E);
        break;
      /*case PS2VPU::PSR:
        Op = PS2VPUOperand::CreateToken("%psr", S);
        break;
      case PS2VPU::FSR:
        Op = PS2VPUOperand::CreateToken("%fsr", S);
        break;
      case PS2VPU::FQ:
        Op = PS2VPUOperand::CreateToken("%fq", S);
        break;
      case PS2VPU::CPSR:
        Op = PS2VPUOperand::CreateToken("%csr", S);
        break;
      case PS2VPU::CPQ:
        Op = PS2VPUOperand::CreateToken("%cq", S);
        break;
      case PS2VPU::WIM:
        Op = PS2VPUOperand::CreateToken("%wim", S);
        break;
      case PS2VPU::TBR:
        Op = PS2VPUOperand::CreateToken("%tbr", S);
        break;
      case PS2VPU::PC:
        Op = PS2VPUOperand::CreateToken("%pc", S);
        break;
      case PS2VPU::ICC:
        if (name == "xcc")
          Op = PS2VPUOperand::CreateToken("%xcc", S);
        else
          Op = PS2VPUOperand::CreateToken("%icc", S);
        break;*/
      }
      break;
    }
    if (matchPS2VPUAsmModifiers(EVal, E)) {
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Op = PS2VPUOperand::CreateImm(EVal, S, E);
    }
    break;

  case AsmToken::Plus:
  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::LParen:
  case AsmToken::Dot:
  case AsmToken::Identifier:
    if (getParser().parseExpression(EVal, E))
      break;

    int64_t Res;
    if (!EVal->evaluateAsAbsolute(Res)) {
      PS2VPUMCExpr::VariantKind Kind = PS2VPUMCExpr::VK_PS2VPU_13;

      if (getContext().getObjectFileInfo()->isPositionIndependent()) {
        if (isCall)
          Kind = PS2VPUMCExpr::VK_PS2VPU_WPLT30;
        else
          Kind = PS2VPUMCExpr::VK_PS2VPU_GOT13;
      }
      EVal = PS2VPUMCExpr::create(Kind, EVal, getContext());
    }
    Op = PS2VPUOperand::CreateImm(EVal, S, E);
    break;
  }
  return (Op) ? MatchOperand_Success : MatchOperand_ParseFail;
}

OperandMatchResultTy
PS2VPUAsmParser::parseBranchModifiers(OperandVector &Operands) {
  // parse (,a|,pn|,pt)+

  while (getLexer().is(AsmToken::Comma)) {
    Parser.Lex(); // Eat the comma

    if (!getLexer().is(AsmToken::Identifier))
      return MatchOperand_ParseFail;
    StringRef modName = Parser.getTok().getString();
    if (modName == "a" || modName == "pn" || modName == "pt") {
      Operands.push_back(
          PS2VPUOperand::CreateToken(modName, Parser.getTok().getLoc()));
      Parser.Lex(); // eat the identifier.
    }
  }
  return MatchOperand_Success;
}

bool PS2VPUAsmParser::matchRegisterName(const AsmToken &Tok, unsigned &RegNo,
                                       unsigned &RegKind) {
  int64_t intVal = 0;
  RegNo = 0;
  RegKind = PS2VPUOperand::rk_None;
  if (Tok.is(AsmToken::Identifier)) {
    StringRef name = Tok.getString();

    // %fp
    //if (name.equals("fp")) {
    //  RegNo = PS2VPU::I6;
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}
    //// %sp
    //if (name.equals("sp")) {
    //  RegNo = PS2VPU::O6;
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}

    //if (name.equals("y")) {
    //  RegNo = PS2VPU::Y;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.substr(0, 3).equals_insensitive("asr") &&
    //    !name.substr(3).getAsInteger(10, intVal) && intVal > 0 && intVal < 32) {
    //  RegNo = ASRRegs[intVal];
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //// %fprs is an alias of %asr6.
    //if (name.equals("fprs")) {
    //  RegNo = ASRRegs[6];
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("icc")) {
    //  RegNo = PS2VPU::ICC;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("psr")) {
    //  RegNo = PS2VPU::PSR;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("fsr")) {
    //  RegNo = PS2VPU::FSR;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("fq")) {
    //  RegNo = PS2VPU::FQ;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("csr")) {
    //  RegNo = PS2VPU::CPSR;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("cq")) {
    //  RegNo = PS2VPU::CPQ;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("wim")) {
    //  RegNo = PS2VPU::WIM;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("tbr")) {
    //  RegNo = PS2VPU::TBR;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //if (name.equals("xcc")) {
    //  // FIXME:: check 64bit.
    //  RegNo = PS2VPU::ICC;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    //// %fcc0 - %fcc3
    //if (name.substr(0, 3).equals_insensitive("fcc") &&
    //    !name.substr(3).getAsInteger(10, intVal) && intVal < 4) {
    //  // FIXME: check 64bit and  handle %fcc1 - %fcc3
    //  RegNo = PS2VPU::FCC0 + intVal;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}

    if (name.substr(0, 1).equals_insensitive("VI") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 16) {
      RegNo = IntRegs[intVal];
      RegKind = PS2VPUOperand::rk_IntReg;
      return true;
    }

    // %g0 - %g7
    //if (name.substr(0, 1).equals_insensitive("g") &&
    //    !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
    //  RegNo = IntRegs[intVal];
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}
    //// %o0 - %o7
    //if (name.substr(0, 1).equals_insensitive("o") &&
    //    !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
    //  RegNo = IntRegs[8 + intVal];
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}
    //if (name.substr(0, 1).equals_insensitive("l") &&
    //    !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
    //  RegNo = IntRegs[16 + intVal];
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}
    //if (name.substr(0, 1).equals_insensitive("i") &&
    //    !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
    //  RegNo = IntRegs[24 + intVal];
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}
    //// %f0 - %f31
    //if (name.substr(0, 1).equals_insensitive("f") &&
    //    !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 32) {
    //  RegNo = FloatRegs[intVal];
    //  RegKind = PS2VPUOperand::rk_FloatReg;
    //  return true;
    //}
    //// %f32 - %f62
    //if (name.substr(0, 1).equals_insensitive("f") &&
    //    !name.substr(1, 2).getAsInteger(10, intVal) && intVal >= 32 &&
    //    intVal <= 62 && (intVal % 2 == 0)) {
    //  // FIXME: Check V9
    //  RegNo = DoubleRegs[intVal / 2];
    //  RegKind = PS2VPUOperand::rk_DoubleReg;
    //  return true;
    //}

    //// %r0 - %r31
    //if (name.substr(0, 1).equals_insensitive("r") &&
    //    !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 31) {
    //  RegNo = IntRegs[intVal];
    //  RegKind = PS2VPUOperand::rk_IntReg;
    //  return true;
    //}

    //// %c0 - %c31
    //if (name.substr(0, 1).equals_insensitive("c") &&
    //    !name.substr(1).getAsInteger(10, intVal) && intVal < 32) {
    //  RegNo = CoprocRegs[intVal];
    //  RegKind = PS2VPUOperand::rk_CoprocReg;
    //  return true;
    //}

    //if (name.equals("tpc")) {
    //  RegNo = PS2VPU::TPC;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tnpc")) {
    //  RegNo = PS2VPU::TNPC;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tstate")) {
    //  RegNo = PS2VPU::TSTATE;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tt")) {
    //  RegNo = PS2VPU::TT;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tick")) {
    //  RegNo = PS2VPU::TICK;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tba")) {
    //  RegNo = PS2VPU::TBA;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("pstate")) {
    //  RegNo = PS2VPU::PSTATE;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("tl")) {
    //  RegNo = PS2VPU::TL;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("pil")) {
    //  RegNo = PS2VPU::PIL;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("cwp")) {
    //  RegNo = PS2VPU::CWP;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("cansave")) {
    //  RegNo = PS2VPU::CANSAVE;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("canrestore")) {
    //  RegNo = PS2VPU::CANRESTORE;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("cleanwin")) {
    //  RegNo = PS2VPU::CLEANWIN;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("otherwin")) {
    //  RegNo = PS2VPU::OTHERWIN;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("wstate")) {
    //  RegNo = PS2VPU::WSTATE;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
    //if (name.equals("pc")) {
    //  RegNo = PS2VPU::PC;
    //  RegKind = PS2VPUOperand::rk_Special;
    //  return true;
    //}
  }
  return false;
}

// Determine if an expression contains a reference to the symbol
// "_GLOBAL_OFFSET_TABLE_".
static bool hasGOTReference(const MCExpr *Expr) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    if (const PS2VPUMCExpr *SE = dyn_cast<PS2VPUMCExpr>(Expr))
      return hasGOTReference(SE->getSubExpr());
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    return hasGOTReference(BE->getLHS()) || hasGOTReference(BE->getRHS());
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    return (SymRef.getSymbol().getName() == "_GLOBAL_OFFSET_TABLE_");
  }

  case MCExpr::Unary:
    return hasGOTReference(cast<MCUnaryExpr>(Expr)->getSubExpr());
  }
  return false;
}

const PS2VPUMCExpr *
PS2VPUAsmParser::adjustPICRelocation(PS2VPUMCExpr::VariantKind VK,
                                    const MCExpr *subExpr) {
  // When in PIC mode, "%lo(...)" and "%hi(...)" behave differently.
  // If the expression refers contains _GLOBAL_OFFSET_TABLE, it is
  // actually a %pc10 or %pc22 relocation. Otherwise, they are interpreted
  // as %got10 or %got22 relocation.

  if (getContext().getObjectFileInfo()->isPositionIndependent()) {
    switch (VK) {
    default:
      break;
    case PS2VPUMCExpr::VK_PS2VPU_LO:
      VK = (hasGOTReference(subExpr) ? PS2VPUMCExpr::VK_PS2VPU_PC10
                                     : PS2VPUMCExpr::VK_PS2VPU_GOT10);
      break;
    case PS2VPUMCExpr::VK_PS2VPU_HI:
      VK = (hasGOTReference(subExpr) ? PS2VPUMCExpr::VK_PS2VPU_PC22
                                     : PS2VPUMCExpr::VK_PS2VPU_GOT22);
      break;
    }
  }

  return PS2VPUMCExpr::create(VK, subExpr, getContext());
}

bool PS2VPUAsmParser::matchPS2VPUAsmModifiers(const MCExpr *&EVal,
                                            SMLoc &EndLoc) {
  AsmToken Tok = Parser.getTok();
  if (!Tok.is(AsmToken::Identifier))
    return false;

  StringRef name = Tok.getString();

  PS2VPUMCExpr::VariantKind VK = PS2VPUMCExpr::parseVariantKind(name);
  switch (VK) {
  case PS2VPUMCExpr::VK_PS2VPU_None:
    Error(getLoc(), "invalid operand modifier");
    return false;

  case PS2VPUMCExpr::VK_PS2VPU_GOTDATA_OP:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_GD_ADD:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_GD_CALL:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_ADD:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LD:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_IE_LDX:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_ADD:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_LDM_CALL:
  case PS2VPUMCExpr::VK_PS2VPU_TLS_LDO_ADD:
    // These are special-cased at tablegen level.
    return false;

  default:
    break;
  }

  Parser.Lex(); // Eat the identifier.
  if (Parser.getTok().getKind() != AsmToken::LParen)
    return false;

  Parser.Lex(); // Eat the LParen token.
  const MCExpr *subExpr;
  if (Parser.parseParenExpression(subExpr, EndLoc))
    return false;

  EVal = adjustPICRelocation(VK, subExpr);
  return true;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePS2VPUAsmParser() {
  RegisterMCAsmParser<PS2VPUAsmParser> A(getThePS2VPUTarget());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "PS2VPUGenAsmMatcher.inc"

unsigned PS2VPUAsmParser::validateTargetOperandClass(MCParsedAsmOperand &GOp,
                                                    unsigned Kind) {
  PS2VPUOperand &Op = (PS2VPUOperand &)GOp;
  if (Op.isFloatOrDoubleReg()) {
    switch (Kind) {
    default:
      break;
    /*case MCK_DFPRegs:
      if (!Op.isFloatReg() || PS2VPUOperand::MorphToDoubleReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    case MCK_QFPRegs:
      if (PS2VPUOperand::MorphToQuadReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;*/
    }
  }
  /*if (Op.isIntReg() && Kind == MCK_IntPair) {
    if (PS2VPUOperand::MorphToIntPairReg(Op))
      return MCTargetAsmParser::Match_Success;
  }
  if (Op.isCoprocReg() && Kind == MCK_CoprocPair) {
    if (PS2VPUOperand::MorphToCoprocPairReg(Op))
      return MCTargetAsmParser::Match_Success;
  }*/
  return Match_InvalidOperand;
}
