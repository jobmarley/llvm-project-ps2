//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "ps2vpuInstrFormats.td"

//===----------------------------------------------------------------------===//
// Feature predicates.
//===----------------------------------------------------------------------===//

// True when generating 32-bit code.
//def Is32Bit : Predicate<"!Subtarget->is64Bit()">;
//
//// True when generating 64-bit code. This also implies HasV9.
//def Is64Bit : Predicate<"Subtarget->is64Bit()">;
//
//def UseSoftMulDiv : Predicate<"Subtarget->useSoftMulDiv()">,
//              AssemblerPredicate<(all_of FeatureSoftMulDiv)>;
//
//// HasV9 - This predicate is true when the target processor supports V9
//// instructions.  Note that the machine may be running in 32-bit mode.
//def HasV9   : Predicate<"Subtarget->isV9()">,
//              AssemblerPredicate<(all_of FeatureV9)>;
//
//// HasNoV9 - This predicate is true when the target doesn't have V9
//// instructions.  Use of this is just a hack for the isel not having proper
//// costs for V8 instructions that are more expensive than their V9 ones.
//def HasNoV9 : Predicate<"!Subtarget->isV9()">;
//
//// HasVIS - This is true when the target processor has VIS extensions.
//def HasVIS : Predicate<"Subtarget->isVIS()">,
//             AssemblerPredicate<(all_of FeatureVIS)>;
//def HasVIS2 : Predicate<"Subtarget->isVIS2()">,
//             AssemblerPredicate<(all_of FeatureVIS2)>;
//def HasVIS3 : Predicate<"Subtarget->isVIS3()">,
//             AssemblerPredicate<(all_of FeatureVIS3)>;
//
//// HasHardQuad - This is true when the target processor supports quad floating
//// point instructions.
//def HasHardQuad : Predicate<"Subtarget->hasHardQuad()">;
//
//// HasLeonCASA - This is true when the target processor supports the CASA
//// instruction
//def HasLeonCASA : Predicate<"Subtarget->hasLeonCasa()">;
//
//// HasPWRPSR - This is true when the target processor supports partial
//// writes to the PSR register that only affects the ET field.
//def HasPWRPSR : Predicate<"Subtarget->hasPWRPSR()">,
//                AssemblerPredicate<(all_of FeaturePWRPSR)>;
//
//// HasUMAC_SMAC - This is true when the target processor supports the
//// UMAC and SMAC instructions
//def HasUMAC_SMAC : Predicate<"Subtarget->hasUmacSmac()">;
//
//def HasNoFdivSqrtFix : Predicate<"!Subtarget->fixAllFDIVSQRT()">;
//def HasFMULS : Predicate<"!Subtarget->hasNoFMULS()">;
//def HasFSMULD : Predicate<"!Subtarget->hasNoFSMULD()">;
//
//// UseDeprecatedInsts - This predicate is true when the target processor is a
//// V8, or when it is V9 but the V8 deprecated instructions are efficient enough
//// to use when appropriate.  In either of these cases, the instruction selector
//// will pick deprecated instructions.
//def UseDeprecatedInsts : Predicate<"Subtarget->useDeprecatedV8Instructions()">;

//===----------------------------------------------------------------------===//
// Instruction Pattern Stuff
//===----------------------------------------------------------------------===//

def simm5  : PatLeaf<(imm), [{ return isInt<5>(N->getSExtValue()); }]>;

def simm11  : PatLeaf<(imm), [{ return isInt<11>(N->getSExtValue()); }]>;

def simm13  : PatLeaf<(imm), [{ return isInt<13>(N->getSExtValue()); }]>;

// negative immediate 16
def nimm16  : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()) && N->getSExtValue() < 0; }]>;
// positive immediate 16
def pimm16  : PatLeaf<(imm), [{ return isInt<16>(N->getSExtValue()) && N->getSExtValue() >= 0; }]>;

def IMMNEG : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((unsigned)-N->getSExtValue(), SDLoc(N),
                                   MVT::i16);
}]>;

//def VECTORFIELDADDR : PatLeaf<(imm), [{
//  return CurDAG->getTargetConstant(N->getSExtValue(), SDLoc(N),
//                                   MVT::i16);
//}]>;

def LO10 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() & 1023, SDLoc(N),
                                   MVT::i32);
}]>;

def HI22 : SDNodeXForm<imm, [{
  // Transformation function: shift the immediate value down into the low bits.
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() >> 10, SDLoc(N),
                                   MVT::i32);
}]>;

// Return the complement of a HI22 immediate value.
def HI22_not : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(~(unsigned)N->getZExtValue() >> 10, SDLoc(N),
                                   MVT::i32);
}]>;

def SETHIimm : PatLeaf<(imm), [{
  return isShiftedUInt<22, 10>(N->getZExtValue());
}], HI22>;

// The N->hasOneUse() prevents the immediate from being instantiated in both
// normal and complement form.
def SETHIimm_not : PatLeaf<(i32 imm), [{
  return N->hasOneUse() && isShiftedUInt<22, 10>(~(unsigned)N->getZExtValue());
}], HI22_not>;

// Addressing modes.
def ADDRrr : ComplexPattern<iPTR, 2, "SelectADDRrr", [], []>;
def ADDRri : ComplexPattern<iPTR, 2, "SelectADDRri", [frameindex], []>;
def ADDRrix : ComplexPattern<iPTR, 2, "SelectADDRrix", [], []>;
def ADDRriy : ComplexPattern<iPTR, 2, "SelectADDRriy", [], []>;
def ADDRriz : ComplexPattern<iPTR, 2, "SelectADDRriz", [], []>;
def ADDRriw : ComplexPattern<iPTR, 2, "SelectADDRriw", [], []>;

// Constrained operands for the shift operations.
class ShiftAmtImmAsmOperand<int Bits> : AsmOperandClass {
    let Name = "ShiftAmtImm" # Bits;
    let ParserMethod = "parseShiftAmtImm<" # Bits # ">";
}
def shift_imm5 : Operand<i32> {
  let ParserMatchClass = ShiftAmtImmAsmOperand<5>;
}
def shift_imm6 : Operand<i32> {
  let ParserMatchClass = ShiftAmtImmAsmOperand<6>;
}

// Address operands
def PS2VPUMEMrrAsmOperand : AsmOperandClass {
  let Name = "MEMrr";
  let ParserMethod = "parseMEMOperand";
}

def PS2VPUMEMriAsmOperand : AsmOperandClass {
  let Name = "MEMri";
  let ParserMethod = "parseMEMOperand";
}

def MEMrr : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops ptr_rc, ptr_rc);
  let ParserMatchClass = PS2VPUMEMrrAsmOperand;
}
def MEMri : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops ptr_rc, i32imm);
  let ParserMatchClass = PS2VPUMEMriAsmOperand;
}

// Represents a tail relocation operand for instructions such as add, ld, call.
class PS2VPUTailRelocSymAsmOperand<string Kind> : AsmOperandClass {
  let Name = "TailRelocSym" # Kind;
  let RenderMethod = "addTailRelocSymOperands";
  let PredicateMethod = "isTailRelocSym";
  let ParserMethod = "parseTailRelocSym<TailRelocKind::" # Kind # ">";
}

def TailRelocSymGOTLoad : Operand<iPTR> {
  let ParserMatchClass = PS2VPUTailRelocSymAsmOperand<"Load_GOT">;
}

def TailRelocSymTLSAdd : Operand<iPTR> {
  let ParserMatchClass = PS2VPUTailRelocSymAsmOperand<"Add_TLS">;
}

def TailRelocSymTLSLoad : Operand<iPTR> {
  let ParserMatchClass = PS2VPUTailRelocSymAsmOperand<"Load_TLS">;
}

def TailRelocSymTLSCall : Operand<iPTR> {
  let ParserMatchClass = PS2VPUTailRelocSymAsmOperand<"Call_TLS">;
}

def PS2VPUMembarTagAsmOperand : AsmOperandClass {
  let Name = "MembarTag";
  let ParserMethod = "parseMembarTag";
}

def MembarTag : Operand<i32> {
  let PrintMethod = "printMembarTag";
  let ParserMatchClass = PS2VPUMembarTagAsmOperand;
}

// Branch targets have OtherVT type.
def brtarget : Operand<OtherVT> {
  let EncoderMethod = "getBranchTargetOpValue";
}

def bprtarget : Operand<OtherVT> {
  let EncoderMethod = "getBranchPredTargetOpValue";
}

def bprtarget16 : Operand<OtherVT> {
  let EncoderMethod = "getBranchOnRegTargetOpValue";
}

def PS2VPUCallTargetAsmOperand : AsmOperandClass {
  let Name = "CallTarget";
  let ParserMethod = "parseCallTarget";
}

def calltarget : Operand<i32> {
  let EncoderMethod = "getCallTargetOpValue";
  let DecoderMethod = "DecodeCall";
  let ParserMatchClass = PS2VPUCallTargetAsmOperand;
}

def simm13Op : Operand<i32> {
  let DecoderMethod = "DecodeSIMM13";
  let EncoderMethod = "getSImm13OpValue";
}

//def PS2VPUVectorFieldAsmOperand : AsmOperandClass {
//  let Name = "VectorField";
//  let ParserMethod = "parseVectorField";
//}
//
//def VectorField : Operand<i32> {
//  let PrintMethod = "printVectorField";
//  let ParserMatchClass = PS2VPUVectorFieldAsmOperand;
//}

// Operand for printing out a condition code.
let PrintMethod = "printCCOperand" in
  def CCOp : Operand<i32>;

def SDTSPcmpicc :
SDTypeProfile<0, 2, [SDTCisInt<0>, SDTCisSameAs<0, 1>]>;
def SDTSPcmpfcc :
SDTypeProfile<0, 2, [SDTCisFP<0>, SDTCisSameAs<0, 1>]>;
def SDTSPbrcc :
SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i32>]>;
def SDTSPselectcc :
SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i32>]>;
def SDTSPFTOI :
SDTypeProfile<1, 1, [SDTCisVT<0, f32>, SDTCisFP<1>]>;
def SDTSPITOF :
SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisVT<1, f32>]>;
def SDTSPFTOX :
SDTypeProfile<1, 1, [SDTCisVT<0, f64>, SDTCisFP<1>]>;
def SDTSPXTOF :
SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisVT<1, f64>]>;

def SDTSPtlsadd :
SDTypeProfile<1, 3, [SDTCisInt<0>, SDTCisSameAs<0, 1>, SDTCisPtrTy<2>]>;
def SDTSPtlsld :
SDTypeProfile<1, 2, [SDTCisPtrTy<0>, SDTCisPtrTy<1>]>;

def SDTSPloadgdop :
SDTypeProfile<1, 2, [SDTCisPtrTy<0>, SDTCisPtrTy<1>]>;

def SPcmpicc : SDNode<"PS2VPUISD::CMPICC", SDTSPcmpicc, [SDNPOutGlue]>;
def SPcmpfcc : SDNode<"PS2VPUISD::CMPFCC", SDTSPcmpfcc, [SDNPOutGlue]>;
def SPbricc : SDNode<"PS2VPUISD::BRICC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbrxcc : SDNode<"PS2VPUISD::BRXCC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;
def SPbrfcc : SDNode<"PS2VPUISD::BRFCC", SDTSPbrcc, [SDNPHasChain, SDNPInGlue]>;

def SPhi    : SDNode<"PS2VPUISD::Hi", SDTIntUnaryOp>;
def SPlo    : SDNode<"PS2VPUISD::Lo", SDTIntUnaryOp>;

def SPftoi  : SDNode<"PS2VPUISD::FTOI", SDTSPFTOI>;
def SPitof  : SDNode<"PS2VPUISD::ITOF", SDTSPITOF>;
def SPftox  : SDNode<"PS2VPUISD::FTOX", SDTSPFTOX>;
def SPxtof  : SDNode<"PS2VPUISD::XTOF", SDTSPXTOF>;

def SPselecticc : SDNode<"PS2VPUISD::SELECT_ICC", SDTSPselectcc, [SDNPInGlue]>;
def SPselectxcc : SDNode<"PS2VPUISD::SELECT_XCC", SDTSPselectcc, [SDNPInGlue]>;
def SPselectfcc : SDNode<"PS2VPUISD::SELECT_FCC", SDTSPselectcc, [SDNPInGlue]>;

//  These are target-independent nodes, but have target-specific formats.
def SDT_SPCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                          SDTCisVT<1, i32> ]>;
def SDT_SPCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                        SDTCisVT<1, i32> ]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_SPCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_SPCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def SDT_SPCall    : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def call          : SDNode<"PS2VPUISD::CALL", SDT_SPCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

def tailcall      : SDNode<"PS2VPUISD::TAIL_CALL", SDT_SPCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

def SDT_SPRet     : SDTypeProfile<0, 1, [SDTCisVT<0, i16>]>;
def retflag       : SDNode<"PS2VPUISD::RET_FLAG", SDT_SPRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def flushw        : SDNode<"PS2VPUISD::FLUSHW", SDTNone,
                           [SDNPHasChain, SDNPSideEffect, SDNPMayStore]>;

def tlsadd        : SDNode<"PS2VPUISD::TLS_ADD", SDTSPtlsadd>;
def tlsld         : SDNode<"PS2VPUISD::TLS_LD",  SDTSPtlsld>;
def tlscall       : SDNode<"PS2VPUISD::TLS_CALL", SDT_SPCall,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                             SDNPVariadic]>;

def load_gdop : SDNode<"PS2VPUISD::LOAD_GDOP",  SDTSPloadgdop>;

def getPCX        : Operand<iPTR> {
  let PrintMethod = "printGetPCX";
}

def SDTPS2VPUbricc : SDTypeProfile<0, 4, [SDTCisInt<0>, SDTCisVT<1, i16>, SDTCisVT<2, i16>, SDTCisVT<3, OtherVT>]>;
def PS2VPUbricc : SDNode<"PS2VPUISD::BRICC", SDTPS2VPUbricc, [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

//===----------------------------------------------------------------------===//
// PS2VPU Flag Conditions
//===----------------------------------------------------------------------===//

// Note that these values must be kept in sync with the CCOp::CondCode enum
// values.
class ICC_VAL<int N> : PatLeaf<(i32 N)>;
def ICC_NE  : ICC_VAL< 9>;  // Not Equal
def ICC_E   : ICC_VAL< 1>;  // Equal
def ICC_G   : ICC_VAL<10>;  // Greater
def ICC_LE  : ICC_VAL< 2>;  // Less or Equal
def ICC_GE  : ICC_VAL<11>;  // Greater or Equal
def ICC_L   : ICC_VAL< 3>;  // Less
def ICC_GU  : ICC_VAL<12>;  // Greater Unsigned
def ICC_LEU : ICC_VAL< 4>;  // Less or Equal Unsigned
def ICC_CC  : ICC_VAL<13>;  // Carry Clear/Great or Equal Unsigned
def ICC_CS  : ICC_VAL< 5>;  // Carry Set/Less Unsigned
def ICC_POS : ICC_VAL<14>;  // Positive
def ICC_NEG : ICC_VAL< 6>;  // Negative
def ICC_VC  : ICC_VAL<15>;  // Overflow Clear
def ICC_VS  : ICC_VAL< 7>;  // Overflow Set

class FCC_VAL<int N> : PatLeaf<(i32 N)>;
def FCC_U   : FCC_VAL<23>;  // Unordered
def FCC_G   : FCC_VAL<22>;  // Greater
def FCC_UG  : FCC_VAL<21>;  // Unordered or Greater
def FCC_L   : FCC_VAL<20>;  // Less
def FCC_UL  : FCC_VAL<19>;  // Unordered or Less
def FCC_LG  : FCC_VAL<18>;  // Less or Greater
def FCC_NE  : FCC_VAL<17>;  // Not Equal
def FCC_E   : FCC_VAL<25>;  // Equal
def FCC_UE  : FCC_VAL<26>;  // Unordered or Equal
def FCC_GE  : FCC_VAL<27>;  // Greater or Equal
def FCC_UGE : FCC_VAL<28>;  // Unordered or Greater or Equal
def FCC_LE  : FCC_VAL<29>;  // Less or Equal
def FCC_ULE : FCC_VAL<30>;  // Unordered or Less or Equal
def FCC_O   : FCC_VAL<31>;  // Ordered

class CPCC_VAL<int N> : PatLeaf<(i32 N)>;
def CPCC_3   : CPCC_VAL<39>;  // 3
def CPCC_2   : CPCC_VAL<38>;  // 2
def CPCC_23  : CPCC_VAL<37>;  // 2 or 3
def CPCC_1   : CPCC_VAL<36>;  // 1
def CPCC_13  : CPCC_VAL<35>;  // 1 or 3
def CPCC_12  : CPCC_VAL<34>;  // 1 or 2
def CPCC_123 : CPCC_VAL<33>;  // 1 or 2 or 3
def CPCC_0   : CPCC_VAL<41>;  // 0
def CPCC_03  : CPCC_VAL<42>;  // 0 or 3
def CPCC_02  : CPCC_VAL<43>;  // 0 or 2
def CPCC_023 : CPCC_VAL<44>;  // 0 or 2 or 3
def CPCC_01  : CPCC_VAL<45>;  // 0 or 1
def CPCC_013 : CPCC_VAL<46>;  // 0 or 1 or 3
def CPCC_012 : CPCC_VAL<47>;  // 0 or 1 or 2


//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//

multiclass VecLoad7<string OpcStr, bits<7> OpVal, RegisterClass RC, InstrItinClass itin> {
    def x : LoadStore7<OpVal, (outs RC:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".x $dst, $addr"),
                [(set IntRegs:$dst, (load ADDRrix:$addr))],
                0b1000,
                itin>;
    def y : LoadStore7<OpVal, (outs RC:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".y $dst, $addr"),
                [(set IntRegs:$dst, (load ADDRriy:$addr))],
                0b0100,
                itin>;
    def z : LoadStore7<OpVal, (outs RC:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".z $dst, $addr"),
                [(set IntRegs:$dst, (load ADDRriz:$addr))],
                0b0010,
                itin>;
    def w : LoadStore7<OpVal, (outs RC:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".w $dst, $addr"),
                [(set IntRegs:$dst, (load ADDRriw:$addr))],
                0b0001,
                itin>;
}
multiclass VecStore7<string OpcStr, bits<7> OpVal, RegisterClass RC, InstrItinClass itin> {
    def x : LoadStore7<OpVal, (outs), (ins RC:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".x $src, $addr"),
                [(store RC:$src, ADDRrix:$addr)],
                0b1000,
                itin>;
    def y : LoadStore7<OpVal, (outs), (ins RC:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".y $src, $addr"),
                [(store RC:$src, ADDRriy:$addr)], 
                0b0100,
                itin>;
    def z : LoadStore7<OpVal, (outs), (ins RC:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".z $src, $addr"),
                [(store RC:$src, ADDRriz:$addr)],
                0b0010,
                itin>;
    def w : LoadStore7<OpVal, (outs), (ins RC:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".w $src, $addr"),
                [(store RC:$src, ADDRriw:$addr)],
                0b0001,
                itin>;
}
multiclass VecLoadFloat7<string OpcStr, bits<7> OpVal, InstrItinClass itin> {
    def x : LoadStore7<OpVal, (outs FloatXRegs:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".x $dst, $addr"),
                [(set FloatXRegs:$dst, (load ADDRrix:$addr))],
                0b1000,
                itin>;
    def y : LoadStore7<OpVal, (outs FloatYRegs:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".y $dst, $addr"),
                [(set FloatYRegs:$dst, (load ADDRriy:$addr))],
                0b0100,
                itin>;
    def z : LoadStore7<OpVal, (outs FloatZRegs:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".z $dst, $addr"),
                [(set FloatZRegs:$dst, (load ADDRriz:$addr))],
                0b0010,
                itin>;
    def w : LoadStore7<OpVal, (outs FloatWRegs:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".w $dst, $addr"),
                [(set FloatWRegs:$dst, (load ADDRriw:$addr))],
                0b0001,
                itin>;
    def v4 : LoadStore7<OpVal, (outs VFRegs:$dst), (ins (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".xyzw $dst, $addr"),
                [(set VFRegs:$dst, (load ADDRri:$addr))],
                0b1111,
                itin>;
}
multiclass VecStoreFloat7<string OpcStr, bits<7> OpVal, InstrItinClass itin> {
    def x : LoadStore7<OpVal, (outs), (ins FloatXRegs:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".x $src, $addr"),
                [(store FloatXRegs:$src, ADDRrix:$addr)],
                0b1000,
                itin>;
    def y : LoadStore7<OpVal, (outs), (ins FloatYRegs:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".y $src, $addr"),
                [(store FloatYRegs:$src, ADDRriy:$addr)], 
                0b0100,
                itin>;
    def z : LoadStore7<OpVal, (outs), (ins FloatZRegs:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".z $src, $addr"),
                [(store FloatZRegs:$src, ADDRriz:$addr)],
                0b0010,
                itin>;
    def w : LoadStore7<OpVal, (outs), (ins FloatWRegs:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".w $src, $addr"),
                [(store FloatWRegs:$src, ADDRriw:$addr)],
                0b0001,
                itin>;
    def v4 : LoadStore7<OpVal, (outs), (ins VFRegs:$src, (MEMri $is, $simm13):$addr),
                !strconcat(OpcStr, ".xyzw $src, $addr"),
                [(store VFRegs:$src, ADDRri:$addr)],
                0b1111,
                itin>;
}
let isReturn = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1, isCodeGenOnly = 1 in {
  def Jr : BranchInstr<0b0100100, (outs), (ins IntRegs:$r),
                    "JR $r", [(retflag i16:$r)], 0b0000, IIC_branch_instr>;
}


let isBranch = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1 in
{
  def Bi : BranchInstr<0b0100000, (outs), (ins brtarget:$imm11),
                      "B $imm11", [(br bb:$imm11)], 0b0000, IIC_branch_instr>;
                      
  def IBEQ : BranchInstr<0b0101000, (outs), (ins IntRegs:$it, IntRegs:$is, brtarget:$imm11),
                    "IBEQ $it, $is, $imm11", [(brcc SETEQ, IntRegs:$it, IntRegs:$is, bb:$imm11)], 0b0000, IIC_branch_instr>;

  def IBNE : BranchInstr<0b0101001, (outs), (ins IntRegs:$it, IntRegs:$is, brtarget:$imm11),
                    "IBNE $it, $is, $imm11", [(brcc SETNE, IntRegs:$it, IntRegs:$is, bb:$imm11)], 0b0000, IIC_branch_instr>;
                    
  def IBLTZ : BranchInstr<0b0101100, (outs), (ins IntRegs:$it, brtarget:$imm11),
                    "IBLTZ $it, $imm11", [(brcc SETLT, IntRegs:$it, 0, bb:$imm11)], 0b0000, IIC_branch_instr>;

  def IBLEZ : BranchInstr<0b0101110, (outs), (ins IntRegs:$it, brtarget:$imm11),
                    "IBLEZ $it, $imm11", [(brcc SETLE, IntRegs:$it, 0, bb:$imm11)], 0b0000, IIC_branch_instr>;
                    
  def IBGTZ : BranchInstr<0b0101101, (outs), (ins IntRegs:$it, brtarget:$imm11),
                    "IBGTZ $it, $imm11", [(brcc SETGT, IntRegs:$it, 0, bb:$imm11)], 0b0000, IIC_branch_instr>;

  def IBGEZ : BranchInstr<0b0101111, (outs), (ins IntRegs:$it, brtarget:$imm11),
                    "IBGEZ $it, $imm11", [(brcc SETGE, IntRegs:$it, 0, bb:$imm11)], 0b0000, IIC_branch_instr>;
}

defm ILW : VecLoad7<"ILW", 0b0000100, IntRegs, IIC_ldst_instr>;
defm ISW : VecStore7<"ISW", 0b0000101, IntRegs, IIC_ldst_instr>;

def IADDrr : Lower1InstPS2VPU<0b110000, (outs IntRegs:$id), (ins IntRegs:$is, IntRegs:$it),
                    "IADD $id, $is, $it", [(set IntRegs:$id, (add IntRegs:$is, IntRegs:$it))], 0b0000, IIC_ialu_instr>;

def IADDri : Lower5InstPS2VPU<0b110010, (outs IntRegs:$it), (ins IntRegs:$is, i16imm:$imm5),
                    "IADDI $it, $is, $imm5", [(set IntRegs:$it, (add IntRegs:$is, (i16 simm5:$imm5)))], IIC_ialu_instr>;

def IADDUri : Lower8InstPS2VPU<0b001000, (outs IntRegs:$it), (ins IntRegs:$is, i16imm:$imm15),
                    "IADDIU $it, $is, $imm15", [(set IntRegs:$it, (add IntRegs:$is, imm:$imm15))], IIC_ialu_instr>;


def ISUBrr : Lower1InstPS2VPU<0b110001, (outs IntRegs:$id), (ins IntRegs:$is, IntRegs:$it),
                    "ISUB $id, $is, $it", [(set IntRegs:$id, (sub IntRegs:$is, IntRegs:$it))], 0b0000, IIC_ialu_instr>;
                    
def ISUBUri : Lower8InstPS2VPU<0b001001, (outs IntRegs:$it), (ins IntRegs:$is, i16imm:$imm15),
                    "ISUBIU $it, $is, $imm15", [(set IntRegs:$it, (sub IntRegs:$is, imm:$imm15))], IIC_ialu_instr>;

                    
def IORrr : Lower1InstPS2VPU<0b110101, (outs IntRegs:$id), (ins IntRegs:$is, IntRegs:$it),
                    "IOR $id, $is, $it", [(set IntRegs:$id, (or IntRegs:$is, IntRegs:$it))], 0b0000, IIC_ialu_instr>;

//// Pseudo instructions.
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary>
   : LowerInst<outs, ins, asmstr, pattern, itin> {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
}


let isCodeGenOnly = 1, isPseudo = 1 in
{
def ILW_PSEUDO : Pseudo<(outs IntRegs:$dst), (ins (MEMri $is, $simm13):$addr),
        "ILW_PSEUDO $dst, $addr",
        [(set IntRegs:$dst, (load ADDRri:$addr))],
        IIC_ldst_instr>;
def ISW_PSEUDO : Pseudo<(outs), (ins IntRegs:$src, (MEMri $is, $simm13):$addr),
        "ISW_PSEUDO $src, $addr",
        [(store IntRegs:$src, ADDRri:$addr)],
        IIC_ldst_instr>;
}

let usesCustomInserter = 1, isCodeGenOnly = 1, isPseudo = 1, isBranch = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1 in
{
  def BR_CC_PSEUDOrr
   : Pseudo<(outs), (ins i16imm:$a, IntRegs:$b, IntRegs:$c, brtarget:$d),
            "; BR_CC_PSEUDO PSEUDO!",
            //[(PS2VPUbrccz cond:$a, Any:$b, Any:$c, bb:$d)]>;
            [(PS2VPUbricc (i16 imm:$a), IntRegs:$b, IntRegs:$c, bb:$d)]>;
  def BR_CC_PSEUDOri
   : Pseudo<(outs), (ins i16imm:$a, IntRegs:$b, i16imm:$c, brtarget:$d),
            "; BR_CC_PSEUDO PSEUDO!",
            //[(PS2VPUbrccz cond:$a, Any:$b, Any:$c, bb:$d)]>;
            [(PS2VPUbricc (i16 imm:$a), IntRegs:$b, imm:$c, bb:$d)]>;
}

//// upper instructions
multiclass VecInstrUpper0<string OpcStr, bits<4> OpVal, SDNode OpNode, RegisterClass BroadcastRC, InstrItinClass itin> {
    def x : Upper0InstPS2VPU<OpVal, (outs FloatXRegs:$fd), (ins FloatXRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".x $fd, $fs, $ft"),
                [(set FloatXRegs:$fd, (OpNode FloatXRegs:$fs, BroadcastRC:$ft))],
                0b1000,
                itin>;
    def y : Upper0InstPS2VPU<OpVal, (outs FloatYRegs:$fd), (ins FloatYRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".y $fd, $fs, $ft"),
                [(set FloatYRegs:$fd, (OpNode FloatYRegs:$fs, BroadcastRC:$ft))],
                0b0100,
                itin>;
    def z : Upper0InstPS2VPU<OpVal, (outs FloatZRegs:$fd), (ins FloatZRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".z $fd, $fs, $ft"),
                [(set FloatZRegs:$fd, (OpNode FloatZRegs:$fs, BroadcastRC:$ft))],
                0b0010,
                itin>;
    def w : Upper0InstPS2VPU<OpVal, (outs FloatWRegs:$fd), (ins FloatWRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".w $fd, $fs, $ft"),
                [(set FloatWRegs:$fd, (OpNode FloatWRegs:$fs, BroadcastRC:$ft))],
                0b0001,
                itin>;

    let isCodeGenOnly = 1 in
    def v4 : Upper0InstPS2VPU<OpVal, (outs VFRegs:$fd), (ins VFRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".xyzw $fd, $fs, $ft"),
                [], 
                0b1111,
                itin>;
}
multiclass VecInstrUpper1<string OpcStr, bits<6> OpVal, SDNode OpNode, InstrItinClass itin> {
    def x : Upper1InstPS2VPU<OpVal, (outs FloatXRegs:$fd), (ins FloatXRegs:$fs, FloatXRegs:$ft),
                !strconcat(OpcStr, ".x $fd, $fs, $ft"),
                [(set FloatXRegs:$fd, (OpNode FloatXRegs:$fs, FloatXRegs:$ft))],
                0b1000,
                itin>;
    def y : Upper1InstPS2VPU<OpVal, (outs FloatYRegs:$fd), (ins FloatYRegs:$fs, FloatYRegs:$ft),
                !strconcat(OpcStr, ".y $fd, $fs, $ft"),
                [(set FloatYRegs:$fd, (OpNode FloatYRegs:$fs, FloatYRegs:$ft))],
                0b0100,
                itin>;
    def z : Upper1InstPS2VPU<OpVal, (outs FloatZRegs:$fd), (ins FloatZRegs:$fs, FloatZRegs:$ft),
                !strconcat(OpcStr, ".z $fd, $fs, $ft"),
                [(set FloatZRegs:$fd, (OpNode FloatZRegs:$fs, FloatZRegs:$ft))],
                0b0010,
                itin>;
    def w : Upper1InstPS2VPU<OpVal, (outs FloatWRegs:$fd), (ins FloatWRegs:$fs, FloatWRegs:$ft),
                !strconcat(OpcStr, ".w $fd, $fs, $ft"),
                [(set FloatWRegs:$fd, (OpNode FloatWRegs:$fs, FloatWRegs:$ft))],
                0b0001,
                itin>;

    def v4 : Upper1InstPS2VPU<OpVal, (outs VFRegs:$fd), (ins VFRegs:$fs, VFRegs:$ft),
                !strconcat(OpcStr, ".xyzw $fd, $fs, $ft"),
                [(set VFRegs:$fd, (OpNode VFRegs:$fs, VFRegs:$ft))], 
                0b1111,
                itin>;
}
multiclass VecInstrUpper1WithACC<string OpcStr, bits<6> OpVal, SDNode OpNode, InstrItinClass itin> {
    def x : Upper1InstPS2VPU<OpVal, (outs FloatXRegs:$fd), (ins FloatXRegs:$fs, FloatXRegs:$ft, ACCxRegs:$acc),
                !strconcat(OpcStr, ".x $fd, $fs, $ft, $acc"),
                [(set FloatXRegs:$fd, (OpNode FloatXRegs:$fs, FloatXRegs:$ft, ACCxRegs:$acc))],
                0b1000,
                itin>;
    def y : Upper1InstPS2VPU<OpVal, (outs FloatYRegs:$fd), (ins FloatYRegs:$fs, FloatYRegs:$ft, ACCyRegs:$acc),
                !strconcat(OpcStr, ".y $fd, $fs, $ft, $acc"),
                [(set FloatYRegs:$fd, (OpNode FloatYRegs:$fs, FloatYRegs:$ft, ACCyRegs:$acc))],
                0b0100,
                itin>;
    def z : Upper1InstPS2VPU<OpVal, (outs FloatZRegs:$fd), (ins FloatZRegs:$fs, FloatZRegs:$ft, ACCzRegs:$acc),
                !strconcat(OpcStr, ".z $fd, $fs, $ft, $acc"),
                [(set FloatZRegs:$fd, (OpNode FloatZRegs:$fs, FloatZRegs:$ft, ACCzRegs:$acc))],
                0b0010,
                itin>;
    def w : Upper1InstPS2VPU<OpVal, (outs FloatWRegs:$fd), (ins FloatWRegs:$fs, FloatWRegs:$ft, ACCwRegs:$acc),
                !strconcat(OpcStr, ".w $fd, $fs, $ft, $acc"),
                [(set FloatWRegs:$fd, (OpNode FloatWRegs:$fs, FloatWRegs:$ft, ACCwRegs:$acc))],
                0b0001,
                itin>;

    def v4 : Upper1InstPS2VPU<OpVal, (outs VFRegs:$fd), (ins VFRegs:$fs, VFRegs:$ft, ACCRegs:$acc),
                !strconcat(OpcStr, ".xyzw $fd, $fs, $ft, $acc"),
                [(set VFRegs:$fd, (OpNode VFRegs:$fs, VFRegs:$ft, ACCRegs:$acc))], 
                0b1111,
                itin>;
}
multiclass VecInstrUpper2<string OpcStr, bits<5> OpVal, SDNode OpNode, RegisterClass BroadcastRC, InstrItinClass itin> {
    def x : Upper2InstPS2VPU<OpVal, (outs ACCxRegs:$acc), (ins FloatXRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".x $acc, $fs, $ft"),
                [(set ACCxRegs:$acc, (OpNode FloatXRegs:$fs, BroadcastRC:$ft))],
                0b1000,
                itin>;
    def y : Upper2InstPS2VPU<OpVal, (outs ACCyRegs:$acc), (ins FloatYRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".y $acc, $fs, $ft"),
                [(set ACCyRegs:$acc, (OpNode FloatYRegs:$fs, BroadcastRC:$ft))],
                0b0100,
                itin>;
    def z : Upper2InstPS2VPU<OpVal, (outs ACCzRegs:$acc), (ins FloatZRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".z $acc, $fs, $ft"),
                [(set ACCzRegs:$acc, (OpNode FloatZRegs:$fs, BroadcastRC:$ft))],
                0b0010,
                itin>;
    def w : Upper2InstPS2VPU<OpVal, (outs ACCwRegs:$acc), (ins FloatWRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".w $acc, $fs, $ft"),
                [(set ACCwRegs:$acc, (OpNode FloatWRegs:$fs, BroadcastRC:$ft))],
                0b0001,
                itin>;

    let isCodeGenOnly = 1 in
    def v4 : Upper2InstPS2VPU<OpVal, (outs ACCRegs:$acc), (ins VFRegs:$fs, BroadcastRC:$ft),
                !strconcat(OpcStr, ".xyzw $acc, $fs, $ft"),
                [], //(set ACCRegs:$acc, (OpNode FloatRegs:$fs, BroadcastRC:$ft))
                0b1111,
                itin>;
}

multiclass VecInstrUpper3<string OpcStr, bits<11> OpVal, SDNode OpNode, InstrItinClass itin> {
    def x : Upper3InstPS2VPU<OpVal, (outs ACCxRegs:$acc), (ins FloatXRegs:$fs, FloatXRegs:$ft),
                !strconcat(OpcStr, ".x $acc, $fs, $ft"),
                [(set ACCxRegs:$acc, (OpNode FloatXRegs:$fs, FloatXRegs:$ft))],
                0b1000,
                itin>;
    def y : Upper3InstPS2VPU<OpVal, (outs ACCyRegs:$acc), (ins FloatYRegs:$fs, FloatYRegs:$ft),
                !strconcat(OpcStr, ".y $acc, $fs, $ft"),
                [(set ACCyRegs:$acc, (OpNode FloatYRegs:$fs, FloatYRegs:$ft))],
                0b0100,
                itin>;
    def z : Upper3InstPS2VPU<OpVal, (outs ACCzRegs:$acc), (ins FloatZRegs:$fs, FloatZRegs:$ft),
                !strconcat(OpcStr, ".z $acc, $fs, $ft"),
                [(set ACCzRegs:$acc, (OpNode FloatZRegs:$fs, FloatZRegs:$ft))],
                0b0010,
                itin>;
    def w : Upper3InstPS2VPU<OpVal, (outs ACCwRegs:$acc), (ins FloatWRegs:$fs, FloatWRegs:$ft),
                !strconcat(OpcStr, ".w $acc, $fs, $ft"),
                [(set ACCwRegs:$acc, (OpNode FloatWRegs:$fs, FloatWRegs:$ft))],
                0b0001,
                itin>;

    def v4 : Upper3InstPS2VPU<OpVal, (outs ACCRegs:$acc), (ins VFRegs:$fs, VFRegs:$ft),
                !strconcat(OpcStr, ".xyzw $acc, $fs, $ft"),
                [(set ACCRegs:$acc, (OpNode VFRegs:$fs, VFRegs:$ft))], 
                0b1111,
                itin>;
}


defm ADD : VecInstrUpper1<"ADD", 0b101000, fadd, IIC_fmac_instr>;
defm ADDbc : VecInstrUpper0<"ADDbc", 0b0000, fadd, FloatRegs, IIC_fmac_instr>;
defm ADDA : VecInstrUpper3<"ADDA", 0b01010111100, fadd, IIC_fmac_instr>;
defm ADDAbc : VecInstrUpper2<"ADDAbc", 0b00000, fadd, FloatRegs, IIC_fmac_instr>;

defm MADD : VecInstrUpper1WithACC<"MADD", 0b101001, fmad, IIC_fmac_instr>;

def ADDiv4 : Upper1InstPS2VPU<0b100010, (outs FloatRegs:$fd), (ins FloatRegs:$fs, IRegs:$i),
                    "ADDi $fd, $fs, $i", [(set FloatRegs:$fd, (fadd FloatRegs:$fs, IRegs:$i))], 0b0000, IIC_fmac_instr>;

let isCodeGenOnly = 1 in  
def MUL : Upper1InstPS2VPU<0b101010, (outs FloatRegs:$fd), (ins FloatRegs:$fs, FloatRegs:$ft),
                    "MUL $fd, $fs, $ft", [(set FloatRegs:$fd, (fmul FloatRegs:$fs, FloatRegs:$ft))], 0b0000, IIC_fmac_instr>;
def MULv4 : Upper1InstPS2VPU<0b101010, (outs VFRegs:$fd), (ins VFRegs:$fs, VFRegs:$ft),
                    "MUL $fd, $fs, $ft", [(set VFRegs:$fd, (fmul VFRegs:$fs, VFRegs:$ft))], 0b0000, IIC_fmac_instr>;
let isCodeGenOnly = 1 in  
def SUB : Upper1InstPS2VPU<0b101100, (outs FloatRegs:$fd), (ins FloatRegs:$fs, FloatRegs:$ft),
                    "SUB $fd, $fs, $ft", [(set FloatRegs:$fd, (fsub FloatRegs:$fs, FloatRegs:$ft))], 0b0000, IIC_fmac_instr>;
def SUBv4 : Upper1InstPS2VPU<0b101100, (outs VFRegs:$fd), (ins VFRegs:$fs, VFRegs:$ft),
                    "SUB $fd, $fs, $ft", [(set VFRegs:$fd, (fsub VFRegs:$fs, VFRegs:$ft))], 0b0000, IIC_fmac_instr>;    
//let isCodeGenOnly = 1 in

//def MADDx : Upper1InstPS2VPU<0b101001, (outs FloatXRegs:$fd), (ins FloatXRegs:$fs, FloatXRegs:$ft, ACCxRegs:$acc),
//                    "MADD.x $fd, $fs, $ft, ACCx", [(set FloatXRegs:$fd, (fmad FloatXRegs:$fs, FloatXRegs:$ft, ACCxRegs:$acc))], 0b0000, IIC_fmac_instr>;
//def MADDv4 : Upper1InstPS2VPU<0b101001, (outs VFRegs:$fd), (ins VFRegs:$fs, VFRegs:$ft, ACCRegs:$acc),
//                    "MADD $fd, $fs, $ft, $acc", [(set VFRegs:$fd, (fmad VFRegs:$fs, VFRegs:$ft, ACCRegs:$acc))], 0b0000, IIC_fmac_instr>;


let hasDelaySlot = 1, isBarrier = 1 in
{
defm LQ : VecLoadFloat7<"LQ", 0b0000000, IIC_ldst_instr>;
defm SQ : VecStoreFloat7<"SQ", 0b0000001, IIC_ldst_instr>;

def LQ_PSEUDO : Pseudo<(outs FloatRegs:$dst), (ins (MEMri $is, $simm13):$addr),
        "LQ_PSEUDO $dst, $addr",
        [(set FloatRegs:$dst, (load ADDRri:$addr))],
        IIC_ldst_instr>;
def SQ_PSEUDO : Pseudo<(outs), (ins FloatRegs:$src, (MEMri $is, $simm13):$addr),
        "SQ_PSEUDO $src, $addr",
        [(store FloatRegs:$src, ADDRri:$addr)],
        IIC_ldst_instr>;
}

let isCodeGenOnly = 1 in
{
def LOIv4 : Pseudo<(outs VFRegs:$fd), (ins f32imm:$a, f32imm:$b, f32imm:$c, f32imm:$d),
            "LOI $fd, $a, $b, $c, $d",
            [(set VFRegs:$fd, (build_vector (f32 fpimm:$a), (f32 fpimm:$b), (f32 fpimm:$c), (f32 fpimm:$d)))], IIC_ialu_instr>;
            
def LOIv1 : Pseudo<(outs FloatRegs:$fd), (ins f32imm:$a),
            "LOI $fd, $a",
            [], IIC_ialu_instr>;

def MOVEx : MoveInstr<0b01100111100, (outs FloatXRegs:$ft), (ins FloatXRegs:$fs),
                    "MOVE.x $ft, $fs", [], 0b1000, IIC_nop_4cycle_instr>; // (set FloatXRegs:$ft, FloatXRegs:$fs)
def MOVEy : MoveInstr<0b01100111100, (outs FloatYRegs:$ft), (ins FloatYRegs:$fs),
                    "MOVE.y $ft, $fs", [], 0b0100, IIC_nop_4cycle_instr>; // (set FloatYRegs:$ft, FloatYRegs:$fs)
def MOVEz : MoveInstr<0b01100111100, (outs FloatZRegs:$ft), (ins FloatZRegs:$fs),
                    "MOVE.z $ft, $fs", [], 0b0010, IIC_nop_4cycle_instr>; // (set FloatZRegs:$ft, FloatZRegs:$fs)
def MOVEw : MoveInstr<0b01100111100, (outs FloatWRegs:$ft), (ins FloatWRegs:$fs),
                    "MOVE.w $ft, $fs", [], 0b0001, IIC_nop_4cycle_instr>; // (set FloatWRegs:$ft, FloatWRegs:$fs)
def MOVEv4 : MoveInstr<0b01100111100, (outs VFRegs:$ft), (ins VFRegs:$fs),
                    "MOVE.xyzw $ft, $fs", [], 0b1111, IIC_nop_4cycle_instr>; //(set VFRegs:$ft, VFRegs:$fs)
                    
}

let isCodeGenOnly = 1 in
def NOP  : Upper3InstPS2VPU<0b01011111111, (outs), (ins),
                    "NOP", [], 0b0000, IIC_fmac_instr>;



//def VSLDOI_get_imm : SDNodeXForm<vector_shuffle, [{
//  return getI32Imm(PPC::isVSLDOIShuffleMask(N, 0, *CurDAG), SDLoc(N));
//}]>;
//def vsldoi_shuffle : PatFrag<(ops node:$lhs, node:$rhs),
//                             (vector_shuffle node:$lhs, node:$rhs), [{
//  return PPC::isVSLDOIShuffleMask(N, 0, *CurDAG) != -1;
//}], VSLDOI_get_imm>;

//def get_floatX :
//  PatFrag<(ops node:$src),
//          (VFRegs:$src), [{}], [{ return  }]>;

def : Pat<(vector_extract (v4f32 VFRegs:$fs), 0), (f32 (EXTRACT_SUBREG VFRegs:$fs, sub_x))>;
def : Pat<(vector_extract (v4f32 VFRegs:$fs), 1), (f32 (EXTRACT_SUBREG VFRegs:$fs, sub_y))>;
def : Pat<(vector_extract (v4f32 VFRegs:$fs), 2), (f32 (EXTRACT_SUBREG VFRegs:$fs, sub_z))>;
def : Pat<(vector_extract (v4f32 VFRegs:$fs), 3), (f32 (EXTRACT_SUBREG VFRegs:$fs, sub_w))>;

def : Pat<(f32 fpimm:$val), (LOIv1 fpimm:$val)>;
def : Pat<(v4f32 immAllZerosV), // FIXME: that's wrong, VF0 is (0, 0, 0, 1)
          (v4f32 VF0)>;

// Zero immediate.
def : Pat<(i16 0),
          (i16 VI0)>;

// Small immediates.
def : Pat<(i16 simm5:$val),
          (IADDri (i16 VI0), (imm:$val))>;
          
def : Pat<(i16 pimm16:$val),
          (IADDUri (i16 VI0), imm:$val)>;
          
def : Pat<(i16 nimm16:$val),
          (ISUBUri (i16 VI0), (IMMNEG imm:$val))>;
