////===-- PS2VPUISelLowering.cpp - PS2VPU DAG Lowering Implementation ---------===//
////
//// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
//// See https://llvm.org/LICENSE.txt for license information.
//// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
////
////===----------------------------------------------------------------------===//
////
//// This file implements the interfaces that PS2VPU uses to lower LLVM code into a
//// selection DAG.
////
////===----------------------------------------------------------------------===//
//
#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "PS2VPUISelLowering.h"
#include "PS2VPUMachineFunctionInfo.h"
#include "PS2VPURegisterInfo.h"
#include "PS2VPUTargetMachine.h"
#include "PS2VPUTargetObjectFile.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"

#define DEBUG_TYPE "baba"
using namespace llvm;

#include "PS2VPUGenCallingConv.inc"
//
//// The calling conventions in PS2VPUCallingConv.td are described in terms of the
//// callee's register window. This function translates registers to the
//// corresponding caller window %o register.
static unsigned toCallerWindow(unsigned Reg) {
  /*static_assert(SP::I0 + 7 == SP::I7 && SP::O0 + 7 == SP::O7,
                "Unexpected enum");*/

    // I have no fucking clue what the fuck that is
  if (Reg >= PS2VPUNS::VI0 && Reg <= PS2VPUNS::VI7)
    return Reg - PS2VPUNS::VI0 + PS2VPUNS::VI8;
  return Reg;
}
//
SDValue
PS2VPUTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                 bool IsVarArg,
                                 const SmallVectorImpl<ISD::OutputArg> &Outs,
                                 const SmallVectorImpl<SDValue> &OutVals,
                                 const SDLoc &DL, SelectionDAG &DAG) const {
  //if (Subtarget->is64Bit())
  //  return LowerReturn_64(Chain, CallConv, IsVarArg, Outs, OutVals, DL, DAG);
  return LowerReturn_32(Chain, CallConv, IsVarArg, Outs, OutVals, DL, DAG);
}

SDValue
PS2VPUTargetLowering::LowerReturn_32(SDValue Chain, CallingConv::ID CallConv,
                                    bool IsVarArg,
                                    const SmallVectorImpl<ISD::OutputArg> &Outs,
                                    const SmallVectorImpl<SDValue> &OutVals,
                                    const SDLoc &DL, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();

  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  // Analyze return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_PS2VPU);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);
  // Make room for the return address offset.
  RetOps.push_back(SDValue());

  // Copy the result values into the output registers.
  for (unsigned i = 0, realRVLocIdx = 0; i != RVLocs.size();
       ++i, ++realRVLocIdx) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    SDValue Arg = OutVals[realRVLocIdx];

    if (VA.needsCustom()) {
      llvm_unreachable("LowerReturn_32 VA.needsCustom() not supported");
      //assert(VA.getLocVT() == MVT::v2i32);
      //// Legalize ret v2i32 -> ret 2 x i32 (Basically: do what would
      //// happen by default if this wasn't a legal type)

      //SDValue Part0 = DAG.getNode(
      //    ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32, Arg,
      //    DAG.getConstant(0, DL, getVectorIdxTy(DAG.getDataLayout())));
      //SDValue Part1 = DAG.getNode(
      //    ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32, Arg,
      //    DAG.getConstant(1, DL, getVectorIdxTy(DAG.getDataLayout())));

      //Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part0, Flag);
      //Flag = Chain.getValue(1);
      //RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
      //VA = RVLocs[++i]; // skip ahead to next loc
      //Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part1, Flag);
    } else
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Arg, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned RetAddrOffset = 8; // Call Inst + Delay Slot
  // If the function returns a struct, copy the SRetReturnReg to I0
  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("LowerReturn_32 hasStructRetAttr() not supported");
    //PS2VPUMachineFunctionInfo *SFI = MF.getInfo<PS2VPUMachineFunctionInfo>();
    //Register Reg = SFI->getSRetReturnReg();
    //if (!Reg)
    //  llvm_unreachable("sret virtual register not created in the entry block");
    //auto PtrVT = getPointerTy(DAG.getDataLayout());
    //SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg, PtrVT);
    //Chain = DAG.getCopyToReg(Chain, DL, PS2VPUNS::VI1, Val, Flag);
    //Flag = Chain.getValue(1);
    //RetOps.push_back(DAG.getRegister(PS2VPUNS::VI1, PtrVT));
    //RetAddrOffset = 12; // CallInst + Delay Slot + Unimp
  }

  RetOps[0] = Chain; // Update chain.
  RetOps[1] = DAG.getRegister(
      PS2VPUNS::VI15,
      getPointerTy(DAG.getDataLayout())); // DAG.getConstant(RetAddrOffset,
                                          // DL, MVT::i16);

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(PS2VPUISD::RET_FLAG, DL, MVT::Other, RetOps);
}

SDValue PS2VPUTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  /*if (Subtarget->is64Bit())
    return LowerFormalArguments_64(Chain, CallConv, IsVarArg, Ins, DL, DAG,
                                   InVals);*/
  return LowerFormalArguments_32(Chain, CallConv, IsVarArg, Ins, DL, DAG,
                                 InVals);
}
//
///// LowerFormalArguments32 - V8 uses a very simple ABI, where all values are
///// passed in either one or two GPRs, including FP values.  TODO: we should
///// pass FP values in FP registers for fastcc functions.
SDValue PS2VPUTargetLowering::LowerFormalArguments_32(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  PS2VPUMachineFunctionInfo *FuncInfo = MF.getInfo<PS2VPUMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_PS2VPU);

  const unsigned StackOffset = 92;
  bool IsLittleEndian = DAG.getDataLayout().isLittleEndian();

  unsigned InIdx = 0;
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i, ++InIdx) {
    CCValAssign &VA = ArgLocs[i];

    if (Ins[InIdx].Flags.isSRet()) {
      llvm_unreachable(
          "LowerFormalArguments_32 VA.needsCustom() not supported");
      //if (InIdx != 0)
      //  report_fatal_error("PS2VPU only supports sret on the first parameter");
      //// Get SRet from [%fp+64].
      //int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, 64, true);
      //SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
      //SDValue Arg =
      //    DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      //InVals.push_back(Arg);
      //continue;
    }

    if (VA.isRegLoc()) {
      if (VA.needsCustom()) {
        /*assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

        Register VRegHi = RegInfo.createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
        MF.getRegInfo().addLiveIn(VA.getLocReg(), VRegHi);
        SDValue HiVal = DAG.getCopyFromReg(Chain, dl, VRegHi, MVT::i32);

        assert(i + 1 < e);
        CCValAssign &NextVA = ArgLocs[++i];

        SDValue LoVal;
        if (NextVA.isMemLoc()) {
          int FrameIdx = MF.getFrameInfo().CreateFixedObject(
              4, StackOffset + NextVA.getLocMemOffset(), true);
          SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
          LoVal = DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
        } else {
          Register loReg =
              MF.addLiveIn(NextVA.getLocReg(), &PS2VPUNS::IntRegsRegClass);
          LoVal = DAG.getCopyFromReg(Chain, dl, loReg, MVT::i32);
        }

        if (IsLittleEndian)
          std::swap(LoVal, HiVal);

        SDValue WholeValue =
            DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
        WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), WholeValue);
        InVals.push_back(WholeValue);
        continue;*/
        llvm_unreachable("LowerFormalArguments_32 VA.needsCustom() not supported");
      }
      Register VReg = RegInfo.createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
      MF.getRegInfo().addLiveIn(VA.getLocReg(), VReg);
      SDValue Arg = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i16);
      /*if (VA.getLocVT() == MVT::f32)
        Arg = DAG.getNode(ISD::BITCAST, dl, MVT::f32, Arg);
      else */if (VA.getLocVT() != MVT::i16) {
        Arg = DAG.getNode(ISD::AssertSext, dl, MVT::i16, Arg,
                          DAG.getValueType(VA.getLocVT()));
        Arg = DAG.getNode(ISD::TRUNCATE, dl, VA.getLocVT(), Arg);
      }
      InVals.push_back(Arg);
      continue;
    }

    assert(VA.isMemLoc());

    unsigned Offset = VA.getLocMemOffset() + StackOffset;
    auto PtrVT = getPointerTy(DAG.getDataLayout());

    if (VA.needsCustom()) {
      //assert(VA.getValVT() == MVT::f64 || VA.getValVT() == MVT::v2i32);
      //// If it is double-word aligned, just load.
      //if (Offset % 8 == 0) {
      //  int FI = MF.getFrameInfo().CreateFixedObject(8, Offset, true);
      //  SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
      //  SDValue Load =
      //      DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
      //  InVals.push_back(Load);
      //  continue;
      //}

      //int FI = MF.getFrameInfo().CreateFixedObject(4, Offset, true);
      //SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
      //SDValue HiVal =
      //    DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      //int FI2 = MF.getFrameInfo().CreateFixedObject(4, Offset + 4, true);
      //SDValue FIPtr2 = DAG.getFrameIndex(FI2, PtrVT);

      //SDValue LoVal =
      //    DAG.getLoad(MVT::i32, dl, Chain, FIPtr2, MachinePointerInfo());

      //if (IsLittleEndian)
      //  std::swap(LoVal, HiVal);

      //SDValue WholeValue =
      //    DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
      //WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getValVT(), WholeValue);
      //InVals.push_back(WholeValue);
      //continue;
      llvm_unreachable(
          "LowerFormalArguments_32 VA.needsCustom() not supported");
    }

    int FI = MF.getFrameInfo().CreateFixedObject(4, Offset, true);
    SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
    SDValue Load;
    if (VA.getValVT() == MVT::i16 || VA.getValVT() == MVT::f32) {
      Load = DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
    /*} else if (VA.getValVT() == MVT::f128) {
      report_fatal_error("PS2VPUv8 does not handle f128 in calls; "
                         "pass indirectly");*/
    } else {
      // We shouldn't see any other value types here.
      llvm_unreachable("Unexpected ValVT encountered in frame lowering.");
    }
    InVals.push_back(Load);
  }

  if (MF.getFunction().hasStructRetAttr()) {
    // Copy the SRet Argument to SRetReturnReg.
    /*PS2VPUMachineFunctionInfo *SFI = MF.getInfo<PS2VPUMachineFunctionInfo>();
    Register Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
      SFI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);*/
    llvm_unreachable("LowerFormalArguments_32 sret not supported for now");
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    llvm_unreachable("LowerFormalArguments_32 vaarg not supported for now");
    //static const MCPhysReg ArgRegs[] = {
    //    PS2VPUNS::VI1, PS2VPUNS::VI2, PS2VPUNS::VI3,
    //                                    PS2VPUNS::VI4, PS2VPUNS::VI5};
    //unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs);
    //const MCPhysReg *CurArgReg = ArgRegs + NumAllocated,
    //                *ArgRegEnd = ArgRegs + 6;
    //unsigned ArgOffset = CCInfo.getNextStackOffset();
    //if (NumAllocated == 6)
    //  ArgOffset += StackOffset;
    //else {
    //  assert(!ArgOffset);
    //  ArgOffset = 68 + 4 * NumAllocated;
    //}

    //// Remember the vararg offset for the va_start implementation.
    //FuncInfo->setVarArgsFrameOffset(ArgOffset);

    //std::vector<SDValue> OutChains;

    //for (; CurArgReg != ArgRegEnd; ++CurArgReg) {
    //  Register VReg = RegInfo.createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
    //  MF.getRegInfo().addLiveIn(*CurArgReg, VReg);
    //  SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), dl, VReg, MVT::i32);

    //  int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, ArgOffset, true);
    //  SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);

    //  OutChains.push_back(
    //      DAG.getStore(DAG.getRoot(), dl, Arg, FIPtr, MachinePointerInfo()));
    //  ArgOffset += 4;
    //}

    //if (!OutChains.empty()) {
    //  OutChains.push_back(Chain);
    //  Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
    //}
  }

  return Chain;
}

SDValue PS2VPUTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                       SmallVectorImpl<SDValue> &InVals) const {
  /*if (Subtarget->is64Bit())
    return LowerCall_64(CLI, InVals);*/
  return LowerCall_32(CLI, InVals);
}

static bool hasReturnsTwiceAttr(SelectionDAG &DAG, SDValue Callee,
                                const CallBase *Call) {
  if (Call)
    return Call->hasFnAttr(Attribute::ReturnsTwice);

  const Function *CalleeFn = nullptr;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    CalleeFn = dyn_cast<Function>(G->getGlobal());
  } else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const Function &Fn = DAG.getMachineFunction().getFunction();
    const Module *M = Fn.getParent();
    const char *CalleeName = E->getSymbol();
    CalleeFn = M->getFunction(CalleeName);
  }

  if (!CalleeFn)
    return false;
  return CalleeFn->hasFnAttribute(Attribute::ReturnsTwice);
}

///// IsEligibleForTailCallOptimization - Check whether the call is eligible
///// for tail call optimization.
bool PS2VPUTargetLowering::IsEligibleForTailCallOptimization(
    CCState &CCInfo, CallLoweringInfo &CLI, MachineFunction &MF) const {

  auto &Outs = CLI.Outs;
  auto &Caller = MF.getFunction();

  // Do not tail call opt functions with "disable-tail-calls" attribute.
  if (Caller.getFnAttribute("disable-tail-calls").getValueAsString() == "true")
    return false;

  // Do not tail call opt if the stack is used to pass parameters.
  unsigned StackSizeLimit = 0;
  if (CCInfo.getStackSize() > StackSizeLimit)
    return false;

  // Do not tail call opt if either the callee or caller returns
  // a struct and the other does not.
  if (!Outs.empty() && Caller.hasStructRetAttr() != Outs[0].Flags.isSRet())
    return false;

  // Byval parameters hand the function a pointer directly into the stack area
  // we want to reuse during a tail call.
  for (auto &Arg : Outs)
    if (Arg.Flags.isByVal())
      return false;

  return true;
}

//// Lower a call for the 32-bit ABI.
SDValue
PS2VPUTargetLowering::LowerCall_32(TargetLowering::CallLoweringInfo &CLI,
                                  SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &dl = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  bool &isTailCall = CLI.IsTailCall;
  CallingConv::ID CallConv = CLI.CallConv;
  bool isVarArg = CLI.IsVarArg;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_PS2VPU);

  isTailCall = isTailCall && IsEligibleForTailCallOptimization(
                                 CCInfo, CLI, DAG.getMachineFunction());

  // Get the size of the outgoing arguments stack space requirement.
  unsigned ArgsSize = CCInfo.getStackSize();

  // Keep stack frames 8-byte aligned.
  ArgsSize = (ArgsSize + 7) & ~7;

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

  // Create local copies for byval args.
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0, e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    Align Alignment = Flags.getNonZeroByValAlign();

    if (Size > 0U) {
      int FI = MFI.CreateStackObject(Size, Alignment, false);
      SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
      SDValue SizeNode = DAG.getConstant(Size, dl, MVT::i16);

      Chain = DAG.getMemcpy(Chain, dl, FIPtr, Arg, SizeNode, Alignment,
                            false,        // isVolatile,
                            (Size <= 32), // AlwaysInline if size <= 32,
                            false,        // isTailCall
                            MachinePointerInfo(), MachinePointerInfo());
      ByValArgs.push_back(FIPtr);
    } else {
      SDValue nullVal;
      ByValArgs.push_back(nullVal);
    }
  }

  assert(!isTailCall || ArgsSize == 0);

  if (!isTailCall)
    Chain = DAG.getCALLSEQ_START(Chain, ArgsSize, 0, dl);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  const unsigned StackOffset = 92;
  bool hasStructRetAttr = false;
  unsigned SRetArgSize = 0;
  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, realArgIdx = 0, byvalArgIdx = 0, e = ArgLocs.size();
       i != e; ++i, ++realArgIdx) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[realArgIdx];

    ISD::ArgFlagsTy Flags = Outs[realArgIdx].Flags;

    // Use local copy if it is a byval arg.
    if (Flags.isByVal()) {
      Arg = ByValArgs[byvalArgIdx++];
      if (!Arg) {
        continue;
      }
    }

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), Arg);
      break;
    }

    if (Flags.isSRet()) {
      assert(VA.needsCustom());

      if (isTailCall)
        continue;

      // store SRet argument in %sp+64
      SDValue StackPtr = DAG.getRegister(PS2VPUNS::VI6, MVT::i16);
      SDValue PtrOff = DAG.getIntPtrConstant(64, dl);
      PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
      MemOpChains.push_back(
          DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
      hasStructRetAttr = true;
      // sret only allowed on first argument
      assert(Outs[realArgIdx].OrigArgIndex == 0);
      SRetArgSize =
          DAG.getDataLayout().getTypeAllocSize(CLI.getArgs()[0].IndirectType);
      continue;
    }

    if (VA.needsCustom()) {
      assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

      if (VA.isMemLoc()) {
        unsigned Offset = VA.getLocMemOffset() + StackOffset;
        // if it is double-word aligned, just store.
        if (Offset % 8 == 0) {
          SDValue StackPtr = DAG.getRegister(PS2VPUNS::VI6, MVT::i16);
          SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
          PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
          MemOpChains.push_back(
              DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
          continue;
        }
      }

      if (VA.getLocVT() == MVT::f64) {
        // Move from the float value from float registers into the
        // integer registers.
        /*if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(Arg))
          Arg = bitcastConstantFPToInt(C, dl, DAG);
        else
          Arg = DAG.getNode(ISD::BITCAST, dl, MVT::v2i32, Arg);*/
        llvm_unreachable("VA.getLocVT() == MVT::f64");
      }

      SDValue Part0 = DAG.getNode(
          ISD::EXTRACT_VECTOR_ELT, dl, MVT::i16, Arg,
          DAG.getConstant(0, dl, getVectorIdxTy(DAG.getDataLayout())));
      SDValue Part1 = DAG.getNode(
          ISD::EXTRACT_VECTOR_ELT, dl, MVT::i16, Arg,
          DAG.getConstant(1, dl, getVectorIdxTy(DAG.getDataLayout())));

      if (VA.isRegLoc()) {
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), Part0));
        assert(i + 1 != e);
        CCValAssign &NextVA = ArgLocs[++i];
        if (NextVA.isRegLoc()) {
          RegsToPass.push_back(std::make_pair(NextVA.getLocReg(), Part1));
        } else {
          // Store the second part in stack.
          unsigned Offset = NextVA.getLocMemOffset() + StackOffset;
          SDValue StackPtr = DAG.getRegister(PS2VPUNS::VI6, MVT::i16);
          SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
          PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
          MemOpChains.push_back(
              DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
        }
      } else {
        unsigned Offset = VA.getLocMemOffset() + StackOffset;
        // Store the first part.
        SDValue StackPtr = DAG.getRegister(PS2VPUNS::VI6, MVT::i16);
        SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
        PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
        MemOpChains.push_back(
            DAG.getStore(Chain, dl, Part0, PtrOff, MachinePointerInfo()));
        // Store the second part.
        PtrOff = DAG.getIntPtrConstant(Offset + 4, dl);
        PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
        MemOpChains.push_back(
            DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
      }
      continue;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      if (VA.getLocVT() != MVT::f32) {
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
        continue;
      }
      Arg = DAG.getNode(ISD::BITCAST, dl, MVT::i16, Arg);
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      continue;
    }

    assert(VA.isMemLoc());

    // Create a store off the stack pointer for this argument.
    SDValue StackPtr = DAG.getRegister(PS2VPUNS::VI6, MVT::i16);
    SDValue PtrOff =
        DAG.getIntPtrConstant(VA.getLocMemOffset() + StackOffset, dl);
    PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i16, StackPtr, PtrOff);
    MemOpChains.push_back(
        DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
  }

  // Emit all stores, make sure the occur before any copies into physregs.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Register Reg = RegsToPass[i].first;
    if (!isTailCall)
      Reg = toCallerWindow(Reg);
    Chain = DAG.getCopyToReg(Chain, dl, Reg, RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  bool hasReturnsTwice = hasReturnsTwiceAttr(DAG, Callee, CLI.CB);

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  unsigned TF = isPositionIndependent() ? PS2VPUMCExpr::VK_PS2VPU_WPLT30
                                        : PS2VPUMCExpr::VK_PS2VPU_WDISP30;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i16, 0, TF);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i16, TF);

  // Returns a chain & a flag for retval copy to use
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);
  if (hasStructRetAttr)
    Ops.push_back(DAG.getTargetConstant(SRetArgSize, dl, MVT::i16));
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Register Reg = RegsToPass[i].first;
    if (!isTailCall)
      Reg = toCallerWindow(Reg);
    Ops.push_back(DAG.getRegister(Reg, RegsToPass[i].second.getValueType()));
  }

  // Add a register mask operand representing the call-preserved registers.
  const PS2VPURegisterInfo *TRI = Subtarget->getRegisterInfo();
  const uint32_t *Mask =
      ((hasReturnsTwice)
           ? TRI->getRTCallPreservedMask(CallConv)
           : TRI->getCallPreservedMask(DAG.getMachineFunction(), CallConv));
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (InFlag.getNode())
    Ops.push_back(InFlag);

  if (isTailCall) {
    DAG.getMachineFunction().getFrameInfo().setHasTailCall();
    return DAG.getNode(PS2VPUISD::TAIL_CALL, dl, MVT::Other, Ops);
  }

  Chain = DAG.getNode(PS2VPUISD::CALL, dl, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(Chain, ArgsSize, 0, InFlag, dl);
  InFlag = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  RVInfo.AnalyzeCallResult(Ins, RetCC_PS2VPU);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    if (RVLocs[i].getLocVT() == MVT::v2i16) {
      SDValue Vec = DAG.getNode(ISD::UNDEF, dl, MVT::v2i16);
      SDValue Lo = DAG.getCopyFromReg(
          Chain, dl, toCallerWindow(RVLocs[i++].getLocReg()), MVT::i16, InFlag);
      Chain = Lo.getValue(1);
      InFlag = Lo.getValue(2);
      Vec = DAG.getNode(ISD::INSERT_VECTOR_ELT, dl, MVT::v2i16, Vec, Lo,
                        DAG.getConstant(0, dl, MVT::i16));
      SDValue Hi = DAG.getCopyFromReg(
          Chain, dl, toCallerWindow(RVLocs[i].getLocReg()), MVT::i16, InFlag);
      Chain = Hi.getValue(1);
      InFlag = Hi.getValue(2);
      Vec = DAG.getNode(ISD::INSERT_VECTOR_ELT, dl, MVT::v2i16, Vec, Hi,
                        DAG.getConstant(1, dl, MVT::i16));
      InVals.push_back(Vec);
    } else {
      Chain =
          DAG.getCopyFromReg(Chain, dl, toCallerWindow(RVLocs[i].getLocReg()),
                             RVLocs[i].getValVT(), InFlag)
              .getValue(1);
      InFlag = Chain.getValue(2);
      InVals.push_back(Chain.getValue(0));
    }
  }

  return Chain;
}
//
//// FIXME? Maybe this could be a TableGen attribute on some registers and
//// this table could be generated automatically from RegInfo.
Register
PS2VPUTargetLowering::getRegisterByName(const char *RegName, LLT VT,
                                       const MachineFunction &MF) const {
  Register Reg = StringSwitch<Register>(RegName)
                     .Case("VI0", PS2VPUNS::VI0)
                     .Case("VI1", PS2VPUNS::VI1)
                     .Case("VI2", PS2VPUNS::VI2)
                     .Case("VI3", PS2VPUNS::VI3)
                     .Case("VI4", PS2VPUNS::VI4)
                     .Case("VI5", PS2VPUNS::VI5)
                     .Case("VI6", PS2VPUNS::VI6)
                     .Case("VI7", PS2VPUNS::VI7)
                     .Case("VI8", PS2VPUNS::VI8)
                     .Case("VI9", PS2VPUNS::VI9)
                     .Case("VI10", PS2VPUNS::VI10)
                     .Case("VI11", PS2VPUNS::VI11)
                     .Case("VI12", PS2VPUNS::VI12)
                     .Case("VI13", PS2VPUNS::VI13)
                     .Case("VI14", PS2VPUNS::VI14)
                     .Case("VI15", PS2VPUNS::VI15)
                     .Default(0);

  if (Reg)
    return Reg;

  report_fatal_error("Invalid register name global variable");
}

//
////===----------------------------------------------------------------------===//
//// TargetLowering Implementation
////===----------------------------------------------------------------------===//
//
TargetLowering::AtomicExpansionKind
PS2VPUTargetLowering::shouldExpandAtomicRMWInIR(AtomicRMWInst *AI) const {
  if (AI->getOperation() == AtomicRMWInst::Xchg &&
      AI->getType()->getPrimitiveSizeInBits() == 32)
    return AtomicExpansionKind::None; // Uses xchg instruction

  return AtomicExpansionKind::CmpXChg;
}
//
///// IntCondCCodeToICC - Convert a DAG integer condition code to a PS2VPU ICC
///// condition.
static PS2VPUNS::CondCodes IntCondCCodeToICC(ISD::CondCode CC) {
  switch (CC) {
  default:
    llvm_unreachable("Unknown integer condition code!");
  case ISD::SETEQ:
    return PS2VPUNS::ICC_E;
  case ISD::SETNE:
    return PS2VPUNS::ICC_NE;
  case ISD::SETLT:
    return PS2VPUNS::ICC_L;
  case ISD::SETGT:
    return PS2VPUNS::ICC_G;
  case ISD::SETLE:
    return PS2VPUNS::ICC_LE;
  case ISD::SETGE:
    return PS2VPUNS::ICC_GE;
  case ISD::SETULT:
    return PS2VPUNS::ICC_CS;
  case ISD::SETULE:
    return PS2VPUNS::ICC_LEU;
  case ISD::SETUGT:
    return PS2VPUNS::ICC_GU;
  case ISD::SETUGE:
    return PS2VPUNS::ICC_CC;
  }
}
//
///// FPCondCCodeToFCC - Convert a DAG floatingp oint condition code to a PS2VPU
///// FCC condition.
//static SPCC::CondCodes FPCondCCodeToFCC(ISD::CondCode CC) {
//  switch (CC) {
//  default:
//    llvm_unreachable("Unknown fp condition code!");
//  case ISD::SETEQ:
//  case ISD::SETOEQ:
//    return SPCC::FCC_E;
//  case ISD::SETNE:
//  case ISD::SETUNE:
//    return SPCC::FCC_NE;
//  case ISD::SETLT:
//  case ISD::SETOLT:
//    return SPCC::FCC_L;
//  case ISD::SETGT:
//  case ISD::SETOGT:
//    return SPCC::FCC_G;
//  case ISD::SETLE:
//  case ISD::SETOLE:
//    return SPCC::FCC_LE;
//  case ISD::SETGE:
//  case ISD::SETOGE:
//    return SPCC::FCC_GE;
//  case ISD::SETULT:
//    return SPCC::FCC_UL;
//  case ISD::SETULE:
//    return SPCC::FCC_ULE;
//  case ISD::SETUGT:
//    return SPCC::FCC_UG;
//  case ISD::SETUGE:
//    return SPCC::FCC_UGE;
//  case ISD::SETUO:
//    return SPCC::FCC_U;
//  case ISD::SETO:
//    return SPCC::FCC_O;
//  case ISD::SETONE:
//    return SPCC::FCC_LG;
//  case ISD::SETUEQ:
//    return SPCC::FCC_UE;
//  }
//}
//
PS2VPUTargetLowering::PS2VPUTargetLowering(const TargetMachine &TM,
                                         const PS2VPUSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {
  MVT PtrVT = MVT::getIntegerVT(TM.getPointerSizeInBits(0));

  // Instructions which use registers as conditionals examine all the
  // bits (as does the pseudo SELECT_CC expansion). I don't think it
  // matters much whether it's ZeroOrOneBooleanContent, or
  // ZeroOrNegativeOneBooleanContent, so, arbitrarily choose the
  // former.
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // Set up the register classes.
  addRegisterClass(MVT::i16, &PS2VPUNS::IntRegsRegClass);
  addRegisterClass(MVT::v4f32, &PS2VPUNS::VFRegsRegClass);
  addRegisterClass(MVT::f32, &PS2VPUNS::FloatRegsRegClass);
  /*setOperationAction(ISD::FrameIndex, MVT::i32, Expand);*/
  //if (!Subtarget->useSoftFloat()) {
  //  addRegisterClass(MVT::f32, &SP::FPRegsRegClass);
  //  addRegisterClass(MVT::f64, &SP::DFPRegsRegClass);
  //  addRegisterClass(MVT::f128, &SP::QFPRegsRegClass);
  //}
  //if (Subtarget->is64Bit()) {
  //  addRegisterClass(MVT::i64, &SP::I64RegsRegClass);
  //} else {
  //  // On 32bit PS2VPU, we define a double-register 32bit register
  //  // class, as well. This is modeled in LLVM as a 2-vector of i32.
  //  addRegisterClass(MVT::v2i32, &SP::IntPairRegClass);

   for (unsigned Op = 0; Op < ISD::BUILTIN_OP_END; ++Op) {
    setOperationAction(Op, MVT::i32, Expand);
  }
  
  // ...but almost all operations must be expanded, so set that as
  // the default.
  for (unsigned Op = 0; Op < ISD::BUILTIN_OP_END; ++Op) {
    /*setOperationAction(Op, MVT::v2i32, Expand);*/
    setOperationAction(Op, MVT::v2f32, LegalizeAction::Promote);
    setOperationAction(Op, MVT::v1f32, LegalizeAction::Promote);
    setOperationAction(Op, MVT::f32, LegalizeAction::Promote);
    AddPromotedToType(Op, MVT::f32, MVT::v4f32);
  }
  for (MVT VT : {MVT::f32, MVT::v4f32}) {
    setOperationAction(ISD::STORE, VT, LegalizeAction::Legal);
    setOperationAction(ISD::LOAD, VT, LegalizeAction::Legal);
    setOperationAction(ISD::FADD, VT, LegalizeAction::Legal);
    setOperationAction(ISD::FSUB, VT, LegalizeAction::Legal);
    setOperationAction(ISD::FMUL, VT, LegalizeAction::Legal);
    setOperationAction(ISD::FABS, VT, LegalizeAction::Legal);
    setOperationAction(ISD::FMAD, VT, LegalizeAction::Legal);
  }
  setOperationAction(ISD::FDIV, MVT::f32, LegalizeAction::Legal);

  //  // Truncating/extending stores/loads are also not supported.
  //  for (MVT VT : MVT::integer_fixedlen_vector_valuetypes()) {
  //    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::v2i32, Expand);
  //    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::v2i32, Expand);
  //    setLoadExtAction(ISD::EXTLOAD, VT, MVT::v2i32, Expand);

  //    setLoadExtAction(ISD::SEXTLOAD, MVT::v2i32, VT, Expand);
  //    setLoadExtAction(ISD::ZEXTLOAD, MVT::v2i32, VT, Expand);
  //    setLoadExtAction(ISD::EXTLOAD, MVT::v2i32, VT, Expand);

  //    setTruncStoreAction(VT, MVT::v2i32, Expand);
  //    setTruncStoreAction(MVT::v2i32, VT, Expand);
  //  }
  //  // However, load and store *are* legal.
  //  setOperationAction(ISD::LOAD, MVT::v2i32, Legal);
  //  setOperationAction(ISD::STORE, MVT::v2i32, Legal);
  //  setOperationAction(ISD::EXTRACT_VECTOR_ELT, MVT::v2i32, Legal);
  //  setOperationAction(ISD::BUILD_VECTOR, MVT::v2i32, Legal);

  //  // And we need to promote i64 loads/stores into vector load/store
  //  setOperationAction(ISD::LOAD, MVT::i64, Custom);
  //  setOperationAction(ISD::STORE, MVT::i64, Custom);

  //  // Sadly, this doesn't work:
  //  //    AddPromotedToType(ISD::LOAD, MVT::i64, MVT::v2i32);
  //  //    AddPromotedToType(ISD::STORE, MVT::i64, MVT::v2i32);
  //}

  // Turn FP extload into load/fpextend
  //for (MVT VT : MVT::fp_valuetypes()) {
  //  setLoadExtAction(ISD::EXTLOAD, VT, MVT::f16, Expand);
  //  setLoadExtAction(ISD::EXTLOAD, VT, MVT::f32, Expand);
  //  setLoadExtAction(ISD::EXTLOAD, VT, MVT::f64, Expand);
  //}

  //// PS2VPU doesn't have i1 sign extending load
    for (MVT VT : MVT::integer_valuetypes()) {
      for (MVT VT2 : MVT::integer_valuetypes())
        setLoadExtAction(ISD::SEXTLOAD, VT, VT2, Expand);
    }

  //// Turn FP truncstore into trunc + store.
  //setTruncStoreAction(MVT::f32, MVT::f16, Expand);
  //setTruncStoreAction(MVT::f64, MVT::f16, Expand);
  //setTruncStoreAction(MVT::f64, MVT::f32, Expand);
  //setTruncStoreAction(MVT::f128, MVT::f16, Expand);
  //setTruncStoreAction(MVT::f128, MVT::f32, Expand);
  //setTruncStoreAction(MVT::f128, MVT::f64, Expand);

  //// Custom legalize GlobalAddress nodes into LO/HI parts.
  //setOperationAction(ISD::GlobalAddress, PtrVT, Custom);
  //setOperationAction(ISD::GlobalTLSAddress, PtrVT, Custom);
  setOperationAction(ISD::ConstantPool, PtrVT, Custom);
  //setOperationAction(ISD::BlockAddress, PtrVT, Custom);

  //// PS2VPU doesn't have sext_inreg, replace them with shl/sra
  //setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  //setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand);
  //setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);
    for (MVT VT : MVT::integer_valuetypes()) {
      setOperationAction(ISD::SRA, VT, Custom);
      setOperationAction(ISD::SIGN_EXTEND_INREG, VT, Expand);
      setOperationAction(ISD::SIGN_EXTEND, VT, Legal);
    }

  //// PS2VPU has no select or setcc: expand to SELECT_CC.
  setOperationAction(ISD::SELECT, MVT::i16, Expand);
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::f32, Expand);
  setOperationAction(ISD::SELECT, MVT::f64, Expand);
  setOperationAction(ISD::SELECT, MVT::f128, Expand);

  setOperationAction(ISD::SETCC, MVT::i16, Legal);
  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SETCC, MVT::f32, Expand);
  setOperationAction(ISD::SETCC, MVT::f64, Expand);
  setOperationAction(ISD::SETCC, MVT::f128, Expand);

  //// PS2VPU doesn't have BRCOND either, it has BR_CC.
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BRIND, MVT::Other, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);

  setMaxAtomicSizeInBitsSupported(0);
  setTargetDAGCombine(ISD::SIGN_EXTEND);

  setMinFunctionAlignment(Align(8));

  computeRegisterProperties(Subtarget->getRegisterInfo());
}
//
//bool PS2VPUTargetLowering::useSoftFloat() const {
//  return Subtarget->useSoftFloat();
//}
//
const char *PS2VPUTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((PS2VPUISD::NodeType)Opcode) {
  case PS2VPUISD::FIRST_NUMBER:
    break;
  case PS2VPUISD::CMPICC:
    return "PS2VPUISD::CMPICC";
  case PS2VPUISD::CMPFCC:
    return "PS2VPUISD::CMPFCC";
  case PS2VPUISD::BRICC:
    return "PS2VPUISD::BRICC";
  case PS2VPUISD::BRXCC:
    return "PS2VPUISD::BRXCC";
  case PS2VPUISD::BRFCC:
    return "PS2VPUISD::BRFCC";
  case PS2VPUISD::SELECT_ICC:
    return "PS2VPUISD::SELECT_ICC";
  case PS2VPUISD::SELECT_XCC:
    return "PS2VPUISD::SELECT_XCC";
  case PS2VPUISD::SELECT_FCC:
    return "PS2VPUISD::SELECT_FCC";
  case PS2VPUISD::Hi:
    return "PS2VPUISD::Hi";
  case PS2VPUISD::Lo:
    return "PS2VPUISD::Lo";
  case PS2VPUISD::FTOI:
    return "PS2VPUISD::FTOI";
  case PS2VPUISD::ITOF:
    return "PS2VPUISD::ITOF";
  case PS2VPUISD::FTOX:
    return "PS2VPUISD::FTOX";
  case PS2VPUISD::XTOF:
    return "PS2VPUISD::XTOF";
  case PS2VPUISD::CALL:
    return "PS2VPUISD::CALL";
  case PS2VPUISD::RET_FLAG:
    return "PS2VPUISD::RET_FLAG";
  case PS2VPUISD::GLOBAL_BASE_REG:
    return "PS2VPUISD::GLOBAL_BASE_REG";
  case PS2VPUISD::FLUSHW:
    return "PS2VPUISD::FLUSHW";
  case PS2VPUISD::TLS_ADD:
    return "PS2VPUISD::TLS_ADD";
  case PS2VPUISD::TLS_LD:
    return "PS2VPUISD::TLS_LD";
  case PS2VPUISD::TLS_CALL:
    return "PS2VPUISD::TLS_CALL";
  case PS2VPUISD::TAIL_CALL:
    return "PS2VPUISD::TAIL_CALL";
  case PS2VPUISD::LOAD_GDOP:
    return "PS2VPUISD::LOAD_GDOP";
  }
  return nullptr;
}

EVT PS2VPUTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                            EVT VT) const {
  if (!VT.isVector())
    return MVT::i16;
  return VT.changeVectorElementTypeToInteger();
}
//
///// isMaskedValueZeroForTargetNode - Return true if 'Op & Mask' is known to
///// be zero. Op is expected to be a target specific node. Used by DAG
///// combiner.
void PS2VPUTargetLowering::computeKnownBitsForTargetNode(
    const SDValue Op, KnownBits &Known, const APInt &DemandedElts,
    const SelectionDAG &DAG, unsigned Depth) const {
  KnownBits Known2;
  Known.resetAll();

  switch (Op.getOpcode()) {
  default:
    break;
  case PS2VPUISD::SELECT_ICC:
  case PS2VPUISD::SELECT_XCC:
  case PS2VPUISD::SELECT_FCC:
    Known = DAG.computeKnownBits(Op.getOperand(1), Depth + 1);
    Known2 = DAG.computeKnownBits(Op.getOperand(0), Depth + 1);

    // Only known if known in both the LHS and RHS.
    Known = KnownBits::commonBits(Known, Known2);
    break;
  }
}

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG,
                          const PS2VPUTargetLowering &TLI) {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);
  unsigned Opc, SPCC = ~0U;

  switch (CC) {
  case ISD::CondCode::SETLT:
  case ISD::CondCode::SETLE:
  case ISD::CondCode::SETGT:
  case ISD::CondCode::SETGE: {
      // we don't lower for SETEQ or SETNE (ideally not when RHS is 0 either but whatever)
    return DAG.getNode(PS2VPUISD::BRICC, dl, MVT::Other, Chain,
                       DAG.getConstant(IntCondCCodeToICC(CC), dl, MVT::i16),
                       LHS, RHS, Dest);
  }
  default:
    break;
  }
  return Op;
}

SDValue PS2VPUTargetLowering::LowerOperation(SDValue Op,
                                            SelectionDAG &DAG) const {

  //bool hasHardQuad = Subtarget->hasHardQuad();
  //bool isV9 = Subtarget->isV9();

    auto qzdqd = Op.getOpcode();
  switch (Op.getOpcode()) {
  default:
    llvm_unreachable("Should not custom lower this!");
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG, *this);
  /*case ISD::RETURNADDR:
    return LowerRETURNADDR(Op, DAG, *this, Subtarget);
  case ISD::FRAMEADDR:
    return LowerFRAMEADDR(Op, DAG, Subtarget);
  case ISD::GlobalTLSAddress:
    return LowerGlobalTLSAddress(Op, DAG);
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:
    return LowerBlockAddress(Op, DAG);
  case ISD::ConstantPool:
    return LowerConstantPool(Op, DAG);
  case ISD::FP_TO_SINT:
    return LowerFP_TO_SINT(Op, DAG, *this, hasHardQuad);
  case ISD::SINT_TO_FP:
    return LowerSINT_TO_FP(Op, DAG, *this, hasHardQuad);
  case ISD::FP_TO_UINT:
    return LowerFP_TO_UINT(Op, DAG, *this, hasHardQuad);
  case ISD::UINT_TO_FP:
    return LowerUINT_TO_FP(Op, DAG, *this, hasHardQuad);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG, *this, hasHardQuad);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG, *this, hasHardQuad);
  case ISD::VASTART:
    return LowerVASTART(Op, DAG, *this);
  case ISD::VAARG:
    return LowerVAARG(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC:
    return LowerDYNAMIC_STACKALLOC(Op, DAG, Subtarget);

  case ISD::LOAD:
    return LowerLOAD(Op, DAG);
  case ISD::STORE:
    return LowerSTORE(Op, DAG);
  case ISD::FADD:
    return LowerF128Op(Op, DAG, getLibcallName(RTLIB::ADD_F128), 2);
  case ISD::FSUB:
    return LowerF128Op(Op, DAG, getLibcallName(RTLIB::SUB_F128), 2);
  case ISD::FMUL:
    return LowerF128Op(Op, DAG, getLibcallName(RTLIB::MUL_F128), 2);
  case ISD::FDIV:
    return LowerF128Op(Op, DAG, getLibcallName(RTLIB::DIV_F128), 2);
  case ISD::FSQRT:
    return LowerF128Op(Op, DAG, getLibcallName(RTLIB::SQRT_F128), 1);
  case ISD::FABS:
  case ISD::FNEG:
    return LowerFNEGorFABS(Op, DAG, isV9);
  case ISD::FP_EXTEND:
    return LowerF128_FPEXTEND(Op, DAG, *this);
  case ISD::FP_ROUND:
    return LowerF128_FPROUND(Op, DAG, *this);
  case ISD::ADDC:
  case ISD::ADDE:
  case ISD::SUBC:
  case ISD::SUBE:
    return LowerADDC_ADDE_SUBC_SUBE(Op, DAG);
  case ISD::UMULO:
  case ISD::SMULO:
    return LowerUMULO_SMULO(Op, DAG, *this);
  case ISD::ATOMIC_LOAD:
  case ISD::ATOMIC_STORE:
    return LowerATOMIC_LOAD_STORE(Op, DAG);
  case ISD::INTRINSIC_WO_CHAIN:
    return LowerINTRINSIC_WO_CHAIN(Op, DAG);*/
  }
}
//
SDValue PS2VPUTargetLowering::bitcastConstantFPToInt(ConstantFPSDNode *C,
                                                    const SDLoc &DL,
                                                    SelectionDAG &DAG) const {
  APInt V = C->getValueAPF().bitcastToAPInt();
  SDValue Lo = DAG.getConstant(V.zextOrTrunc(32), DL, MVT::i32);
  SDValue Hi = DAG.getConstant(V.lshr(32).zextOrTrunc(32), DL, MVT::i32);
  if (DAG.getDataLayout().isLittleEndian())
    std::swap(Lo, Hi);
  return DAG.getBuildVector(MVT::v2i32, DL, {Hi, Lo});
}
//
//SDValue PS2VPUTargetLowering::PerformBITCASTCombine(SDNode *N,
//                                                   DAGCombinerInfo &DCI) const {
//  SDLoc dl(N);
//  SDValue Src = N->getOperand(0);
//
//  if (isa<ConstantFPSDNode>(Src) && N->getSimpleValueType(0) == MVT::v2i32 &&
//      Src.getSimpleValueType() == MVT::f64)
//    return bitcastConstantFPToInt(cast<ConstantFPSDNode>(Src), dl, DCI.DAG);
//
//  return SDValue();
//}
//
SDValue PS2VPUTargetLowering::PerformDAGCombine(SDNode *N,
                                               DAGCombinerInfo &DCI) const {
  switch (N->getOpcode()) {
  default:
    break;
  /*case ISD::BITCAST:
    return PerformBITCASTCombine(N, DCI);*/
  }
  return SDValue();
}

PS2VPUNS::CondCodes invertCC(PS2VPUNS::CondCodes cc) {
  switch (cc) {
  case PS2VPUNS::CondCodes::ICC_G:
    return PS2VPUNS::CondCodes::ICC_L;
  case PS2VPUNS::CondCodes::ICC_GE:
    return PS2VPUNS::CondCodes::ICC_LE;
  case PS2VPUNS::CondCodes::ICC_L:
    return PS2VPUNS::CondCodes::ICC_G;
  case PS2VPUNS::CondCodes::ICC_LE:
    return PS2VPUNS::CondCodes::ICC_GE;
  default:
    return cc;
  }
}
 MachineBasicBlock *
 PS2VPUTargetLowering::expandBrCCPseudo(MachineInstr &MI, MachineBasicBlock *BB) const {
   const TargetInstrInfo &TII = *Subtarget->getInstrInfo();
   DebugLoc dl = MI.getDebugLoc();
   PS2VPUNS::CondCodes CC = (PS2VPUNS::CondCodes)MI.getOperand(0).getImm();

   auto LHS = MI.getOperand(1);
   auto RHS = MI.getOperand(2);

   auto TargetBB = MI.getOperand(3).getMBB();

   if (LHS.isImm() && RHS.isReg())
   {
    std::swap(LHS, RHS);
    CC = invertCC(CC);
   }


   unsigned opcode = 0;
   switch (CC) {
   case PS2VPUNS::CondCodes::ICC_E:
    opcode = PS2VPUNS::IBEQ;
    break;
   case PS2VPUNS::CondCodes::ICC_G:
    opcode = PS2VPUNS::IBGTZ;
    break;
   case PS2VPUNS::CondCodes::ICC_GE:
    opcode = PS2VPUNS::IBGEZ;
    break;
   case PS2VPUNS::CondCodes::ICC_L:
    opcode = PS2VPUNS::IBLTZ;
    break;
   case PS2VPUNS::CondCodes::ICC_LE:
    opcode = PS2VPUNS::IBLEZ;
    break;
   case PS2VPUNS::CondCodes::ICC_NE:
    opcode = PS2VPUNS::IBNE;
    break;
   default:
    llvm_unreachable("unknown condition code");
   }


   MachineBasicBlock *NewMBB = BB;

   if (LHS.isReg() && RHS.isReg()) {
    //  // We produce the following control flow:
    //  //                ThisMBB
    //  //           /               \
    //  //     ALessMBB              IfFalseMBB
    //  //      /     \               /      \
    //  //  TrueMBB   SubMBB    FalseMBB    SubMBB
    const BasicBlock *LLVM_BB = BB->getBasicBlock();
    MachineFunction::iterator It = ++BB->getIterator();
    MachineBasicBlock *ThisMBB = BB;
    MachineFunction *F = BB->getParent();

    MachineBasicBlock *TrueMBB = TargetBB;
    MachineBasicBlock *ALessMBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *FalseMBB = F->CreateMachineBasicBlock(LLVM_BB);
    MachineBasicBlock *SubMBB = F->CreateMachineBasicBlock(LLVM_BB);
    F->insert(It, ALessMBB);
    F->insert(It, FalseMBB);
    F->insert(It, SubMBB);

    NewMBB = SubMBB;

    bool isLesser =
        (CC == PS2VPUNS::CondCodes::ICC_L || CC == PS2VPUNS::CondCodes::ICC_LE);

    // Transfer the remainder of ThisMBB and its successor edges to SinkMBB.
    FalseMBB->splice(FalseMBB->begin(), ThisMBB,
                    std::next(MachineBasicBlock::iterator(MI)), ThisMBB->end());
    FalseMBB->transferSuccessorsAndUpdatePHIs(ThisMBB);


    BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBLTZ))
        .addReg(LHS.getReg())
        .addMBB(ALessMBB);
    ThisMBB->addSuccessor(ALessMBB);

    // A Less
    BuildMI(ALessMBB, dl, TII.get(PS2VPUNS::IBGEZ))
        .addReg(RHS.getReg())
        .addMBB(isLesser ? TrueMBB : FalseMBB);
    BuildMI(ALessMBB, dl, TII.get(PS2VPUNS::Bi)).addMBB(SubMBB);
    ALessMBB->addSuccessor(isLesser ? TrueMBB : FalseMBB);
    ALessMBB->addSuccessor(SubMBB);

    // A GreaterEq
    BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBLTZ))
        .addReg(RHS.getReg())
        .addMBB(isLesser ? FalseMBB : TrueMBB);
    ThisMBB->addSuccessor(isLesser ? FalseMBB : TrueMBB);
    BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::Bi)).addMBB(SubMBB);
    ThisMBB->addSuccessor(SubMBB);

    // Sub
    llvm::Register dest =
        F->getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
    BuildMI(SubMBB, dl, TII.get(PS2VPUNS::ISUBrr), dest)
        .addReg(LHS.getReg())
        .addReg(RHS.getReg());
    BuildMI(SubMBB, dl, TII.get(opcode)).addReg(dest).addMBB(TrueMBB);
    BuildMI(SubMBB, dl, TII.get(PS2VPUNS::Bi)).addMBB(FalseMBB);
    SubMBB->addSuccessor(TrueMBB);
    SubMBB->addSuccessor(FalseMBB);

   } else if (LHS.isReg() && RHS.isImm()) {
    int64_t imm = RHS.getImm();
    if (imm == 0) {
        BuildMI(BB, dl, TII.get(opcode)).addReg(LHS.getReg()).addMBB(TargetBB);
    } else if (imm > 0) {
        const BasicBlock *LLVM_BB = BB->getBasicBlock();
        MachineFunction::iterator It = ++BB->getIterator();
        MachineBasicBlock *ThisMBB = BB;
        MachineFunction *F = BB->getParent();

        switch (CC) {
        case PS2VPUNS::CondCodes::ICC_LE:
        case PS2VPUNS::CondCodes::ICC_L: {
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBLTZ))
            .addReg(LHS.getReg())
            .addMBB(TargetBB);
        llvm::Register dest =
            F->getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::ISUBUri), dest)
            .addReg(LHS.getReg())
            .addImm(imm);
        BuildMI(ThisMBB, dl, TII.get(opcode))
            .addReg(dest)
            .addMBB(TargetBB);
        }
        break;
        case PS2VPUNS::CondCodes::ICC_GE:
        case PS2VPUNS::CondCodes::ICC_G: {
        MachineBasicBlock *SinkMBB = F->CreateMachineBasicBlock(LLVM_BB);
        F->insert(It, SinkMBB);
        SinkMBB->splice(SinkMBB->begin(), ThisMBB,
                        std::next(MachineBasicBlock::iterator(MI)),
                        ThisMBB->end());
        SinkMBB->transferSuccessorsAndUpdatePHIs(ThisMBB);
        ThisMBB->addSuccessor(SinkMBB);
        ThisMBB->addSuccessor(TargetBB);

        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBLTZ))
            .addReg(LHS.getReg())
            .addMBB(SinkMBB);
        llvm::Register dest =
            F->getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::ISUBUri), dest)
            .addReg(LHS.getReg())
            .addImm(imm);
        BuildMI(ThisMBB, dl, TII.get(opcode))
            .addReg(dest)
            .addMBB(TargetBB);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::Bi))
            .addMBB(SinkMBB);

        NewMBB = SinkMBB;
        /*LLVM_DEBUG(dbgs() << "*** My function lala ***\n");
        LLVM_DEBUG(F->print(dbgs()));*/
        } break;
        default:
        llvm_unreachable("unexpected condition code");
        }
    } else if (imm < 0) {
        const BasicBlock *LLVM_BB = BB->getBasicBlock();
        MachineFunction::iterator It = ++BB->getIterator();
        MachineBasicBlock *ThisMBB = BB;
        MachineFunction *F = BB->getParent();

        switch (CC) {
        case PS2VPUNS::CondCodes::ICC_LE:
        case PS2VPUNS::CondCodes::ICC_L: {
        MachineBasicBlock *SinkMBB = F->CreateMachineBasicBlock(LLVM_BB);
        F->insert(It, SinkMBB);
        SinkMBB->splice(SinkMBB->begin(), ThisMBB,
                        std::next(MachineBasicBlock::iterator(MI)),
                        ThisMBB->end());
        SinkMBB->transferSuccessorsAndUpdatePHIs(ThisMBB);
        ThisMBB->addSuccessor(SinkMBB);
        ThisMBB->addSuccessor(TargetBB);

        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBGTZ))
            .addReg(LHS.getReg())
            .addMBB(SinkMBB);
        llvm::Register dest =
            F->getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::ISUBUri), dest)
            .addReg(LHS.getReg())
            .addImm(imm);
        BuildMI(ThisMBB, dl, TII.get(opcode)).addReg(dest).addMBB(TargetBB);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::Bi)).addMBB(SinkMBB);

        NewMBB = SinkMBB;
        } break;
        case PS2VPUNS::CondCodes::ICC_GE:
        case PS2VPUNS::CondCodes::ICC_G: {
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::IBGTZ))
            .addReg(LHS.getReg())
            .addMBB(TargetBB);
        llvm::Register dest =
            F->getRegInfo().createVirtualRegister(&PS2VPUNS::IntRegsRegClass);
        BuildMI(ThisMBB, dl, TII.get(PS2VPUNS::ISUBUri), dest)
            .addReg(LHS.getReg())
            .addImm(imm);
        BuildMI(ThisMBB, dl, TII.get(opcode)).addReg(dest).addMBB(TargetBB);
        
        } break;
        default:
        llvm_unreachable("unexpected condition code");
        }
    }
   }

   MI.eraseFromParent(); // The pseudo instruction is gone now.
   return NewMBB;
 }

 MachineBasicBlock *
PS2VPUTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unknown Instr!");
  case PS2VPUNS::BR_CC_PSEUDOrr:
  case PS2VPUNS::BR_CC_PSEUDOri:
    return expandBrCCPseudo(MI, BB);
  }
}

bool PS2VPUTargetLowering::isOffsetFoldingLegal(
    const GlobalAddressSDNode *GA) const {
  // The PS2VPU target isn't yet aware of offsets.
  return false;
}
//
void PS2VPUTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue> &Results,
                                             SelectionDAG &DAG) const {

  SDLoc dl(N);

  RTLIB::Libcall libCall = RTLIB::UNKNOWN_LIBCALL;

  switch (N->getOpcode()) {
  default:
    llvm_unreachable("Do not know how to custom type legalize this operation!");

  //case ISD::FP_TO_SINT:
  //case ISD::FP_TO_UINT:
  //  // Custom lower only if it involves f128 or i64.
  //  if (N->getOperand(0).getValueType() != MVT::f128 ||
  //      N->getValueType(0) != MVT::i64)
  //    return;
  //  libCall = ((N->getOpcode() == ISD::FP_TO_SINT) ? RTLIB::FPTOSINT_F128_I64
  //                                                 : RTLIB::FPTOUINT_F128_I64);

  //  Results.push_back(
  //      LowerF128Op(SDValue(N, 0), DAG, getLibcallName(libCall), 1));
  //  return;
  //case ISD::READCYCLECOUNTER: {
  //  assert(Subtarget->hasLeonCycleCounter());
  //  SDValue Lo = DAG.getCopyFromReg(N->getOperand(0), dl, SP::ASR23, MVT::i32);
  //  SDValue Hi = DAG.getCopyFromReg(Lo, dl, SP::G0, MVT::i32);
  //  SDValue Ops[] = {Lo, Hi};
  //  SDValue Pair = DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, Ops);
  //  Results.push_back(Pair);
  //  Results.push_back(N->getOperand(0));
  //  return;
  //}
  //case ISD::SINT_TO_FP:
  //case ISD::UINT_TO_FP:
  //  // Custom lower only if it involves f128 or i64.
  //  if (N->getValueType(0) != MVT::f128 ||
  //      N->getOperand(0).getValueType() != MVT::i64)
  //    return;

  //  libCall = ((N->getOpcode() == ISD::SINT_TO_FP) ? RTLIB::SINTTOFP_I64_F128
  //                                                 : RTLIB::UINTTOFP_I64_F128);

  //  Results.push_back(
  //      LowerF128Op(SDValue(N, 0), DAG, getLibcallName(libCall), 1));
  //  return;
  //case ISD::LOAD: {
  //  LoadSDNode *Ld = cast<LoadSDNode>(N);
  //  // Custom handling only for i64: turn i64 load into a v2i32 load,
  //  // and a bitcast.
  //  if (Ld->getValueType(0) != MVT::i64 || Ld->getMemoryVT() != MVT::i64)
  //    return;

  //  SDLoc dl(N);
  //  SDValue LoadRes =
  //      DAG.getExtLoad(Ld->getExtensionType(), dl, MVT::v2i32, Ld->getChain(),
  //                     Ld->getBasePtr(), Ld->getPointerInfo(), MVT::v2i32,
  //                     Ld->getOriginalAlign(), Ld->getMemOperand()->getFlags(),
  //                     Ld->getAAInfo());

  //  SDValue Res = DAG.getNode(ISD::BITCAST, dl, MVT::i64, LoadRes);
  //  Results.push_back(Res);
  //  Results.push_back(LoadRes.getValue(1));
  //  return;
  //}
  }
}

//// Override to enable LOAD_STACK_GUARD lowering on Linux.
//bool PS2VPUTargetLowering::useLoadStackGuardNode() const {
//  if (!Subtarget->isTargetLinux())
//    return TargetLowering::useLoadStackGuardNode();
//  return true;
//}
//
//// Override to disable global variable loading on Linux.
//void PS2VPUTargetLowering::insertSSPDeclarations(Module &M) const {
//  if (!Subtarget->isTargetLinux())
//    return TargetLowering::insertSSPDeclarations(M);
//}
