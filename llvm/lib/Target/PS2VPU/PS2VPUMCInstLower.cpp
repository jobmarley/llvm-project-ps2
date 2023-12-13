//===-- PS2VPUMCInstLower.cpp - Convert PS2VPU MachineInstr to MCInst -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower PS2VPU MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PS2VPUMCExpr.h"
#include "PS2VPU.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

static MCOperand LowerSymbolOperand(const MachineInstr *MI,
                                    const MachineOperand &MO, AsmPrinter &AP) {

  PS2VPUMCExpr::VariantKind Kind = (PS2VPUMCExpr::VariantKind)MO.getTargetFlags();
  const MCSymbol *Symbol = nullptr;

  switch (MO.getType()) {
  default:
    llvm_unreachable("Unknown type in LowerSymbolOperand");
  case MachineOperand::MO_MachineBasicBlock:
    Symbol = MO.getMBB()->getSymbol();
    break;

  case MachineOperand::MO_GlobalAddress:
    Symbol = AP.getSymbol(MO.getGlobal());
    break;

  case MachineOperand::MO_BlockAddress:
    Symbol = AP.GetBlockAddressSymbol(MO.getBlockAddress());
    break;

  case MachineOperand::MO_ExternalSymbol:
    Symbol = AP.GetExternalSymbolSymbol(MO.getSymbolName());
    break;

  case MachineOperand::MO_ConstantPoolIndex:
    Symbol = AP.GetCPISymbol(MO.getIndex());
    break;
  }

  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::create(Symbol, AP.OutContext);
  const PS2VPUMCExpr *expr = PS2VPUMCExpr::create(Kind, MCSym, AP.OutContext);
  return MCOperand::createExpr(expr);
}

static MCOperand LowerOperand(const MachineInstr *MI, const MachineOperand &MO,
                              AsmPrinter &AP) {
  switch (MO.getType()) {
  default:
    llvm_unreachable("unknown operand type");
    break;
  case MachineOperand::MO_Register:
    if (MO.isImplicit())
      break;
    return MCOperand::createReg(MO.getReg());

  case MachineOperand::MO_FPImmediate:
    return MCOperand::createSFPImm(MO.getFPImm()->getValue().bitcastToAPInt().getZExtValue());
  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm());

  case MachineOperand::MO_MachineBasicBlock:
  case MachineOperand::MO_GlobalAddress:
  case MachineOperand::MO_BlockAddress:
  case MachineOperand::MO_ExternalSymbol:
  case MachineOperand::MO_ConstantPoolIndex:
    return LowerSymbolOperand(MI, MO, AP);

  case MachineOperand::MO_RegisterMask:
    break;
  }
  return MCOperand();
}

void llvm::LowerPS2VPUMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                          AsmPrinter &AP) {

  OutMI.setOpcode(MI->getOpcode());

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp = LowerOperand(MI, MO, AP);

    if (MCOp.isValid())
      OutMI.addOperand(MCOp);
  }
}
