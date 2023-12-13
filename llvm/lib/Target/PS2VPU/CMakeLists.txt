add_llvm_component_group(PS2VPU)

set(LLVM_TARGET_DEFINITIONS PS2VPU.td)

tablegen(LLVM PS2VPUGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM PS2VPUGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM PS2VPUGenCallingConv.inc -gen-callingconv)
tablegen(LLVM PS2VPUGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM PS2VPUGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM PS2VPUGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM PS2VPUGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM PS2VPUGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM PS2VPUGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(PS2VPUCommonTableGen)

add_llvm_target(PS2VPUCodeGen
  PS2VPUAsmPrinter.cpp
  PS2VPUInstrInfo.cpp
  PS2VPUISelDAGToDAG.cpp 
  PS2VPUISelLowering.cpp
  PS2VPUFrameLowering.cpp
  PS2VPUMachineFunctionInfo.cpp
  PS2VPURegisterInfo.cpp
  PS2VPUSubtarget.cpp
  PS2VPUTargetMachine.cpp
  PS2VPUTargetTransformInfo.cpp

  PS2VPUMCInstLower.cpp
  PS2VPUTargetObjectFile.cpp
  
  LINK_COMPONENTS
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  PS2VPUDesc
  PS2VPUInfo
  Support
  Target

  ADD_TO_COMPONENT
  PS2VPU
  )

add_subdirectory(AsmParser)
#add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
