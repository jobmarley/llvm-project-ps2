add_llvm_component_library(LLVMPS2VPUDesc
  PS2VPUAsmBackend.cpp
  PS2VPUELFObjectWriter.cpp
  PS2VPUInstPrinter.cpp
  PS2VPUMCAsmInfo.cpp
  PS2VPUMCCodeEmitter.cpp
  PS2VPUMCTargetDesc.cpp
  PS2VPUMCExpr.cpp
  PS2VPUTargetStreamer.cpp
  PS2VPUMCInstrInfo.cpp

  LINK_COMPONENTS
  MC
  PS2VPUInfo
  Support

  ADD_TO_COMPONENT
  PS2VPU
  )
