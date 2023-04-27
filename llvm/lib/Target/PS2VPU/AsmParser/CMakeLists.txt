add_llvm_component_library(LLVMPS2VPUAsmParser
  PS2VPUAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  PS2VPUDesc
  PS2VPUInfo
  Support

  ADD_TO_COMPONENT
  PS2VPU
  )
