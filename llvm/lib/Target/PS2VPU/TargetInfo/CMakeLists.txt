add_llvm_component_library(LLVMPS2VPUInfo
  PS2VPUTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  PS2VPU
  )
