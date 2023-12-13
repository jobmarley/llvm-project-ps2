
def CC_PS2VPU : CallingConv<[
  // i32 f32 arguments get passed in integer registers if there is space.
  CCIfType<[i16], CCAssignToReg<[VI1, VI2, VI3, VI4, VI5]>>,
  CCIfType<[v4f32], CCAssignToReg<[VF0, VF1, VF2, VF3, VF4, VF5]>>,

  // Alternatively, they are assigned to the stack in 4-byte aligned units.
  CCAssignToStack<4, 4>
]>;

def RetCC_PS2VPU : CallingConv<[
  CCIfType<[i16], CCAssignToReg<[VI1, VI2, VI3, VI4, VI5]>>,
  CCIfType<[f32, v2f32], CCPromoteToType<v4f32>>,
  CCIfType<[v4f32], CCAssignToReg<[VF0, VF1, VF2, VF3, VF4, VF5]>>,
  //CCIfType<[v2i32], CCCustom<"CC_Sparc_Assign_Ret_Split_64">>
]>;

// Callee-saved registers are handled by the register window mechanism.
def CSR : CalleeSavedRegs<(add)> {
  let OtherPreserved = (add (sequence "VI%u", 0, 7));
}

// Callee-saved registers for calls with ReturnsTwice attribute.
def RTCSR : CalleeSavedRegs<(add)> {
  let OtherPreserved = (add VI6, VI7);
}
