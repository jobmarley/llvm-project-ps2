# PS2 Vector Unit LLVM Target 
I thought it'd be fun to make an FPGA implementation of the PS2 VU, but then realized I needed compiler support, so here we go.
As far as I know there was no compiler support for it, neither in the official SDK nor in the homebrew one. 
So the goal was to make a llvm target to have modern c++ support and use it use it to do some 2d/3d calculation.  
I focused on micro mode, so macro is just not supported.

## VU Architecture
The VU uses a 64bits VLIW architecture with 1 upper and 1 lower instruction.
The upper instruction is used for SIMD floating point vector operations. The lower for int16, branches, memory operations, random numbers and math functions like exponential, arctan, etc...
Here are the basic caracteristics of the VU:
- Vector floating point registers VF0 to VF31
- Int16 registers VI0 to VI15
- Additionnal registers ACC(v4f32), I(f32), P(f32), Q(f32), R(i23)
- Not fully IEEE 754 compatible but close enough

The VU can work in micro mode, completely asynchronously from the main CPU, or as a MIPS coprocessor (macro mode), where the instructions are issued by the main CPU.
The PS2 has 2 VU.
VU0:
- micro or macro mode
- 4KB data & 4KB instruction ram
VU1:
- micro only
- 16KB data & 16KB instruction ram

## Features
- Integer computation : **supported** (int and shorts are defined as 16 bits with 32 bits alignment)
- Stack handling : **partially supported**
- Pointers : **not supported** (need a custom pass to handle GetElementPtr lowering)
- If/else/loops/branches/return : **supported**
- Floating point calculation : **partially supported** (not all operations)
- Vectorization : **supported** (non tuned, but the generated llvm IR seems decent)
- Asm generation : **supported**
- Binary generation with debug info : **not supported**

As of now it's still too early, and there is a ton of bugs everywhere. Simple functions can be compiled for testing but that's it.

## Issues & difficulties
### Pointers & memory operations
Sony decided to make the pointers 16 bytes aligned (discarding the lower bits), and all memory operations aligned on 16bytes as well.
The result is all memory instructions are vectorized. There is a component field in the instructions to select the component in memory.
```
LQ.xz VF1xz, 4(VI1)  ; loads MEM[(VI1+4)*16] into VF1x, and MEM[(VI1+4)*16 + 8] into VF1z
ILW.w VI2, 4(VI1)  ; loads MEM[(VI1+4)*16 + 12] into VI1
```
For integer load/store the operation is undefined is more than 1 component is selected.
As a consequence, in c++, most pointer are not representable (any type not aligned on 16 bytes, char* or wchar_t* for example).
Even if it was, loop vectorization would be mandatory (imagine you iterate over a int16 array).
  
For stack operations, instructions are selected postRA, after stack lowering, which means it might no be the most optimized, and cannot be used to guide the selection of other instructions.

### Integers
The VU only has int16 support, with very limited instructions (add/sub/and/or).  
On top of that, because of the vectorized memory operations, int16 have to be aligned on 32bits.  
That means the upper 16 bits cannot be accessed and are effectively wasted.  
Since there is no support for shift or mul, this cause an issue for memory operations as well.

Let's take the simple example of a vertex/index buffer. You have an array of vertex (v4f32 position + v4f32 color), and an array of integers (lets ignore the vectorization issue).  
When indexing into the vertex array, you need to compute `Index*64/16`, which is not possible because we have neither shift or multiplications.  
There is floating point to fixed point convertion available though, so such a computation could look like this.
```
MFIR.x VF1x, VI1          ; move index to fp reg
MFIR.y VF1y, VI2          ; move structure size to fp reg
ITOF0.xy VF1xy, VF1xy     ; convert both fixed point with 0 fractional part (integer) to floating point
MULy.x VF1x, VF1x, VF1y   ; x = x*y
FTOI0.x VF1x, VF1x        ; convert back to fixed point 0
MTIR VI1, VF1x
```
With a latency of 18.

### Immediate constants
There are several ways to load immediates.
- VF0 is a constant register (0, 0, 0, 1) with broadcast instructions (0 latency)
- I register + I instructions, takes 1 lower instruction in a previous slot (1 cycle)
- Save constant vector in data memory, and load it with LQ (4 cycles latency)
But the situation becomes a bit complicated when you try to optimize it, as 1 variation might be better than an other one depending on register pressure or scheduling.

### Fused multiply add
The FMA instruction uses the special ACC register, as it does `VFA = VFB * VFC + ACC`.
Meaning you need to change a previous operation to a ACC variant. And it's a struggle to do multiple FMA operations at the same time because of it.
