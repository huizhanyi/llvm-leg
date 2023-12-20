~/build-llvm-leg/bin/clang -target leg-unknown-unknown -O1 -emit-llvm -S ex2.c
~/build-llvm-leg/bin/llc -march leg -debug-only=isel ex2.ll
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll  -relocation-model=pic -filetype=asm -o -
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll

## Calling convention lowering
LEGCallingConv.td生成函数，用于ISelLowering.
```
这里会生成C++函数RetCC_LEG供LEGISelLowering调用
//===----------------------------------------------------------------------===//
// LEG Return Value Calling Convention
//===----------------------------------------------------------------------===//
def RetCC_LEG : CallingConv<[
  // i32 are returned in registers R0, R1, R2, R3
  CCIfType<[i32], CCAssignToReg<[R0, R1, R2, R3]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;
这里会生成C++函数RCC_LEG供LEGISelLowering调用
//===----------------------------------------------------------------------===//
// LEG Argument Calling Conventions
//===----------------------------------------------------------------------===//
def CC_LEG : CallingConv<[
  // Promote i8/i16 arguments to i32.
  CCIfType<[i8, i16], CCPromoteToType<i32>>,

  // The first 4 integer arguments are passed in integer registers.
  CCIfType<[i32], CCAssignToReg<[R0, R1, R2, R3]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;
这里会生成一个数据结构CC_Save_RegMask，LEGRegisterInfo::getCallPreservedMask函数直接使用了这个数据结构。
LEG的getCallPreservedMask和getCalleeSavedRegs定义是一致的，CalleeSavedRegs就是CallPreserved的寄存器。
def CC_Save : CalleeSavedRegs<(add R4, R5, R6, R7, R8, R9)>;
```
这里的CC_Save_RegMask
```
static const uint32_t CC_Save_RegMask[] = { 0x00001f80, };
```
对应到R4-R9，查看前面的枚举定义，之间是一致的。
```
enum {
  NoRegister,
  LR = 1,
  SP = 2,
  R0 = 3,
  R1 = 4,
  R2 = 5,
  R3 = 6,
  R4 = 7,
  R5 = 8,
  R6 = 9,
  R7 = 10,
  R8 = 11,
  R9 = 12,
  NUM_TARGET_REGS       // 13
};
```
主要文件位于LEGISelLowering.h/cpp
### LowerFormalArguments
SelectionDAGISel::runOnMachineFunction -> SelectionDAGISel::SelectAllBasicBlocks (entry block) -> SelectionDAGISel::LowerArguments -> LEGTargetLowering::LowerFormalArguments
```
243 SDValue LEGTargetLowering::LowerFormalArguments(
244     SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
245     const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc dl, SelectionDAG &DAG,
246     SmallVectorImpl<SDValue> &InVals) const {
调用td文件生成的函数，确定参数的传递方式
257   CCInfo.AnalyzeFormalArguments(Ins, CC_LEG);
259   for (auto &VA : ArgLocs) {
如果是寄存器传递的参数
260     if (VA.isRegLoc()) {
生成虚拟寄存器
265       const unsigned VReg = RegInfo.createVirtualRegister(&LEG::GRRegsRegClass);
增加LiveIn信息
266       RegInfo.addLiveIn(VA.getLocReg(), VReg);
增加读寄存器指令
267       SDValue ArgIn = DAG.getCopyFromReg(Chain, dl, VReg, RegVT);
保存生成的SelectionDAG节点
269       InVals.push_back(ArgIn);
对应的这些节点在初始的DAG上面就可以看到，因此这个函数在初始到DAG转化时就被调用过。
```


### 定制SelctionDAG节点
```
 28 namespace LEGISD {
 29 enum NodeType {
 30   // Start the numbering where the builtin ops and target ops leave off.
 31   FIRST_NUMBER = ISD::BUILTIN_OP_END,
 32   RET_FLAG,
 33   // This loads the symbol (e.g. global address) into a register.
 34   LOAD_SYM,
 35   // This loads a 32-bit immediate into a register.
 36   MOVEi32,
 37   CALL,
 38 };
 39 }
```
这里增加了4个Target具体的SelectionDAG节点类型。
增加定制的SelectionDAG方法参考PPT报告的p54。
还需要定义LEGTargetLowering::getTargetNodeName函数返回每个定制节点名字。
另外需要在TableGen中增加节点定义。
```
 31 def SDT_LEGCall    : SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>;
 41 def leg_call
 42     : SDNode<"LEGISD::CALL", SDT_LEGCall,
 43              [ SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic ]>;

```
定制节点能够使用在pattern匹配里。那么SelectionDAG里为什么会有定制节点呢？例如LEGTargetLowering::LowerCall生成了LEGISD::CALL节点。
LEGTargetLowering::LowerCall函数则是在处理函数调用时调用的函数，作为目标定制实现的虚函数，可以用于生成定制节点。
### Custom DAG lowering
对特殊DAG节点做Lowering处理
```
 52 LEGTargetLowering::LEGTargetLowering(LEGTargetMachine &LEGTM)
 53     : TargetLowering(LEGTM, new TargetLoweringObjectFileELF()),
 54       Subtarget(*LEGTM.getSubtargetImpl()) {
```
在TargetLowering类中调用setOperationAction(nodeOpcode, type, Custom)，例如
```
 66   setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
```
这里Custom是LegalizeAction枚举类型，因此对应到合法化阶段。
定义LowerOPCODE函数，例如LEGTargetLowering::LowerGlobalAddress
```
 78 SDValue LEGTargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG& DAG) const
```
调用该函数
```
 69 SDValue LEGTargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
 70   switch (Op.getOpcode()) {
 71   default:
 72     llvm_unreachable("Unimplemented operand");
 73   case ISD::GlobalAddress:
 74     return LowerGlobalAddress(Op, DAG);
 75   }
 76 }
```
LowerOperation被调用的时机包括合法化阶段。
https://www.cnblogs.com/Five100Miles/p/12865995.html
```
 78 SDValue LEGTargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG& DAG) const
 79 {
 80   EVT VT = Op.getValueType();
 81   GlobalAddressSDNode *GlobalAddr = cast<GlobalAddressSDNode>(Op.getNode());
 82   SDValue TargetAddr =
 83       DAG.getTargetGlobalAddress(GlobalAddr->getGlobal(), Op, MVT::i32);
生成定制的DAG节点类型，这种类型还要被后面指令选择节点替换
 84   return DAG.getNode(LEGISD::LOAD_SYM, Op, VT, TargetAddr);
 85 }
```
LOAD_SYM节点
```
def load_sym : SDNode<"LEGISD::LOAD_SYM", SDTIntUnaryOp>;
```
load_sym被用于指令模式拼配
LEGInstrInfo.td
```
191 def : Pattern<(i32 (load_sym tglobaladdr:$addr)),  [(MOVi32 $addr)]>;
```
使用一个例子检查编译过程
```
int global = 1;
int foo(int a, int b) {
    int result = a + b;   // r0 + r1
    result += global;
    return result;        // r0
}
```
LLVM IR表示为
```
@global = global i32 1, align 4

; Function Attrs: nounwind readonly
define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %add = add nsw i32 %b, %a
  %0 = load i32* @global, align 4, !tbaa !1
  %add1 = add nsw i32 %add, %0
  ret i32 %add1
}
```
编译查看
~/build-llvm-leg/bin/llc -march leg -debug-only=isel ex2.ll
初始的SelectionDAG为
```
Initial selection DAG: BB#0 'foo:entry'
SelectionDAG has 14 nodes:
  0x55a9c0811340: ch = EntryToken

  0x55a9c083afb0: i32 = Constant<0>

  0x55a9c083b3d0: i32 = Register %R0

    0x55a9c0811340: <multiple use>
    0x55a9c083b3d0: <multiple use>
          0x55a9c0811340: <multiple use>
          0x55a9c083ab90: i32 = Register %vreg1

        0x55a9c083ac98: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083ab90 [ORD=1]

          0x55a9c0811340: <multiple use>
          0x55a9c083a980: i32 = Register %vreg0

        0x55a9c083aa88: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083a980 [ORD=1]

      0x55a9c083ada0: i32 = add 0x55a9c083ac98, 0x55a9c083aa88 [ORD=2]

        0x55a9c0811340: <multiple use>
        0x55a9c083aea8: i32 = GlobalAddress<i32* @global> 0 [ORD=3]

        0x55a9c083b0b8: i32 = undef

      0x55a9c083b1c0: i32,ch = load 0x55a9c0811340, 0x55a9c083aea8, 0x55a9c083b0b8<LD4[@global](tbaa=<badref>)> [ORD=3]

    0x55a9c083b2c8: i32 = add 0x55a9c083ada0, 0x55a9c083b1c0 [ORD=4]

  0x55a9c083b4d8: ch,glue = CopyToReg 0x55a9c0811340, 0x55a9c083b3d0, 0x55a9c083b2c8 [ORD=5]

    0x55a9c083b4d8: <multiple use>
    0x55a9c083b3d0: <multiple use>
    0x55a9c083b4d8: <multiple use>
  0x55a9c083b5e0: ch = RetFlag 0x55a9c083b4d8, 0x55a9c083b3d0, 0x55a9c083b4d8:1 [ORD=5]
```
其中有GlobalAddress节点，并且类型为i32,Legalized selection DAG之后
```
Legalized selection DAG: BB#0 'foo:entry'
SelectionDAG has 14 nodes:
  0x55a9c0811340: ch = EntryToken [ID=0]

  0x55a9c083b3d0: i32 = Register %R0 [ID=5]

    0x55a9c0811340: <multiple use>
    0x55a9c083b3d0: <multiple use>
          0x55a9c0811340: <multiple use>
          0x55a9c083ab90: i32 = Register %vreg1 [ID=2]

        0x55a9c083ac98: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083ab90 [ORD=1] [ID=7]

          0x55a9c0811340: <multiple use>
          0x55a9c083a980: i32 = Register %vreg0 [ID=1]

        0x55a9c083aa88: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083a980 [ORD=1] [ID=6]

      0x55a9c083ada0: i32 = add 0x55a9c083ac98, 0x55a9c083aa88 [ORD=2] [ID=9]

        0x55a9c0811340: <multiple use>
          0x55a9c083afb0: i32 = TargetGlobalAddress<i32* @global> 0 [ORD=3]

        0x55a9c083b6e8: i32 = LOAD_SYM 0x55a9c083afb0 [ORD=3]

        0x55a9c083b0b8: i32 = undef [ID=4]

      0x55a9c083b1c0: i32,ch = load 0x55a9c0811340, 0x55a9c083b6e8, 0x55a9c083b0b8<LD4[@global](tbaa=<badref>)> [ORD=3] [ID=8]

    0x55a9c083b2c8: i32 = add 0x55a9c083ada0, 0x55a9c083b1c0 [ORD=4] [ID=10]

  0x55a9c083b4d8: ch,glue = CopyToReg 0x55a9c0811340, 0x55a9c083b3d0, 0x55a9c083b2c8 [ORD=5] [ID=11]

    0x55a9c083b4d8: <multiple use>
    0x55a9c083b3d0: <multiple use>
    0x55a9c083b4d8: <multiple use>
  0x55a9c083b5e0: ch = RetFlag 0x55a9c083b4d8, 0x55a9c083b3d0, 0x55a9c083b4d8:1 [ORD=5] [ID=12]
```
被替换为LOAD_SYM节点类型。指令选择结束后，LOAD_SYM被替换为MOVi32
```
ISEL: Match complete!
===== Instruction selection ends:
Selected selection DAG: BB#0 'foo:entry'
SelectionDAG has 14 nodes:
  0x55a9c0811340: ch = EntryToken

  0x55a9c083b3d0: i32 = Register %R0

    0x55a9c0811340: <multiple use>
    0x55a9c083b3d0: <multiple use>
          0x55a9c0811340: <multiple use>
          0x55a9c083ab90: i32 = Register %vreg1

        0x55a9c083ac98: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083ab90 [ORD=1]

          0x55a9c0811340: <multiple use>
          0x55a9c083a980: i32 = Register %vreg0

        0x55a9c083aa88: i32,ch = CopyFromReg 0x55a9c0811340, 0x55a9c083a980 [ORD=1]

      0x55a9c083ada0: i32 = ADDrr 0x55a9c083ac98, 0x55a9c083aa88 [ORD=2]

          0x55a9c083afb0: i32 = TargetGlobalAddress<i32* @global> 0 [ORD=3]

        0x55a9c083b6e8: i32 = MOVi32 0x55a9c083afb0 [ORD=3]

        0x55a9c083aea8: i32 = TargetConstant<0>

        0x55a9c0811340: <multiple use>
      0x55a9c083b1c0: i32,ch = LDR 0x55a9c083b6e8, 0x55a9c083aea8, 0x55a9c0811340<Mem:LD4[@global](tbaa=<badref>)> [ORD=3]

    0x55a9c083b2c8: i32 = ADDrr 0x55a9c083ada0, 0x55a9c083b1c0 [ORD=4]

  0x55a9c083b4d8: ch,glue = CopyToReg 0x55a9c0811340, 0x55a9c083b3d0, 0x55a9c083b2c8 [ORD=5]

    0x55a9c083b3d0: <multiple use>
    0x55a9c083b4d8: <multiple use>
    0x55a9c083b4d8: <multiple use>
  0x55a9c083b5e0: ch = RET 0x55a9c083b3d0, 0x55a9c083b4d8, 0x55a9c083b4d8:1 [ORD=5]
```
生成的MachineInstr为
```
# Machine code for function foo: SSA
Function Live Ins: %R0 in %vreg0, %R1 in %vreg1

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R1
        %vreg1<def> = COPY %R1; GRRegs:%vreg1
        %vreg0<def> = COPY %R0; GRRegs:%vreg0
        %vreg2<def> = ADDrr %vreg1, %vreg0; GRRegs:%vreg2,%vreg1,%vreg0
        %vreg3<def> = MOVi32 <ga:@global>; GRRegs:%vreg3
        %vreg4<def> = LDR %vreg3<kill>, 0; mem:LD4[@global](tbaa=<badref>) GRRegs:%vreg4,%vreg3
        %vreg5<def> = ADDrr %vreg2<kill>, %vreg4<kill>; GRRegs:%vreg5,%vreg2,%vreg4
        %R0<def> = COPY %vreg5; GRRegs:%vreg5
        RET %R0, %LR<imp-use>
```
MOVi32作为一个宏，在后面的阶段会被处理掉。
由LEGInstrInfo::expandPostRAPseudo处理宏。寄存器分配后，仍然保留的宏指令。宏指令协助寄存器分配。
LEGInstrInfo.cpp给出了一个例子，用于将MOVi32扩展为多条指令。
```
138 bool LEGInstrInfo::expandPostRAPseudo(MachineBasicBlock::iterator MI) const
144   case LEG::MOVi32: {
```
这里的函数处理MOVi32后结果为
```
# *** IR Dump After Post-RA pseudo instruction expansion pass ***:
# Machine code for function foo: Post SSA
Function Live Ins: %R0 in %vreg0, %R1 in %vreg1

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R1
        %R0<def> = ADDrr %R1<kill>, %R0<kill>
        %R1<def> = MOVLOi16 <ga:@global>[TF=1]
        %R1<def> = MOVHIi16 %R1, <ga:@global>[TF=2]
        %R1<def> = LDR %R1<kill>, 0; mem:LD4[@global](tbaa=<badref>)
        %R0<def> = ADDrr %R0<kill>, %R1<kill>
        RET %R0, %LR<imp-use>
```
MOVi32被替换为MOVLOi16和MOVHIi16两条指令。这是两条实际指令，能够直接生成汇编。

