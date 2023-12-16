~/build-llvm-leg/bin/clang -target leg-unknown-unknown -emit-llvm -S ex1.c

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

