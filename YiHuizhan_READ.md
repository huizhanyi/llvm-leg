~/build-llvm-leg/bin/clang -target leg-unknown-unknown -O1 -emit-llvm -S ex2.c
~/build-llvm-leg/bin/llc -march leg -debug-only=isel ex2.ll
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll  -relocation-model=pic -filetype=asm -o -
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll
~/build-llvm-leg/bin/llc -march leg -view-dag-combine1-dags ex2.ll
```
~/build-llvm-leg/bin/llc -march leg -debug-pass=Structure ex2.ll
Pass Arguments:  -targetlibinfo -datalayout -jump-instr-table-info -targetpassconfig -no-aa -tbaa -scoped-noalias -assumption-tracker -basicaa -notti -collector-metadata -machinemoduleinfo -machine-branch-prob -jump-instr-tables -verify -verify-di -domtree -loops -loop-simplify -scalar-evolution -iv-users -loop-reduce -gc-lowering -unreachableblockelim -consthoist -partially-inline-libcalls -codegenprepare -lowerinvoke -unreachableblockelim -verify-di -stack-protector -verify -domtree -loops -branch-prob -expand-isel-pseudos -tailduplication -opt-phis -machinedomtree -slotindexes -stack-coloring -localstackalloc -dead-mi-elimination -machinedomtree -machine-loops -machinelicm -machine-cse -machinepostdomtree -machine-block-freq -machine-sink -peephole-opts -dead-mi-elimination -processimpdefs -unreachable-mbb-elimination -livevars -machinedomtree -machine-loops -phi-node-elimination -twoaddressinstruction -slotindexes -liveintervals -simple-register-coalescing -machine-block-freq -livedebugvars -livestacks -virtregmap -liveregmatrix -edge-bundles -spill-code-placement -virtregrewriter -stack-slot-coloring -machinelicm -prologepilog -machine-block-freq -branch-folder -tailduplication -machine-cp -postrapseudos -machinedomtree -machine-loops -post-RA-sched -gc-analysis -machine-block-freq -block-placement2 -stackmap-liveness -machinedomtree -machine-loops
Target Library Information
Data Layout
Jump-Instruction Table Info
Target Pass Configuration
No Alias Analysis (always returns 'may' alias)
Type-Based Alias Analysis
Scoped NoAlias Alias Analysis
Assumption Tracker
Basic Alias Analysis (stateless AA impl)
No target information
Create Garbage Collector Module Metadata
Machine Module Information
Machine Branch Probability Analysis
  ModulePass Manager
    Jump-Instruction Tables
    FunctionPass Manager
      Module Verifier
    Debug Info Verifier
    FunctionPass Manager
      Dominator Tree Construction
      Natural Loop Information
      Canonicalize natural loops
      Scalar Evolution Analysis
      Loop Pass Manager
        Induction Variable Users
        Loop Strength Reduction
      Lower Garbage Collection Instructions
      Remove unreachable blocks from the CFG
      Constant Hoisting
      Partially inline calls to library functions
      CodeGen Prepare
      Lower invoke and unwind, for unwindless code generators
      Remove unreachable blocks from the CFG
    Debug Info Verifier
    FunctionPass Manager
      Insert stack protectors
      Module Verifier
      Machine Function Analysis
      Dominator Tree Construction
      Natural Loop Information
      Branch Probability Analysis
      LEG DAG->DAG Pattern Instruction Selection
      Expand ISel Pseudo-instructions
      Tail Duplication
      Optimize machine instruction PHIs
      MachineDominator Tree Construction
      Slot index numbering
      Merge disjoint stack slots
      Local Stack Slot Allocation
      Remove dead machine instructions
      MachineDominator Tree Construction
      Machine Natural Loop Construction
      Machine Loop Invariant Code Motion
      Machine Common Subexpression Elimination
      MachinePostDominator Tree Construction
      Machine Block Frequency Analysis
      Machine code sinking
      Peephole Optimizations
      Remove dead machine instructions
      Process Implicit Definitions
      Remove unreachable machine basic blocks
      Live Variable Analysis
      MachineDominator Tree Construction
      Machine Natural Loop Construction
      Eliminate PHI nodes for register allocation
      Two-Address instruction pass
      Slot index numbering
      Live Interval Analysis
      Simple Register Coalescing
      Machine Block Frequency Analysis
      Debug Variable Analysis
      Live Stack Slot Analysis
      Virtual Register Map
      Live Register Matrix
      Bundle Machine CFG Edges
      Spill Code Placement Analysis
      Greedy Register Allocator
      Virtual Register Rewriter
      Stack Slot Coloring
      Machine Loop Invariant Code Motion
      Prologue/Epilogue Insertion & Frame Finalization
      Machine Block Frequency Analysis
      Control Flow Optimizer
      Tail Duplication
      Machine Copy Propagation Pass
      Post-RA pseudo instruction expansion pass
      MachineDominator Tree Construction
      Machine Natural Loop Construction
      Post RA top-down list latency scheduler
      Analyze Machine Code For Garbage Collection
      Machine Block Frequency Analysis
      Branch Probability Basic Block Placement
      StackMap Liveness Analysis
      MachineDominator Tree Construction
      Machine Natural Loop Construction
      LEG Assembly Printer
```
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
根据后面的分析，主要完成对入参数据的使用，生成对寄存器的拷贝或者对栈变量的加载操作。
SelectionDAGISel::runOnMachineFunction -> SelectionDAGISel::SelectAllBasicBlocks (entry block) -> SelectionDAGISel::LowerArguments -> LEGTargetLowering::LowerFormalArguments
```
243 SDValue LEGTargetLowering::LowerFormalArguments(
244     SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
245     const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc dl, SelectionDAG &DAG,
246     SmallVectorImpl<SDValue> &InVals) const {
调用td文件生成的函数，确定参数的传递方式。Ins是一个引用入参，记录了每个参数需要使用什么来传递。
257   CCInfo.AnalyzeFormalArguments(Ins, CC_LEG);
259   for (auto &VA : ArgLocs) {
如果是寄存器传递的参数
260     if (VA.isRegLoc()) {
生成虚拟寄存器
265       const unsigned VReg = RegInfo.createVirtualRegister(&LEG::GRRegsRegClass);
增加指定的寄存器为LiveIn
266       RegInfo.addLiveIn(VA.getLocReg(), VReg);
增加读寄存器指令
267       SDValue ArgIn = DAG.getCopyFromReg(Chain, dl, VReg, RegVT);
保存生成的SelectionDAG节点
269       InVals.push_back(ArgIn);
对应的这些节点在初始的DAG上面就可以看到，因此这个函数在初始到DAG转化时就被调用过。
```
指令选择的最后调度阶段，生成MachineInstr表示。
结束时仍然保留虚拟寄存器和对应物理寄存器和虚拟寄存器的关系
```
*** MachineFunction at end of ISel ***
# Machine code for function foo: SSA
Function Live Ins: %R0 in %vreg0, %R1 in %vreg1

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R1
拷贝%R1到%vreg1
        %vreg1<def> = COPY %R1; GRRegs:%vreg1
拷贝%R0到%vreg0
        %vreg0<def> = COPY %R0; GRRegs:%vreg0
        %vreg2<def> = ADDrr %vreg1, %vreg0; GRRegs:%vreg2,%vreg1,%vreg0
        %vreg3<def> = MOVi32 <ga:@global>; GRRegs:%vreg3
        %vreg4<def> = LDR %vreg3<kill>, 0; mem:LD4[@global](tbaa=<badref>) GRRegs:%vreg4,%vreg3
        %vreg5<def> = ADDrr %vreg2<kill>, %vreg4<kill>; GRRegs:%vreg5,%vreg2,%vreg4
        %R0<def> = COPY %vreg5; GRRegs:%vreg5
        RET %R0, %LR<imp-use>

# End machine code for function foo.
```
对于通过栈传递的数据
```
这里一定时内存位置信息
273     assert(VA.isMemLoc() &&
274            "Can only pass arguments as either registers or via the stack");
275
返回栈偏移
276     const unsigned Offset = VA.getLocMemOffset();
277
这里处理的固定对象，栈帧中插入一个固定对象信息，返回一个负值的固定对象索引
278     const int FI = MF.getFrameInfo()->CreateFixedObject(4, Offset, true);
生成帧索引指针
279     SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy());
280
281     assert(VA.getValVT() == MVT::i32 &&
282            "Only support passing arguments as i32");
生成一个加载类型SDNode，这里对应从FrameIndex位置加载数据
283     SDValue Load = DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr,
284                                MachinePointerInfo(), false, false, false, 0);
285
286     InVals.push_back(Load);
```
```
*** MachineFunction at end of ISel ***
# Machine code for function foo: SSA
这里一个数据使用了栈传递数据
Frame Objects:
  fi#-1: size=4, align=4, fixed, at location [SP]
其他使用寄存器传递数据
Function Live Ins: %R0 in %vreg0, %R1 in %vreg1, %R2 in %vreg2, %R3 in %vreg3

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R1 %R2 %R3
        %vreg3<def> = COPY %R3; GRRegs:%vreg3
        %vreg2<def> = COPY %R2; GRRegs:%vreg2
        %vreg1<def> = COPY %R1; GRRegs:%vreg1
        %vreg0<def> = COPY %R0; GRRegs:%vreg0
这里增加了一个LDR指令，加载栈数据到%vreg4
        %vreg4<def> = LDR <fi#-1>, 0; mem:LD4[FixedStack-1] GRRegs:%vreg4
        %vreg5<def> = ADDrr %vreg1, %vreg0; GRRegs:%vreg5,%vreg1,%vreg0
        %vreg6<def> = ADDrr %vreg5<kill>, %vreg2; GRRegs:%vreg6,%vreg5,%vreg2
        %vreg7<def> = ADDrr %vreg6<kill>, %vreg3; GRRegs:%vreg7,%vreg6,%vreg3
        %vreg8<def> = ADDrr %vreg7<kill>, %vreg4<kill>; GRRegs:%vreg8,%vreg7,%vreg4
        %R0<def> = COPY %vreg8; GRRegs:%vreg8
        RET %R0, %LR<imp-use>
```
### LowerReturn
钩子函数，检查返回值outs是否能够放到返回寄存器中。如果返回假，需要完成sret-demotion。
```
296 bool LEGTargetLowering::CanLowerReturn(
297     CallingConv::ID CallConv, MachineFunction &MF, bool isVarArg,
298     const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
299   SmallVector<CCValAssign, 16> RVLocs;
300   CCState CCInfo(CallConv, isVarArg, MF, RVLocs, Context);
使用calling conv定义处理，如果不能通过寄存器或者栈处理，会返回false，导致返回false
301   if (!CCInfo.CheckReturn(Outs, RetCC_LEG)) {
302     return false;
303   }
变参数情况下，且Stackoffset非零情况，也返回失败
304   if (CCInfo.getNextStackOffset() != 0 && isVarArg) {
305     return false;
306   }
否则正常返回，不需要sret-demotion
307   return true;
308 }
```
LEGTargetLowering::LowerReturn
```
310 SDValue
311 LEGTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
312                                bool isVarArg,
313                                const SmallVectorImpl<ISD::OutputArg> &Outs,
314                                const SmallVectorImpl<SDValue> &OutVals,
315                                SDLoc dl, SelectionDAG &DAG) const {
生成LEGISD::RET_FLAG操作数
331   SmallVector<SDValue, 4> RetOps(1, Chain);
332
333   // Copy the result values into the output registers.
334   for (unsigned i = 0, e = RVLocs.size(); i < e; ++i) {
逐一处理每个返回值，这里只处理寄存器传递的情况
335     CCValAssign &VA = RVLocs[i];
336     assert(VA.isRegLoc() && "Can only return in registers!");
337
生成CopyToReg，将返回值拷贝到寄存器，因为只支持寄存器返回返回值
338     Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(), OutVals[i], Flag);
339
生成依赖链
340     Flag = Chain.getValue(1);
记录每个返回的寄存器
341     RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
342   }
343
第一个RET_FLAG是拷贝到寄存器的所有操作，表明必须要完成这些拷贝操作，才能执行返回
344   RetOps[0] = Chain; // Update chain.
345
增加一个对拷贝操作的依赖
346   // Add the flag if we have it.
347   if (Flag.getNode()) {
348     RetOps.push_back(Flag);
349   }
350
生成返回操作节点，操作数是RetOps。
351   return DAG.getNode(LEGISD::RET_FLAG, dl, MVT::Other, RetOps);
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

