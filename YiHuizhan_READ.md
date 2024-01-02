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
这里生成LEGISD::RET_FLAG DAG节点，对应节点类型在指令选择阶段会被替换为RET指令
```
169 let isTerminator = 1, isReturn = 1, isBarrier = 1, Uses = [LR] in {
170   def RET : InstLEG<(outs), (ins variable_ops),
171                     "bx lr",  [(LEGRetFlag)]> {
172     let Inst{27-0}  = 0b0001001011111111111100011110;
173   }
174 }
```
这里发现LEGISD::RET_FLAG类型节点时，会替换为RET指令，入参这里是一个特殊的variable_ops，代表可变参数数目。前面Uses = \[LR\]表示需要使用到LR寄存器。因此指令选择结束的表示形式为
```
# Machine code for function foo: SSA
Frame Objects:
  fi#-1: size=4, align=4, fixed, at location [SP]
Function Live Ins: %R0 in %vreg0, %R1 in %vreg1, %R2 in %vreg2, %R3 in %vreg3

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R1 %R2 %R3
        %vreg3<def> = COPY %R3; GRRegs:%vreg3
        %vreg2<def> = COPY %R2; GRRegs:%vreg2
        %vreg1<def> = COPY %R1; GRRegs:%vreg1
        %vreg0<def> = COPY %R0; GRRegs:%vreg0
        %vreg4<def> = LDR <fi#-1>, 0; mem:LD4[FixedStack-1] GRRegs:%vreg4
        %vreg5<def> = ADDrr %vreg1, %vreg0; GRRegs:%vreg5,%vreg1,%vreg0
        %vreg6<def> = ADDrr %vreg5<kill>, %vreg2; GRRegs:%vreg6,%vreg5,%vreg2
        %vreg7<def> = ADDrr %vreg6<kill>, %vreg3; GRRegs:%vreg7,%vreg6,%vreg3
        %vreg8<def> = ADDrr %vreg7<kill>, %vreg4<kill>; GRRegs:%vreg8,%vreg7,%vreg4
        %R0<def> = COPY %vreg8; GRRegs:%vreg8
        RET %R0, %LR<imp-use>
```
这里RET使用了%R0寄存器和%LR寄存器
### LowerCall
当函数被调用时，需要调用前所有实参拷贝到正确位置，调用结束把返回值拷贝到指定的虚寄存器。整个过程由CALLSEQ_BEGIN和CALLSEQ_END节点围绕到一起。在指令选择节点这些节点变换为ADJCALLSTACKDOWN和ADJCALLSTACKUP伪指令。函数调用由LowerCall函数处理，生成恰当的SDValues链。
LEGISelLowering.cpp
```
 98 SDValue LEGTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
 99                                      SmallVectorImpl<SDValue> &InVals) const {
```
原型定义
```
这个hook函数用于将call操作lower到指定的DAG。调用的输出参数（传递到call的参数）通过Outs数组描述，调用返回的值用Ins数组描述。
2324   /// This hook must be implemented to lower calls into the the specified
2325   /// DAG. The outgoing arguments to the call are described by the Outs array,
2326   /// and the values to be returned by the call are described by the Ins
2327   /// array. The implementation should fill in the InVals array with legal-type
2328   /// return values from the call, and return the resulting token chain value.
2329   virtual SDValue
2330     LowerCall(CallLoweringInfo &/*CLI*/,
2331               SmallVectorImpl<SDValue> &/*InVals*/) const {
2332     llvm_unreachable("Not Implemented");
2333   }
```
```
122   // Get the size of the outgoing arguments stack space requirement.
123   const unsigned NumBytes = CCInfo.getNextStackOffset();
124
这里看CALLSEQ_START指向的数据为传递输入参数的大小。CALLSEQ_END也保存了这个值。
125   Chain =
126       DAG.getCALLSEQ_START(Chain, DAG.getIntPtrConstant(NumBytes, true), dl);

取用来传递的寄存器和对应的参数值
140       RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
处理内存传递操作数
150     MemOpChains.push_back(DAG.getStore(Chain, dl, Arg, PtrOff,
151                                        MachinePointerInfo(), false, false, 0));
输出所有栈传递操作的store操作，这里生成了一个TokenFactor DAG节点，看DAG图会生成对应栈保存参数的Store操作
154   // Emit all stores, make sure they occur before the call.
155   if (!MemOpChains.empty()) {
156     Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);
157   }
生成到寄存器的拷贝chain
161   SDValue InFlag;
162   for (auto &Reg : RegsToPass) {
163     Chain = DAG.getCopyToReg(Chain, dl, Reg.first, Reg.second, InFlag);
164     InFlag = Chain.getValue(1);
165   }
增加调用需要保留的寄存器掩码，这里是Callee-saved寄存器
183   // Add a register mask operand representing the call-preserved registers.
184   const uint32_t *Mask;
185   const TargetRegisterInfo *TRI =
186       getTargetMachine().getSubtargetImpl()->getRegisterInfo();
187   Mask = TRI->getCallPreservedMask(CallConv);
生成调用指令
199   Chain = DAG.getNode(LEGISD::CALL, dl, NodeTys, Ops);
生成CALLSEQ_END DAG节点
202   Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, true),
203                              DAG.getIntPtrConstant(0, true), InFlag, dl);
处理Call的返回结果
210   return LowerCallResult(Chain, InFlag, CallConv, isVarArg, Ins, dl, DAG,
211                          InVals);

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
### Frame Lowering
LEGFrameLowering.cpp
emitPrologue/emitEpilogue/eliminateCallFramePseudoInstr在下面的PASS被调用，这已经完成了指令选择。
Prologue/Epilogue Insertion & Frame Finalization
```
 57 // Return zero if the offset fits into the instruction as an immediate,
 58 // or the number of the register where the offset is materialized.
 59 static unsigned materializeOffset(MachineFunction &MF, MachineBasicBlock &MBB,
 60                                   MachineBasicBlock::iterator MBBI,
 61                                   unsigned Offset) {
 62   const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
 63   DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
 64   const uint64_t MaxSubImm = 0xfff;
 65   if (Offset <= MaxSubImm) {
因为定义的SUBri和ADDri中立即数字段为12bit
 66     // The stack offset fits in the ADD/SUB instruction.
 67     return 0;
 68   } else {
 69     // The stack offset does not fit in the ADD/SUB instruction.
 70     // Materialize the offset using MOVLO/MOVHI.
使用MOVLO/MOVHI指令
偏移寄存器使用R4
 71     unsigned OffsetReg = LEG::R4;
分为低16位和高16位
 72     unsigned OffsetLo = (unsigned)(Offset & 0xffff);
 73     unsigned OffsetHi = (unsigned)((Offset & 0xffff0000) >> 16);
将低16位使用MOVLOi16移至偏移寄存器
 74     BuildMI(MBB, MBBI, dl, TII.get(LEG::MOVLOi16), OffsetReg)
 75         .addImm(OffsetLo)
 76         .setMIFlag(MachineInstr::FrameSetup);
设置FrameSetup，指令用做Frame Setup的一部分
 77     if (OffsetHi) {
将高16位使用MOVHIi16移至偏移寄存器
 78       BuildMI(MBB, MBBI, dl, TII.get(LEG::MOVHIi16), OffsetReg)
 79           .addReg(OffsetReg)
 80           .addImm(OffsetHi)
设置FrameSetup，指令用做Frame Setup的一部分
 81           .setMIFlag(MachineInstr::FrameSetup);
 82     }
返回偏移寄存器
 83     return OffsetReg;
 84   }
 85 }
```
emitPrologue函数
```
 87 void LEGFrameLowering::emitPrologue(MachineFunction &MF) const {
 88   // Compute the stack size, to determine if we need a prologue at all.
 89   const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
 90   MachineBasicBlock &MBB = MF.front();
 91   MachineBasicBlock::iterator MBBI = MBB.begin();
 92   DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
 93   uint64_t StackSize = computeStackSize(MF);
 94   if (!StackSize) {
 95     return;
 96   }
 97
 98   // Adjust the stack pointer.
 99   unsigned StackReg = LEG::SP;
返回栈大小
100   unsigned OffsetReg = materializeOffset(MF, MBB, MBBI, (unsigned)StackSize);
如果需要寄存器
101   if (OffsetReg) {
使用栈寄存器减去偏移寄存器
102     BuildMI(MBB, MBBI, dl, TII.get(LEG::SUBrr), StackReg)
103         .addReg(StackReg)
104         .addReg(OffsetReg)
105         .setMIFlag(MachineInstr::FrameSetup);
106   } else {
否则直接使用立即数表示偏移，设置指令的MachineInstr::FrameSetup标志
107     BuildMI(MBB, MBBI, dl, TII.get(LEG::SUBri), StackReg)
108         .addReg(StackReg)
109         .addImm(StackSize)
110         .setMIFlag(MachineInstr::FrameSetup);
111   }
112 }
```
emitEpilogue
```
114 void LEGFrameLowering::emitEpilogue(MachineFunction &MF,
115                                     MachineBasicBlock &MBB) const {
116   // Compute the stack size, to determine if we need an epilogue at all.
117   const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
118   MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
119   DebugLoc dl = MBBI->getDebugLoc();
120   uint64_t StackSize = computeStackSize(MF);
121   if (!StackSize) {
122     return;
123   }
124
125   // Restore the stack pointer to what it was at the beginning of the function.
126   unsigned StackReg = LEG::SP;
127   unsigned OffsetReg = materializeOffset(MF, MBB, MBBI, (unsigned)StackSize);
128   if (OffsetReg) {
和prolog的处理相反，使用加法指令
129     BuildMI(MBB, MBBI, dl, TII.get(LEG::ADDrr), StackReg)
130         .addReg(StackReg)
131         .addReg(OffsetReg)
132         .setMIFlag(MachineInstr::FrameSetup);
133   } else {
134     BuildMI(MBB, MBBI, dl, TII.get(LEG::ADDri), StackReg)
135         .addReg(StackReg)
136         .addImm(StackSize)
137         .setMIFlag(MachineInstr::FrameSetup);
138   }
139 }
```
简单删除ADJCALLSTACKDOWN, ADJCALLSTACKUP伪指令
```
141 // This function eliminates ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo
142 // instructions
143 void LEGFrameLowering::eliminateCallFramePseudoInstr(
144     MachineFunction &MF, MachineBasicBlock &MBB,
145     MachineBasicBlock::iterator I) const {
146   if (I->getOpcode() == LEG::ADJCALLSTACKUP ||
147       I->getOpcode() == LEG::ADJCALLSTACKDOWN) {
148     MBB.erase(I);
149   }
150   return;
151 }
```
如下代码
```
define i32 @bar(i32 %a) #0 {
entry:
  %mul = mul nsw i32 %a, 3
  %call = tail call i32 @foo(i32 %a, i32 %mul, i32 %mul, i32 %mul, i32 %mul, i32 %a) #2
  ret i32 %call
}
```
After Prologue/Epilogue Insertion & Frame Finalization 处理后生成
```
# Machine code for function bar: Post SSA
Frame Objects:
  fi#0: size=4, align=4, at location [SP-4]
Function Live Ins: %R0 in %vreg0

BB#0: derived from LLVM BB %entry
    Live Ins: %R0 %R4
这里是emitPrologue生成
        %SP<def> = SUBri %SP, 4; flags: FrameSetup
这里用来保存callee-saved寄存器
*>        STR %R4<kill>, %SP, 0
        %R1<def> = MOVLOi16 3
        %R1<def> = MUL %R0, %R1<kill>
        %R2<def> = ADDri %SP, 4
这里用来保存输出参数
        STR %R0, %R2<kill>, 0; mem:ST4[<unknown>]
*>        STR %R1, %SP, 0; mem:ST4[<unknown>]
        %R4<def> = MOVi32 <ga:@foo>
        %R2<def> = COPY %R1
        %R3<def> = COPY %R1
        BL %R4<kill>, <regmask>, %LR<imp-def>, %SP<imp-use>, %R0<imp-use>, %R1<imp-use>, %R2<imp-use>, %R3<imp-use>, %SP<imp-def>, %R0<imp-def>
        %R4<def> = LDR %SP, 0
这里是emitEpilogue生成
        %SP<def> = ADDri %SP, 4; flags: FrameSetup
        RET %R0, %LR<imp-use>

# End machine code for function bar.
```
*>保存位置冲突，后面的保存把前面的位置覆盖掉了，所以结尾的LDR恢复%R4恢复的结果应该有问题。

### LEGDAGToDAGISel
指令选择PASS
指令选择节点将输入的DAG形式转换为目标指令表示的DAG形式。整个过程通过模式匹配过程完成，首先搜索已知的模式，然后输出对应的指令模式。大部分指令选择代码由tblgen生成。
```
 30 class LEGDAGToDAGISel : public SelectionDAGISel {
 31   const LEGSubtarget &Subtarget;
 32
 33 public:
 34   explicit LEGDAGToDAGISel(LEGTargetMachine &TM, CodeGenOpt::Level OptLevel)
 35       : SelectionDAGISel(TM, OptLevel), Subtarget(*TM.getSubtargetImpl()) {}
 36
选择节点的主要hooks入口
 37   SDNode *Select(SDNode *N);
 38
用在ComplexPattern定义中。
 39   bool SelectAddr(SDValue Addr, SDValue &Base, SDValue &Offset);
 40
这是一个PASS，返回PASS名字
 41   virtual const char *getPassName() const {
 42     return "LEG DAG->DAG Pattern Instruction Selection";
 43   }
 44
 45 private:
这是一个私有函数，LEG后端自己定义的。
 46   SDNode *SelectMoveImmediate(SDNode *N);
 47
 48 // Include the pieces autogenerated from the target description.
 49 #include "LEGGenDAGISel.inc"
提供SelectCode函数
 50 };
```
#### SelectAddr函数及ComplexPattern
首先检查SelectAddr函数，这个函数定义在类LEGDAGToDAGISel中，被tblgen的ComplexPattern调用。原因是LEGGenDAGISel.inc头文件中包含了ComplexPattern生成的函数CheckComplexPattern，该模式定义时，第三个字段是
```
class ComplexPattern<ValueType ty, int numops, string fn,
                     list<SDNode> roots = [], list<SDNodeProperty> props = []> {

def addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;
```
因为ComplexPattern生成的函数CheckComplexPattern包含在类LEGDAGToDAGISel中，成为类的一个方法，而SelectAddr函数则是类的方法，因此可以直接调用SelectAddr方法。
检查SelectAddr定义
```
看参数，这里第一个参数是对应的地址，第二个是Base地址，第三个代表偏移？
bool LEGDAGToDAGISel::SelectAddr(SDValue Addr, SDValue &Base, SDValue &Offset) {
如果Addr是一个FrameIndexSDNode类型的SelectionDAG节点
  if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
根据节点类型，返回基地址和偏移地址
    Base = CurDAG->getTargetFrameIndex(FIN->getIndex(),
                                       getTargetLowering()->getPointerTy());
偏移地址是一个常量
    Offset = CurDAG->getTargetConstant(0, MVT::i32);
    return true;
  }
下面的情况返回false，没有取到基址和偏移
  if (Addr.getOpcode() == ISD::TargetExternalSymbol ||
      Addr.getOpcode() == ISD::TargetGlobalAddress ||
      Addr.getOpcode() == ISD::TargetGlobalTLSAddress) {
    return false; // direct calls.
  }
缺省情况下，直接使用Addr作为基地址
  Base = Addr;
使用常量0作为偏移
  Offset = CurDAG->getTargetConstant(0, MVT::i32);
返回真
  return true;
}
```
这里addr定义为ComplexPattern，addr被用于指令定义的模式匹配定义里。
```
def addr : ComplexPattern<iPTR, 2, "SelectAddr", [], []>;
```
根据这里的定义，这里会生成对应的CheckComplexPattern函数，这个函数应该用于对应的模式匹配。模式为iPTR，2个操作数，用特定的函数SelectAddr处理。

#### select函数
```
 99 SDNode *LEGDAGToDAGISel::Select(SDNode *N) {
100   switch (N->getOpcode()) {
特别处理ISD::Constant
101   case ISD::Constant:
102     return SelectMoveImmediate(N);
103   }
104
其他DAG节点类型使用自动生成的代码处理
105   return SelectCode(N);
106 }
```
对于常量ISD::Constant类型
```
 71 SDNode *LEGDAGToDAGISel::SelectMoveImmediate(SDNode *N) {
 72   // Make sure the immediate size is supported.
 73   ConstantSDNode *ConstVal = cast<ConstantSDNode>(N);
 74   uint64_t ImmVal = ConstVal->getZExtValue();
看后面的分析，这里的Mask应该是0xffffffff（32-bit），这里使用了36-bit,有误？
 75   uint64_t SupportedMask = 0xfffffffff;
这里对于超过Mask长度的立即数由自动代码处理，似乎后面没有对应处理代码
 76   if ((ImmVal & SupportedMask) != ImmVal) {
 77     return SelectCode(N);
 78   }
 79
 80   // Select the low part of the immediate move.
 81   uint64_t LoMask = 0xffff;
 82   uint64_t HiMask = 0xffff0000;
 83   uint64_t ImmLo = (ImmVal & LoMask);
 84   uint64_t ImmHi = (ImmVal & HiMask);
后续生成LEG::MOVLOi16/LEG::MOVHIi16两条指令，分别加载低16和高16位。
 85   SDValue ConstLo = CurDAG->getTargetConstant(ImmLo, MVT::i32);
 86   MachineSDNode *Move =
 87       CurDAG->getMachineNode(LEG::MOVLOi16, N, MVT::i32, ConstLo);
 88
 89   // Select the low part of the immediate move, if needed.
 90   if (ImmHi) {
 91     SDValue ConstHi = CurDAG->getTargetConstant(ImmHi >> 16, MVT::i32);
 92     Move = CurDAG->getMachineNode(LEG::MOVHIi16, N, MVT::i32, SDValue(Move, 0),
 93                                   ConstHi);
 94   }
 95
 96   return Move;
 97 }
```
### InstPrinter





