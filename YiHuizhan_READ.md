后端参考资料：https://github.com/lowRISC/riscv-llvm/tree/master/docs
```
~/build-llvm-leg/bin/clang -target leg-unknown-unknown -O1 -emit-llvm -S ex2.c
~/build-llvm-leg/bin/llc -march leg -debug-only=isel ex2.ll
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll  -relocation-model=pic -filetype=asm -o -
~/build-llvm-leg/bin/llc -march leg -print-after-all ex2.ll
~/build-llvm-leg/bin/llc -march leg -view-dag-combine1-dags ex2.ll
```
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
### 后端PASS的处理
```
#0  addPassesToGenerateCode (TM=0x555555f0769c <llvm::legacy::PassManagerImpl::add(llvm::Pass*)+46>, PM=..., DisableVerify=85,
    StartAfter=0x555555f0d536 <llvm::legacy::PassManager::add(llvm::Pass*)>, StopAfter=0x1a4c24c8ec303600)
    at /home/yhz/llvm-leg/lib/CodeGen/LLVMTargetMachine.cpp:88
#1  0x0000555555b2b7f0 in llvm::LLVMTargetMachine::addPassesToEmitFile (this=0x555556bc7380, PM=..., Out=...,
    FileType=llvm::TargetMachine::CGFT_AssemblyFile, DisableVerify=false, StartAfter=0x0, StopAfter=0x0)
    at /home/yhz/llvm-leg/lib/CodeGen/LLVMTargetMachine.cpp:150
#2  0x0000555555a02813 in compileModule (argv=0x7fffffffe028, Context=...) at /home/yhz/llvm-leg/tools/llc/llc.cpp:341
#3  0x0000555555a01a21 in main (argc=6, argv=0x7fffffffe028) at /home/yhz/llvm-leg/tools/llc/llc.cpp:199
```
llc.cpp
```
242   const Target *TheTarget = TargetRegistry::lookupTarget(MArch, TheTriple,
243                                                          Error);

276   std::unique_ptr<TargetMachine> target(
277       TheTarget->createTargetMachine(TheTriple.getTriple(), MCPU, FeaturesStr,
278                                      Options, RelocModel, CMModel, OLvl));

288   TargetMachine &Target = *target.get();
取目标平台的TargetMachine

340     // Ask the target to add backend passes as necessary.
341     if (Target.addPassesToEmitFile(PM, FOS, FileType, NoVerify,
342                                    StartAfterID, StopAfterID)) {
```
lib/CodeGen/LLVMTargetMachine.cpp
```
138 bool LLVMTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
139                                             formatted_raw_ostream &Out,
140                                             CodeGenFileType FileType,
141                                             bool DisableVerify,
142                                             AnalysisID StartAfter,
143                                             AnalysisID StopAfter) {

150   MCContext *Context = addPassesToGenerateCode(this, PM, DisableVerify,
151                                                StartAfter, StopAfter);

 84 static MCContext *addPassesToGenerateCode(LLVMTargetMachine *TM,
 85                                           PassManagerBase &PM,
 86                                           bool DisableVerify,
 87                                           AnalysisID StartAfter,
 88                                           AnalysisID StopAfter) {

 91   TM->addAnalysisPasses(PM);
这个LEG可以定制，目前实现没有什么内容
 93   // Targets may override createPassConfig to provide a target-specific
 94   // subclass.
 95   TargetPassConfig *PassConfig = TM->createPassConfig(PM);
 96   PassConfig->setStartStopPasses(StartAfter, StopAfter);
LEG有自己的TargetPassConfig定义，相应的可以定制一些pass
109   PassConfig->addISelPrepare();
指令选择前的准备，Target可定制

118   // Set up a MachineFunction for the rest of CodeGen to work on.
119   PM.add(new MachineFunctionAnalysis(*TM));
生成一个pass “Machine Function Analysis”,管理MachineFunction

127   // Ask the target for an isel.
128   if (PassConfig->addInstSelector())
增加指令选择PASS，Target定制LEGDAGToDAGISel
131   PassConfig->addMachinePasses();
增加目标独立的postISel代码生成pass，::addxx可以由目标override

176     MCInstPrinter *InstPrinter =
177       getTarget().createMCInstPrinter(MAI.getAssemblerDialect(), MAI,
178                                       MII, MRI, STI);
这里间接会调用createLEGMCInstPrinter函数，生成LEGInstPrinter

180     // Create a code emitter if asked to show the encoding.
181     MCCodeEmitter *MCE = nullptr;
182     if (Options.MCOptions.ShowMCEncoding)
183       MCE = getTarget().createMCCodeEmitter(MII, MRI, STI, *Context);
这里间接调用createLEGMCCodeEmitter，生成LEGMCCodeEmitter

185     MCAsmBackend *MAB = getTarget().createMCAsmBackend(MRI, getTargetTriple(),
186                                                        TargetCPU);
这里间接调用createLEGAsmBackend，生成ELFLEGAsmBackend

187     MCStreamer *S = getTarget().createAsmStreamer(
188         *Context, Out, Options.MCOptions.AsmVerbose,
189         Options.MCOptions.MCUseDwarfDirectory, InstPrinter, MCE, MAB,
190         Options.MCOptions.ShowMCInst);
这里间接调用createMCAsmStreamer，生成MCStreamer，这里把前面生成的InstPrinter, MCE, MAB,都作为入参使用
191     AsmStreamer.reset(S);

216   // Create the AsmPrinter, which takes ownership of AsmStreamer if successful.
217   FunctionPass *Printer = getTarget().createAsmPrinter(*this, *AsmStreamer);
这里间接生成LEGAsmPrinter，这是一个MachineFunctionPass
224   PM.add(Printer);
增加打印pass
```
addMachinePasses增加的pass一直到“StackMap Liveness Analysis”，之后就到了Printer遍。也就是之前所有的Pass都是MachineInstr表示，在Printer时才转化为MCInst表示。
而Instruction Selection之前增加了一个MachineFunctionAnalysis遍，生成MachineFunction，管理整个代码生成过程。
### Target调用和初始化
llc.cpp
```
166 int main(int argc, char **argv) {

177   InitializeAllTargets();
178   InitializeAllTargetMCs();
179   InitializeAllAsmPrinters();
180   InitializeAllAsmParsers();
```
```
InitializeAllTargets -> InitializeAllTargetInfos -> LLVMInitializeLEGTargetInfo
                    |-> LLVMInitializeLEGTarget
InitializeAllTargetMCs -> LLVMInitializeLEGTargetMC
InitializeAllAsmPrinters -> LLVMInitializeLEGAsmPrinter
```
TargetInfo/LEGTargetInfo.cpp
```
 15 Target llvm::TheLEGTarget;
 16
 17 extern "C" void LLVMInitializeLEGTargetInfo() {
 18   RegisterTarget<Triple::leg> X(TheLEGTarget, "leg", "LEG");
 19 }
```
RegisterTarget在TargetRegistry登记一个Target，名称为"leg"，描述为"LEG"
LEGTargetMachine.cpp
```
 64 // Force static initialization.
 65 extern "C" void LLVMInitializeLEGTarget() {
 66   RegisterTargetMachine<LEGTargetMachine> X(TheLEGTarget);
 67 }
```
在TargetRegistry登记一个TargetMachine
MCTargetDesc/LEGMCTargetDesc.cpp
```
107 extern "C" void LLVMInitializeLEGTargetMC() {
108   // Register the MC asm info.
109   RegisterMCAsmInfoFn X(TheLEGTarget, createLEGMCAsmInfo);
110
111   // Register the MC codegen info.
112   TargetRegistry::RegisterMCCodeGenInfo(TheLEGTarget, createLEGMCCodeGenInfo);
113
114   // Register the MC instruction info.
115   TargetRegistry::RegisterMCInstrInfo(TheLEGTarget, createLEGMCInstrInfo);
116
117   // Register the MC register info.
118   TargetRegistry::RegisterMCRegInfo(TheLEGTarget, createLEGMCRegisterInfo);
119
120   // Register the MC subtarget info.
121   TargetRegistry::RegisterMCSubtargetInfo(TheLEGTarget,
122                                           createLEGMCSubtargetInfo);
123
124   // Register the MCInstPrinter
125   TargetRegistry::RegisterMCInstPrinter(TheLEGTarget, createLEGMCInstPrinter);
126
127   // Register the ASM Backend.
128   TargetRegistry::RegisterMCAsmBackend(TheLEGTarget, createLEGAsmBackend);
129
130   // Register the assembly streamer.
131   TargetRegistry::RegisterAsmStreamer(TheLEGTarget, createMCAsmStreamer);
132
133   // Register the object streamer.
134   TargetRegistry::RegisterMCObjectStreamer(TheLEGTarget, createMCStreamer);
135
136   // Register the MCCodeEmitter
137   TargetRegistry::RegisterMCCodeEmitter(TheLEGTarget, createLEGMCCodeEmitter);
138 }
```
这里调用一系列的TargetRegistry的方法注册相关处理函数。
LEGAsmPrinter.cpp
```
extern "C" void LLVMInitializeLEGAsmPrinter() {
  RegisterAsmPrinter<LEGAsmPrinter> X(TheLEGTarget);
}
```
在TargetRegistry中登记类LEGAsmPrinter
上述过程把所有目标相关的数据关联到Target,登记PASS时可以使用这些数据。
#### 整体上看是 初始化共享的数据结构Target -> 登记PASS关联到这些数据

### Calling convention lowering
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
#### LowerFormalArguments
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
#### LowerReturn
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
#### LowerCall
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
### LEGAsmPrinter
LEGMCInstLower用来把MachineInstr lower到MCInst类
整体看是把不同层次表示lower
```
 25 class LLVM_LIBRARY_VISIBILITY LEGMCInstLower {
 26   typedef MachineOperand::MachineOperandType MachineOperandType;
 27   MCContext *Ctx;
 28   Mangler *Mang;
 29   AsmPrinter &Printer;
 30
 31 public:
constructor函数
 32   LEGMCInstLower(class AsmPrinter &asmprinter);
初始化函数
 33   void Initialize(Mangler *mang, MCContext *C);
MachineInstr到MCInst的转化
 34   void Lower(const MachineInstr *MI, MCInst &OutMI) const;
操作数转化
 35   MCOperand LowerOperand(const MachineOperand &MO, unsigned offset = 0) const;
 36
 37 private:
特殊的SymbolOperand的转化
 38   MCOperand LowerSymbolOperand(const MachineOperand &MO,
 39                                MachineOperandType MOTy, unsigned Offset) const;
 40 };
```
LEGMCInstLower.cpp
定义
指令的lower
```
124 void LEGMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
MCInst的操作码和MachineInstr的操作码相同
125   OutMI.setOpcode(MI->getOpcode());
126
127   for (auto &MO : MI->operands()) {
逐个处理每个操作数
128     const MCOperand MCOp = LowerOperand(MO);
129
如果处理成功，则加到MCInst指令上
130     if (MCOp.isValid()) {
131       OutMI.addOperand(MCOp);
132     }
133   }
134 }
```
对应的这个类用在了LEGAsmPrinter.cpp，就是对应的最后的LEG Assembly Printer
```
 50 class LEGAsmPrinter : public AsmPrinter {
 51   const LEGSubtarget &Subtarget;
 52   LEGMCInstLower MCInstLowering;
 53
 54 public:
 55   explicit LEGAsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
 56       : AsmPrinter(TM, Streamer), Subtarget(TM.getSubtarget<LEGSubtarget>()),
 57         MCInstLowering(*this) {}
 58
遍名字
 59   virtual const char *getPassName() const { return "LEG Assembly Printer"; }
 60
override继承的函数
 61   void EmitFunctionEntryLabel();
 62   void EmitInstruction(const MachineInstr *MI);
 63   void EmitFunctionBodyStart();
 64 };
```
```
函数的第一个基本块之前执行
 67 void LEGAsmPrinter::EmitFunctionBodyStart() {
 68   MCInstLowering.Initialize(Mang, &MF->getContext());
 69 }
输出函数入口标号
 71 void LEGAsmPrinter::EmitFunctionEntryLabel() {
 72   OutStreamer.EmitLabel(CurrentFnSym);
 73 }
这里看，从MachineInstr到MCInst完成Lower后，马上就执行了指令的Emit
 75 void LEGAsmPrinter::EmitInstruction(const MachineInstr *MI) {
 76   MCInst TmpInst;
 77   MCInstLowering.Lower(MI, TmpInst);
 78
这里的OutStreamer是MCStreamer的子类MCAsmStreamer类型
这里Emit实际上调用了InstPrinter，输出MCInst。
 79   EmitToStreamer(OutStreamer, TmpInst);
 80 }
```
分析AsmPrinter的调用过程,入口函数为runOnMachineFunction
```
184     SetupMachineFunction(MF);
185     EmitFunctionHeader();
186     EmitFunctionBody();
```
```
EmitFunctionHeader -> EmitFunctionEntryLabel
这里遍历函数中的所有结构，发射指令等相关的内容
EmitFunctionBody   -> EmitFunctionBodyStart
                   -> EmitInstruction
```
#### InstPrinter
MC描述参考：https://blog.llvm.org/2010/04/intro-to-llvm-mc-project.html
在目录InstPrinter，LEGInstPrinter.h注释，用于打印LEG MCInst到s文件
```
 24 class LEGInstPrinter : public MCInstPrinter {
 25 public:
 26   LEGInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
 27                  const MCRegisterInfo &MRI)
 28       : MCInstPrinter(MAI, MII, MRI) {}
 29
由tblgen自动生成，LEGGenAsmWriter.inc中包括定义LEGInstPrinter::printInstruction
 30   // Autogenerated by tblgen.
 31   void printInstruction(const MCInst *MI, raw_ostream &O);
由tblgen自动生成，LEGGenAsmWriter.inc中包括定义LEGInstPrinter::getRegisterName
 32   static const char *getRegisterName(unsigned RegNo);
 33
由tblgen自动生成还有LEGInstPrinter::printAliasInstr函数，但是没看到定义，这里应该是漏掉了

基类有定义，子类必须自己实现
 34   virtual void printRegName(raw_ostream &OS, unsigned RegNo) const;
 35   virtual void printInst(const MCInst *MI, raw_ostream &O, StringRef Annot);
 36
 37 private:
这个方法是memsrc操作数类型定义的PrintMethod
 38   void printAddrModeMemSrc(const MCInst *MI, unsigned OpNum, raw_ostream &O);

这个方法在printInstruction中有很多次调用
 39   void printOperand(const MCInst *MI, unsigned OpNo, raw_ostream &O);
这个方法用于定义PrintMethod，LEG中没有使用
 40   void printMemOperand(const MCInst *MI, int opNum, raw_ostream &O);
 41 };
```
LEGInstPrinter.cpp
```
 25 #include "LEGGenAsmWriter.inc"
 27 void LEGInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
调用自动生成的函数getRegisterName，生成寄存器字符串输出
 28   OS << StringRef(getRegisterName(RegNo)).lower();
 29 }
```
打印MCInst
```
 31 void LEGInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
 32                                StringRef Annot) {
调用自动生成的函数printInstruction
 33   printInstruction(MI, O);
 34   printAnnotation(O, Annot);
 35 }
```
MCInst定义在include/llvm/MC/MCInst.h

打印MCExpr，MCExpr定义在include/llvm/MC/MCExpr.h
```
这是一个内部函数，由printOperand调用
 37 static void printExpr(const MCExpr *Expr, raw_ostream &OS) {
 38   int Offset = 0;
 39   const MCSymbolRefExpr *SRE;
 40
 41   if (const MCBinaryExpr *BE = dyn_cast<MCBinaryExpr>(Expr)) {
 42     SRE = dyn_cast<MCSymbolRefExpr>(BE->getLHS());
 43     const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(BE->getRHS());
 44     assert(SRE && CE && "Binary expression must be sym+const.");
 45     Offset = CE->getValue();
 46   } else {
 47     SRE = dyn_cast<MCSymbolRefExpr>(Expr);
 48     assert(SRE && "Unexpected MCExpr type.");
 49   }
 50   const MCSymbolRefExpr::VariantKind Kind = SRE->getKind();
 51   assert(Kind == MCSymbolRefExpr::VK_LEG_LO ||
 52          Kind == MCSymbolRefExpr::VK_LEG_HI);
 53
 54   OS << SRE->getSymbol();
 55
 56   if (Offset) {
 57     if (Offset > 0) {
 58       OS << '+';
 59     }
 60     OS << Offset;
 61   }
 62 }
```
打印MEM访问类操作数,输出格式ldr r4, \[sp, \#4\]
```
 64 // Print a 'memsrc' operand which is a (Register, Offset) pair.
 65 void LEGInstPrinter::printAddrModeMemSrc(const MCInst *MI, unsigned OpNum,
 66                                          raw_ostream &O) {
 67   const MCOperand &Op1 = MI->getOperand(OpNum);
 68   const MCOperand &Op2 = MI->getOperand(OpNum + 1);
 69   O << "[";
 70   printRegName(O, Op1.getReg());
 71
 72   unsigned Offset = Op2.getImm();
 73   if (Offset) {
 74     O << ", #" << Offset;
 75   }
 76   O << "]";
 77 }
```
这个函数作为下面memsrc操作数类型定义的PrintMethod
```
 51 def memsrc : Operand<i32> {
 52   let MIOperandInfo = (ops GRRegs, i32imm);
 53   let PrintMethod = "printAddrModeMemSrc";
 54   let EncoderMethod = "getMemSrcValue";
 55 }
```
通过操作数打印函数
```
 590 class Operand<ValueType ty> : DAGOperand {
 591   ValueType Type = ty;
 592   string PrintMethod = "printOperand";
```
```
 79 void LEGInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
 80                                   raw_ostream &O) {
 81   const MCOperand &Op = MI->getOperand(OpNo);
如果是寄存器，则调用寄存器打印
 82   if (Op.isReg()) {
 83     printRegName(O, Op.getReg());
 84     return;
 85   }
 86
如果是立即数，打印#立即数
 87   if (Op.isImm()) {
 88     O << "#" << Op.getImm();
 89     return;
 90   }
 91
 92   assert(Op.isExpr() && "unknown operand kind in printOperand");
否则使用printExpr打印
 93   printExpr(Op.getExpr(), O);
 94 }
```

#### MCTargetDesc
LEGBaseInfo.h
定义了几个LEG具体的标志

LEGMCAsmInfo.h/cpp
汇编属性信息定义

LEGELFObjectWriter.cpp
ELF Writer
LEGMCCodeEmitter.cpp
生成指令的机器编码
```
 34 class LEGMCCodeEmitter : public MCCodeEmitter {
 35   LEGMCCodeEmitter(const LEGMCCodeEmitter &) LLVM_DELETED_FUNCTION;
 36   void operator=(const LEGMCCodeEmitter &) LLVM_DELETED_FUNCTION;
 37   const MCInstrInfo &MCII;
 38   const MCContext &CTX;
由TableGen生成，取指令的二进制编码
 46   // getBinaryCodeForInstr - TableGen'erated function for getting the
 47   // binary encoding for an instruction.
 48   uint64_t getBinaryCodeForInstr(const MCInst &MI,
 49                                  SmallVectorImpl<MCFixup> &Fixups,
 50                                  const MCSubtargetInfo &STI) const;
 51
每个操作数调用这个函数，返回操作数二进制编码。如果需要relocation，记录Fixups，返回0
 52   /// getMachineOpValue - Return binary encoding of operand. If the machine
 53   /// operand requires relocation, record the relocation and return zero.
 54   unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
 55                              SmallVectorImpl<MCFixup> &Fixups,
 56                              const MCSubtargetInfo &STI) const;
取memsrc类型的二进制编码
 58   unsigned getMemSrcValue(const MCInst &MI, unsigned OpIdx,
 59                           SmallVectorImpl<MCFixup> &Fixups,
 60                           const MCSubtargetInfo &STI) const;

 86 /// getMachineOpValue - Return binary encoding of operand. If the machine
 87 /// operand requires relocation, record the relocation and return zero.
 88 unsigned LEGMCCodeEmitter::getMachineOpValue(const MCInst &MI,
 89                                              const MCOperand &MO,
 90                                              SmallVectorImpl<MCFixup> &Fixups,
 91                                              const MCSubtargetInfo &STI) const {
110   assert (Kind == MCExpr::SymbolRef);
111
112   unsigned FixupKind;
113   switch (cast<MCSymbolRefExpr>(Expr)->getKind()) {
114   default:
115     llvm_unreachable("Unknown fixup kind!");
116   case MCSymbolRefExpr::VK_LEG_LO: {
117     FixupKind = LEG::fixup_leg_mov_lo16_pcrel;
118     break;
119   }
120   case MCSymbolRefExpr::VK_LEG_HI: {
121     FixupKind = LEG::fixup_leg_mov_hi16_pcrel;
122     break;
123   }
124   }
125
生成LEG::fixup_leg_mov_lo16_pcrel和LEG::fixup_leg_mov_hi16_pcrel的Fixups类型。LLVM会跟踪Fixups，后面处理。
126   Fixups.push_back(MCFixup::Create(0, MO.getExpr(), MCFixupKind(FixupKind)));


生成1条指令的编码
142 void LEGMCCodeEmitter::EncodeInstruction(const MCInst &MI, raw_ostream &OS,
143                                          SmallVectorImpl<MCFixup> &Fixups,
144                                          const MCSubtargetInfo &STI) const {
145   const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
146   if (Desc.getSize() != 4) {
147     llvm_unreachable("Unexpected instruction size!");
148   }
149
150   const uint32_t Binary = getBinaryCodeForInstr(MI, Fixups, STI);
151
152   EmitConstant(Binary, Desc.getSize(), OS);
153   ++MCNumEmitted;
154 }
```

LEGFixupKinds.h
定义Target specific fixup
参考一下：
https://www.embecosm.com/appnotes/ean10/html/ch06s02.html
https://github.com/lowRISC/riscv-llvm/blob/master/docs/06-relocations-and-fixups.mkd
```
 16 namespace LEG {
 17 enum Fixups {
 18   fixup_leg_mov_hi16_pcrel = FirstTargetFixupKind,
 19   fixup_leg_mov_lo16_pcrel,
 20
 21   // Marker
 22   LastTargetFixupKind,
 23   NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
 24 };
```
LEGAsmBackend.cpp
看注释是汇编器后端，目前LEG还不能通过汇编文件生成目标文件
这里主要定义是处理Fixup，在section处理结束时，section布局已经确定
```
汇编器后端
 40 class LEGAsmBackend : public MCAsmBackend {
 41 public:
 42   LEGAsmBackend(const Target &T, const StringRef TT) : MCAsmBackend() {}

 50   const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
 51     const static MCFixupKindInfo Infos[LEG::NumTargetFixupKinds] = {
 52       // This table *must* be in the order that the fixup_* kinds are defined in
 53       // LEGFixupKinds.h.
 54       //
 55       // Name                      Offset (bits) Size (bits)     Flags
 56       { "fixup_leg_mov_hi16_pcrel", 0, 32, MCFixupKindInfo::FKF_IsPCRel },
 57       { "fixup_leg_mov_lo16_pcrel", 0, 32, MCFixupKindInfo::FKF_IsPCRel },
 58     };
```
调整值，这里调整mov16指令的操作数
```
100 static unsigned adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
101                                  MCContext *Ctx = NULL) {
102   unsigned Kind = Fixup.getKind();
103   switch (Kind) {
104   default:
105     llvm_unreachable("Unknown fixup kind!");
如果是hi16的数据，需要把value移到低16位
106   case LEG::fixup_leg_mov_hi16_pcrel:
107     Value >>= 16;
108   // Intentional fall-through
不管是hi16还是lo16，都要把Lo12放在低12，把hi4放到inst{19-16}，这决定于指令编码
109   case LEG::fixup_leg_mov_lo16_pcrel:
110     unsigned Hi4  = (Value & 0xF000) >> 12;
111     unsigned Lo12 = Value & 0x0FFF;
112     // inst{19-16} = Hi4;
113     // inst{11-0} = Lo12;
114     Value = (Hi4 << 16) | (Lo12);
115     return Value;
116   }
117   return Value;
118 }
```





