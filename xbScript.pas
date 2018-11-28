unit xbScript;

interface
uses
  System.Classes,
  System.SysUtils,
  Variants,
  TypInfo,

  xbParser;

Type
  {Internal structure}
  TInstruction = (
    inPrepare,            // Allocate space in stack for local variables
    inPushInteger, // Push an integer in stack
    inPushDouble, // Push a double in stack
    inPushString, // Push a string in stack
    inPushConst, // Push a Null variant in stack
    inPushOutput, // Push the output result of a function in the stack
    inPushClass, // Push a class reference in stack
    inDuplicate, // // obsolete
    inDuplicate2, // // obsolete
    inDrop2, // // obsolete
    inSwap2, // // obsolete
    inOperNE, // Remove 2 values from stack, test if they are not equal, push the result in stack
    inOperGE, // Remove 2 values from stack, test if first is greater or equal to second, push the result in stack
    inOperLE, // Remove 2 values from stack, test if first is less or equal to second, push the result in stack
    inOperEqual,          // Remove 2 values from stack, test if first is equal to second, push the result in stack
    inOperGreater,        // Remove 2 values from stack, test if first is greater than second, push the result in stack
    inOperLess,           // Remove 2 values from stack, test if first is less than second, push the result in stack
    inOperAdd, // Remove 2 values from stack, add them and push the result in stack
    inOperConcat, // Remove 2 values from stack, concatenate them and push the result in stack
    inOperSub,            // Remove 2 values from stack, subtract the second from the first and push the result in stack
    inOperOr, // Remove 2 values from stack, perform the logic OR operation and push the result in stack
    inOperXor,            // Remove 2 values from stack, perform the logic XOR operation and push the result in stack
    inOperMul,            // Remove 2 values from stack, multiply the first by the second and push the result in stack
    inOperSlash,          // Remove 2 values from stack, divide the first by the second and push the result in stack with "/"
    inOperDiv,            // Remove 2 values from stack, divide the first by the second and push the result in stack as integer
    inOperMod,            // Remove 2 values from stack, divide the first by the second and push the rest as the result
    inOperAnd,            // Remove 2 values from stack, perform the logic AND operation and push the result in stack
    inOperShl,            // Remove 2 values from stack, perform the logic SHL operation and push the result in stack
    inOperShr,            // Remove 2 values from stack, perform the logic SHR operation and push the result in stack
    inOperExp,            // Remove 2 values from stack, raise the first to the power of the second and push the result in stack
    inOperIs, // Remove 2 values from stack, test if the class of the first corresponds or is derivated from the second, return the logic result of the test
    inOperNeg, // Complement the sign of the value in top of stack
    inOperNot, // Complement the value in top of stack using the logic NOT operation
    inJumpIfFalse, // Remove the value in top of stack and jump if it is zero
    inJumpIfTrue, // Remove the value in top of stack and jump if it is not zero
    inJump, // Jump unconditionally with no return
    inStoreVar, // Remove the value in top of stack and store it in the variable
    inStoreVarRef,        // Remove the value in top of stack and store it in the variable passed by reference
    inStoreIdxVar, // Remove the value in top of stack and store it in the variable
    inStoreIdxVarRef,     // Remove the value in top of stack and store it in the variable passed by reference
    inStoreVarInteger, // Store an integer parameter directly into the variable
    inStoreVarRefInteger, // Store an integer parameter directly into the variable passed by reference
    inStoreVarDouble, // Store a numeric (non integer) parameter directly into the variable
    inStoreVarRefDouble,  // Store a numeric (non integer) parameter directly into the variable passed by reference
    inStoreVarString, // Store a string parameter directly into the variable
    inStoreVarRefString, // Store a string parameter directly into the variable passed by reference
    inStoreVarConst, // Store a constant directly into the variable
    inStoreVarRefConst, // Store a constant directly into the variable passed by reference
    inCopyVar, // Copy the content of one variable to another without using the stack
    inCopyVarRef,         // Copy the content of one variable to another passed by reference without using the stack
    inStoreInput, // Store a subroutine input argument in a variable
    inPushVar, // Stack the content of a local variable
    inPushIdxVar, // Stack the content of a local indexed variable
    inPushVarByRef, // Stack a reference to a local variable
    inVarTest,            // Compare the value of the variable to the expression in the top of the stack, and jump to vInteger according to vByte
    inVarTestInteger,     // Compare the value of the variable to the integer parameter, and jump to vInteger according to vByte
    inVarTestDouble,      // Compare the value of the variable to the numeric (non integer) parameter, and jump to vInteger according to vByte
    inVarTestString,      // Compare the value of the variable to the string parameter, and jump to vInteger according to vByte
    inVarTestVar,         // Compare the value of the variable to the value of other variable, and jump to vInteger according to vByte
    inStoreGlobalVar, // Remove the value in top of stack and store it in the global variable
    inStoreGlobalIdxVar, // Remove the value in top of stack and store it in the global variable
    inPushGlobalVar, // Stack the content of a global variable
    inPushGlobalIdxVar, // Stack the content of a global indexed variable
    inPushGlobalVarByRef, // Stack a reference to the global variable
    inCall, // Jump to internal subroutine
    inRet, // Return from subroutine/end of try..except block. vInteger = 0 (ret); vInteger = 1 (try..execept)
    inExecFun, // Execute an external function
    inCallProc, // Execute a special procedure call
    inTryFinally, // Execute a code block protected with guaranteed conclusion
    inTryExcept, // Execute a code block protected with exception handling
    inCallClassProc, // Execute a procedure call to a class member (method or property)
    inBreakPoint, // Entry point to all debugger funcionalities
    inAbort, // Create a silent execution
    inArrayOf, // Create an array of variants with values in stack
    inCastClass, // Forces the object in stack to the class defined in instruction
    inWithObject,         // Adjust the instance to be used by the next CallProc instruction (With expression)
    inGlobalPrepare, // Allocate space in stack to global variables
    inTestIfFalse, // Jump if current stack is false, without removing value from stack
    inTestIfTrue, // Jump if current stack is true, without removing value from stack
    inSaveStackValue, // Pop the current stack value and save it in a secondary stack
    inLoadStackValue // Push back the stack value previously saved with inSaveStackValue

    );
const
  StackSize = 3000; // runtime stack size

Type
  IntObject = NativeInt;

  { Internal structure }
  TGenericProc = procedure of object;

  TScriptValue = Variant;
  PScriptValue = ^TScriptValue;
  TStackValue = Variant;
  PStackValue = ^TStackValue;

  { Internal structure }
  pSimplifiedCode = ^TSimplifiedCode;

  // elements order and size must be kept
  // this structure was fixed on 48 bytes size
  // Be Careful: when changing this structure, keep record size
  // a multiple of 8, to keep alignment in the variant part of the record

  /// <summary>
  ///  TSimplifiedCode is the atomic structure for a virtual machine pcode, in other words,
  ///  a compiled code is a list of TSimplified code records.
  /// </summary>

  TSimplifiedCode = record
    OpCode      : TInstruction;    // instruction op-code 1 byte
    vByte       : byte;            // first byte parameter 1 byte
    vSmallInt   : smallint;        // first smallint parameter 2 bytes
    vInteger    : integer;         // first integer parameter 4 bytes
    vInteger1   : integer;         // second integer parameter 4 bytes
    vString     : string;          // string parameter: element name 4 bytes
    vDebugInfo  : integer;         // debug information: source code position 4 bytes
    Next        : pSimplifiedCode; // Next instruction pointer 4 bytes
    Compute     : TGenericProc;    // instruction method pointer 8 bytes
    vString2    : string;          // string parameter: second element name 4 bytes
    Dummy       : integer;         // add unused 4 bytes in the record to keep record size a multiple of 8

    vPointer    : IntObject;
    vPointer2   : IntObject;     // 2 adicional pointers. 8 bytes on 32-bit, 16 bytes on 64-bit

    {$IFDEF NEXTGEN}
    vProc: TGenericProc;
    {$ENDIF}
    // Extra parameters
    case byte of
      0: ( vDouble:double );               // 1 double parameter - 8 bytes
      {$IFNDEF NEXTGEN}
      1: ( vProc:TGenericProc );           // 1 method parameter - 8 bytes - 16 bytes in 64-bit
      {$ENDIF}
      2: ( vInteger2,vInteger3:integer );  // 2 adicional integer parameters - 8 bytes.
      3: ( vSave1, vSave2: IntObject );    // parameters for saving this part of record. Must be IntObject to save vProc data correctly
      4: ( vInt64: Int64 );                // Int64 value (8 bytes)
  end;

  ///  <summary>
  ///  Internal structure.
  ///  </summary>
  pAddress = ^TAddress;

  TAddress = record
    Address: IntObject; // Line number that references label
    Code: pSimplifiedCode; // Pointer to instruction
    Next: pAddress; // Pointer to the next label reference
  end;

  ///  <summary>
  ///  Internal structure.
  ///  </summary>
  pLabelSpec = ^TLabelSpec;

  ///  <summary>
  ///  Internal structure. 标签
  ///  </summary>
  TLabelSpec = record
    Name: string; // label name
    Address: IntObject; // line number where label was defined
    FirstRef: pAddress; // pointer to the first label reference
    LastRef: pAddress; // pointer to the last label reference
    Next: pLabelSpec; // pointer to the next label
  end;

  ///  <summary>
  ///  Class reference types.
  ///  </summary>
  TClassReference = (crNone, crGetter, crSetter, crMethod);

  pAddrVector = ^TAddrVector;
  TAddrVector = array[0..0] of pSimplifiedCode;

const
  vtLongInt = 0100; { =0 }
  vtSingle = 0400; { =0 }
  vtByte = 0502; { =2 }
  vtDouble = 0912; { =12}
  vtReal = 1012; { =12 }
type
  TScriptClass = class of TxbScript;

  TxbScripts = class;
  TxbScript = class;
  TxbScriptInfo = class;
  TxbRoutinesInfo = class;
  TxbRoutineInfo = class;
  TxbVariablesInfo = class;
  TxbVariableInfo = class;
  TxbClasses = class;
  TxbClass = class;
  TxbMethods = class;
  TxbMethod = class;
  TxbObjects = class;
  TxbObject = class;

  ///  <summary>
  ///  Argument modifiers for use in atPascal class methods.
  ///  </summary>
  TxbArgumentModifier = (moNone, moConst, moVar);


  ///  <summary>
  ///  Internal structure.
  ///  </summary>
  TStackType = (
    stPendingOperators, // stack of pending operators (unstacking follows an order of precedence)
    stIdentifierList, // stack of identifiers of procedures and local variables
    stContext, // generic context stack for IF, FOR, WHILE, REPEAT and ARG_LIST
    stTempVariables );  // stack of local context temporary variables (ex.: for context of structures like With)

  ///  <summary>
  ///  Internal structure.
  ///  </summary>
  pStackElement = ^TStackElement;

  ///  <summary>
  ///  Internal structure.
  ///  </summary>
  TStackElement = record
    Previous: pStackElement;
    Element: TScriptValue;
    Index: integer;
  end;

  ///  <summary>
  ///  Internal use only.
  ///  </summary>
  TLoopStatement = (lsNone, lsFor, lsWhile, lsRepeat);

  ///  <remarks>
  ///  TxbBaseScripter component is the base class for all scripter components (TxbPascalScripter,
  ///  TxbScripter, etc.). TxbBaseScripter holds a collection of scripts in the Scripts property. You can add scripts,
  ///  compile and execute. For the scripts to access Delphi variables, classes, methods and properties, the scripter must
  ///  know that information. Some properties and methods and provides to registered such information, like DefineMethod,
  ///  DefineClass, etc. All registered information about available classes, methods and properties are kept in Classes
  ///  property collection.
  ///  </remarks>
  TxbBaseScripter = class(TComponent)
    private
      { scripter configuration}
      FScriptClass: TScriptClass;
      { aggregated parts}
      FScripts : TxbScripts;
      FCurrentScript: TxbScript; // the script that is using currently
      FDefaultInstances: TxbObjects;
      FClasses: TxbClasses;

      {#region-get}
      function GetSourceCode: TStrings;
      {#endregion-get}

      {#region-set}
      procedure SetSourceCode(const Value: TStrings);
      procedure SetClasses(const Value: TxbClasses);
      {#endregion-set}

    protected
      procedure DefineInternalClasses; virtual;
      function DefaultScriptClass: TScriptClass;virtual;

    protected
      property ScriptClass: TScriptClass read FScriptClass write FScriptClass;

      ///  <summary>
      ///  This property provides access to the SourceCode property of current script. See TatScript.SourceCode for more details.
      ///  Compilation is NOT called automatically when changing this property.
      ///  </summary>
      property SourceCode: TStrings read GetSourceCode write SetSourceCode;

    public

      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      function Execute(Input: Variant): Variant; overload; virtual;
      function Execute: TScriptValue; overload; virtual;

      ///  <remarks>
      ///  DefaultInstances holds a list of objects which are treated by scripter as default objects. A default object is
      ///  an object which methods and properties are directly accessible from all scripts without the need to prepend the method
      ///  name with the object name. For example, if an object MyObject has the method MyMethod, and it is added as a default
      ///  instance in DefaultInstances property, you can access the method directly the following way.
      ///  <code>
      ///  MyMethod;
      ///  </code>
      ///  You will rarely add objects directly to DefaultInstances, you should use UsesObject method instead.
      ///  </remarks>
      property DefaultInstances: TxbObjects read FDefaultInstances;

      ///  <remarks>
      ///  Classes property holds the list of all registered classes in scripter. For the scripter to know about a class and
      ///  the class to be accessible from all scripts, it must be in this collection. Each registered class is a TatClass object
      ///  which holds information about all methods and properties known by the scripter. When the scripter finds in script
      ///  a reference to an object of a specified class, it will look for that class in the Classes property to find all
      ///  the methods and properties that can be accessed from that object.
      ///  When you call a DefineClass method, an entry is added in this collection.
      ///  </remarks>
      property Classes: TxbClasses read FClasses write SetClasses;
  end;

  TxbVirtualMachine = class;

  ///  <summary>
  ///  TxbScripts holds a collection of TatScript objects belonging to the scripter. You can add, delete and iterate through the
  ///  available scripts.
  ///  </summary>
  TxbScripts = class(TCollection)
    private
      [weak] FBaseScripter : TxbBaseScripter;

    public
      constructor Create(AScripter: TxbBaseScripter);

      ///  <summary>
      ///  Adds a new script. You can then set source code, set language type, compile, execute the script, among other
      ///  actions.
      ///  </summary>
      function Add: TxbScript;

  end;

  ///  <remarks>
  ///  TatScript object contains all information related to a script in the scripter component. The scripts are kept
  ///  in the Scripts property which is a TatScripts collection.
  ///  TxbScript allows you to set the source code of the script using SourceCode property. Several methods are provided
  ///  for clearing the script (Clear), compiling (Compile), executing (Execute). Information about the compiled script
  ///  is also provided in the ScriptInfo property which holds the list of declared routines, global variables, etc.
  ///  Many other properties and methods related to a single script are available.
  ///  </remarks>
  TxbScript = class(TCollectionItem)
    private
      [weak] FBaseScripter : TxbBaseScripter;
      FParser : TxbSyntaxParser;
      FVirtualMachine: TxbVirtualMachine;

      { auxiliary code}
      FStack: array[TStackType] of pStackElement; // auxiliar stack
      FCodeLine: integer;
      FFirstLabelSpec: pLabelSpec;
      FLastLabelSpec: pLabelSpec;
      FCurrentRoutine: TxbRoutineInfo;

      { script source information }
      FSourceCode: TStrings;

      { compiled script runtime information }
      FFirstInstruction: pSimplifiedCode;  // primary instruction execute
      FLastInstruction: pSimplifiedCode;   // ultimate instruction execute
      FScriptInfo: TxbScriptInfo;  // script symbols information
      FCompiled: boolean;
      FSolvedAddressing: boolean;
      FCompiling: boolean;
      FRollbackSourceCode: boolean;
      FClearingScript: boolean;
      FDefInstances: TxbObjects;

      procedure InternalCompile(Silent: boolean);
      procedure SetCompiled(const Value: boolean);

    protected
      procedure CompileError(msg: string; debuginfo: integer);

      procedure SetSourceCode(Value: TStrings);
      procedure SourceCodeChange(Sender: TObject);

      procedure StackPush(StackType: TStackType; x: TScriptValue);
      procedure ClearLabelSpecs;
      procedure ClearStacks;

      function NewLabelSpec(Name: string): pLabelSpec;
      function FindLabelSpec(Name: string): pLabelSpec;
      function RegisterReference(Name: string): integer;
      procedure SolveReference(LabelSpec: pLabelSpec);
      procedure DefineReferenceAddress(Name: string);
      procedure SolveUndefinedReferences;
      function EmptyStack(StackType: TStackType): boolean;
      function StackPop(StackType: TStackType): TScriptValue;
      function DeclareVariable(AName: string; AArgIndex: integer = -1;
        AModifier: TxbArgumentModifier = moNone; AGlobal: boolean = false; ASourcePos: integer = -1)
        : TxbVariableInfo;
      function AppendInstruction(i: TInstruction): pSimplifiedCode;
      procedure DisposeCode(var Code: pSimplifiedCode);
      ///<summary>vProc.Data to relative class indexing</summary>
      procedure RelativeMethodRef(AMethod: TxbMethod; AInst: pSimplifiedCode);

      property CurrentRoutine: TxbRoutineInfo read FCurrentRoutine write FCurrentRoutine;
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

      ///  <remarks>
      ///  Use Clear method to clear all info about script compilation. When Clear method is called, the Compiled property is
      ///  set to false, all meta-information in ScriptInfo is erased, events attached to the script are removed, and other
      ///  clean up operations. Please note that Clear method only refers to compilation information. It doesn't clear the source
      ///  code, or other information like unit name, etc.
      ///  You cannot clear a script if it is running.
      ///  </remarks>
      procedure Clear; virtual;

      ///  <remarks>
      ///  Call Compile method to compile the current script source code. After a successful compilation is performed, the Compiled
      ///  property is set to true, and the compiled code is saved into a internal stream in memory for further execution.
      ///  SilentCompiled property is also set to true after a sucessful compilation.
      ///  If compilation fails, an exception is raised.
      ///  </remarks>
      procedure Compile; virtual;

      ///  <remarks>
      ///  Use GetRowColFromSource to retrieve the row and column values of a specified position in the source code.
      ///  The position is provided in APos variable, and is considered as if the source code was a single string, including
      ///  special characters like #13 and #10. The calculated row and column values are set in row and col parameters.
      ///  </remarks>
      procedure GetRowColFromSource(APos: integer; var row, col: integer);

      ///  <summary>
      ///  VirtualMachine property provides access to the script virtual machine. The virtual machine is the object which effectively
      ///  executes the compiled p-code. See TatVirtualMachine class for more info.
      ///  </summary>
      property VirtualMachine: TxbVirtualMachine read FVirtualMachine write FVirtualMachine;

      ///  <summary>
      ///  Use Scripter property to get access to the TxbBaseScripter component that owns the script.
      ///  </summary>
      property Scripter: TxbBaseScripter read FBaseScripter;

      ///  <summary>
      ///  Provides access to the parser object that parses the script source code.
      ///  </summary>
      property Parser: TxbSyntaxParser read FParser;

      ///  <remarks>
      ///  DefInstances holds a list of objects which are treated by script as default objects. A default object is
      ///  an object which methods and properties are directly accessible from the script without the need to prepend the method
      ///  name with the object name. For example, if an object MyObject has the method MyMethod, and it is added as a default
      ///  instance in DefaultInstances property, you can access the method directly the following way.
      ///  <code>
      ///  MyMethod;
      ///  </code>
      ///  You will rarely add objects directly to DefInstances, you should use TatScript.UsesObject method instead.
      ///  </remarks>
      property DefInstances: TxbObjects read FDefInstances;

      ///  <remarks>
      ///  SourceCode property holds the current source code for the script. Executing a script is as simple as setting the
      ///  SourceCode property and calling Execute method.
      ///  You cannot change SourceCode if the script is running. An exception will be raised and change will not be performed.
      ///  When you change the source code of the script, the script is cleared (see Clear method) and a new compilation is needed
      ///  before executing the script.
      ///  </remarks>
      property SourceCode: TStrings read FSourceCode write SetSourceCode;

      ///  <remarks>
      ///  ScriptInfo property gives access to the TatScriptInfo object which holds meta-information about the script. In ScriptInfo
      ///  you can check declared methods, variables, and other information about the script source code.
      ///  ScriptInfo is empty even after you set script source code, information is only available after a compilation.
      ///  Silent compilation (SilentCompile method) also fills information in ScriptInfo object.
      ///  </remarks>
      property ScriptInfo: TxbScriptInfo read FScriptInfo;

      ///  <remarks>
      ///  Use Compiled property to check if the script is already compiled and ready to execute. You can also set Compiled property
      ///  value to true or false. Setting to true compiles the script (if it's not already compiled), and setting to false clears
      ///  the script.
      ///  </remarks>
      property Compiled: boolean read FCompiled write SetCompiled;
  end;

  ///  <remarks>
  ///  TxbVirtualMachine object is used to execute a script compiled p-code. After the script is compiled, the compiled p-code
  ///  is saved in the memory and the script holds a reference to it. When the script is then executed, it uses the virtual machine
  ///  object to execute the code. The virtual machine then uses the compiled p-code in memory to run the script.
  ///  During script execution, several internal variables are updated by virtual machine. Specially when Delphi interoperation is
  ///  performed (accessing Delphi methods, properties, variables, etc.), the information kept by virtual machine is useful.
  ///  For example, the current object being accessed, the parameters passed to the current subroutine being executed, and so on.
  ///  Those information are useful for the methods that implement Delphi interoperability (those methods call the real Delphi
  ///  methods using the information provided by the virtual machine).
  ///  </remarks>
  TxbVirtualMachine = class
    private
      [weak] FScript: TxbScript;
      [weak] FBaseScripter: TxbBaseScripter;

      FRunning: boolean;  // 标记是否在运行
      FInitialized: boolean;
      FMainBegin: pSimplifiedCode;
      FNextInstruction: pSimplifiedCode;
      FCurrentInstruction: pSimplifiedCode;
      FPrepareInstruction: pSimplifiedCode; // Current runtime process (prepare instruction pointer)
      FComputeProcs: array [TInstruction] of TGenericProc;

      FInitialStackTop: integer;
      FProcStack: array [0 .. StackSize - 1] of TStackValue;
      FStackTop : Integer;  // indicate the top of stack
      FStackBase: integer; // índicate the base of stac
      FInputBase: integer; // índicate the first input index  of the argument
      FOutputBase: integer; // indicate the first output index of the argument
      FInputOffset: integer; // offset depend on input base
      FOutputOffset: integer; // offset depend on output base
      FCurrentInputArgBase: integer; // in-CallProc input argument index
      FCurrentArrayIndexBase: integer; // in-CallProc input array index base
      FCurrentOutputArgIndex: integer; // in-CallProc output argument index
      FFastSolving: boolean;

      { execution control }
      function ExecProcess(ParamCount: integer): integer;

      procedure SetRunning(const Value: boolean);
      procedure InitializeScript(ADoBeforeExecute: boolean);

      { Undefined _CallProc }
      procedure UndefinedMethodProc;
    public
      Constructor Create(AScript:TxbScript);
      Destructor Destroy;override;

      ///  <summary>
      ///  Clear method resets the virtual machine, clearing several internal variables, erasing the stack, among other
      ///  operations. You don't need to call Clear method, it's called internally by other methods of virtual machine.
      ///  </summary>
      procedure Clear;

      ///  <summary>
      ///  SolveAbsoluteAddressing method is used to translate address references to direct pointers, which are used by
      ///  p-code jump instructions. You don't need to call this method, it's automatically called by the virtual machine when needed.
      ///  </summary>
      procedure SolveAbsoluteAddressing; virtual;

      function Execute(Input: Variant): Variant; overload; virtual;
      function Execute: TScriptValue; overload; virtual;

      ///  <remarks>
      ///  Executes a specific routine (procedure) declared in the script associated with the virtual machine. If
      ///  program is still not compiled, compilation is performed automatically.
      ///  ALabel must contain the name of the routine to be execute, no matching case is performed.
      ///  if the routine doesn't exist, an exception is raised. You can previously check if the routine
      ///  exists using TatScriptInfo.RoutineByName method.
      ///  Input parameter can contain values that will be passed to the script routine. Input
      ///  can be a single Variant-type value, or an array of variant, or an array of const.
      ///  These values are passed to the script routine as parameters. If the routine parameter is declared
      ///  as a parameter by reference - for example, MyScriptFunction(var MyParam: integer); - then to have your
      ///  variable to be updated, you must use the array of const version of ExecuteSubRoutine, passing a variant of
      ///  type Variant.
      ///  </remarks>
      ///  <example>
      ///  <code>
      ///  //script
      ///  procedure MultiplyMyVar(var AValue: integer; AFactor: integer);
      ///  begin
      ///  AValue := AValue * AFactor;
      ///  end;
      ///
      ///  //Delphi
      ///  var
      ///  SomeVar: Variant; //MUST BE of Variant type
      ///  begin
      ///  SomeVar := 10;
      ///  atScripter1.ExecuteSubroutine('MultiplyMyVar', [SomeVar, 3]);
      ///  //At this point, SomeVar will have value of 30
      ///  end;
      ///  </code>
      ///  //The ExecuteSubRoutine function result contains value returned by the called function.
      ///  </example>
      function ExecuteSubroutine(ALabel: string; Input: Variant): Variant; overload;
      function ExecuteSubroutine(ALabel: string): TScriptValue; overload;
      function ExecuteSubroutine(ALabel: string; Input: array of const): TScriptValue; overload;

      ///  <summary>
      ///  Returns the first p-code instruction of the routine specified by ALabel.
      ///  </summary>
      function EntryPoint(ALabel: string): pSimplifiedCode;

      ///  <summary>
      ///  Returns the TatScript object which this virtual machine belongs to.
      ///  </summary>
      property Script: TxbScript read FScript;

      ///  <summary>
      ///  Returns the TatScripter component which this virtual machine belongs to.
      ///  </summary>
      property Scripter: TxbBaseScripter read FBaseScripter;

      ///  <summary>
      ///  Use Running property to check if the current script is being executed (regardless of the value of Paused property). You can
      ///  also set Running to true to execute a script, or set it to false to halt execution.
      ///  </summary>
      property Running: boolean read FRunning write SetRunning;

      ///  <remarks>
      ///  PrepareInstruction contains a reference to the last p-code Prepare instruction executed. When PrepareInstruction
      ///  is not nil, it means that the script is being executed. When script finishes its execution, the PrepareInstruction is
      ///  reverted to the state previously to script execution. Used by internal methods to verify if the current script is
      ///  being executed with reentrant calls (a script is being executed and a Delphi event fires causing the execution of
      ///  a subroutine of the same script.
      ///  You will rarely need to access this property.
      ///  </remarks>
      property PrepareInstruction: pSimplifiedCode read FPrepareInstruction;
  end;

  ///  <remarks>
  ///  TatScriptInfo object provides information about the current script, such as the declared routines (available in Routines property),
  ///  global variables (available in Globals property), among others. Information is only available if the current script was compiled (with Compile method) or silent compiled
  ///  (with SilentCompile method).
  ///  </remarks>
  TxbScriptInfo = class(TComponent)
    private
      FRoutines: TxbRoutinesInfo;
      FCodeSize: integer;
      FGlobals: TxbVariablesInfo;
      FMainRoutine: TxbRoutineInfo;

      FUnitName: string;
      procedure SetGlobals(const Value: TxbVariablesInfo);
      procedure SetRoutines(const Value: TxbRoutinesInfo);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      ///  <summary>
      ///  Retrieves the TatRoutineInfo object associated with the script routine specified by AName. If the routine doesn't exist
      ///  in the script, RoutineByName returns nil.
      ///  </summary>
      function RoutineByName(AName: string): TxbRoutineInfo;

    published
      property Routines: TxbRoutinesInfo read FRoutines write SetRoutines;
      property Globals: TxbVariablesInfo read FGlobals write SetGlobals;

      ///  <summary>
      ///  Retrives the number of p-code instructions generated after compilation. In other words, it's the size of the "executable".
      ///  </summary>
      property CodeSize: integer read FCodeSize;
      ///  <summary>
      ///  Retrieves the TatRoutineInfo obejct associated with the script main block (which is treated in a similar way as
      ///  any other routine in script).
      ///  </summary>
      property MainRoutine: TxbRoutineInfo read FMainRoutine write FMainRoutine;
  end;

  ///  <summary>
  ///  TatRoutinesInfo holds a list of TatRoutineInfo objects which contains information about the routines declared in script,
  ///  such as the routine name, parameters, etc.. The main routine is also present in the collection.
  ///  </summary>
  TxbRoutinesInfo = class(TOwnedCollection)
    private
      FScriptInfo: TxbScriptInfo;
      function GetItem(i:Integer): TxbRoutineInfo;
    public
      function IndexOf(AName: string): integer;

      property Items[i: integer]: TxbRoutineInfo read GetItem; default;
  end;

  ///  <summary>
  ///  TatRoutineInfo holds information about the routine declared in script. You can retrieve parameters, local variables,
  ///  visibility, and more.
  ///  </summary>
  TxbRoutineInfo = class(TCollectionItem)
    private
      FVariables: TxbVariablesInfo;
      FName: string;
      FDeclarationInstruction: pSimplifiedCode;
      FIsFunction: boolean;
      FArgCount: integer;
      FByRefArgMask: integer;
      FResultIndex: integer;
    public
      ///  <summary>
      ///  Returns a TatVariableInfo object related to the variable (or parameter) specified by the name AName. If the variable
      ///  or parameter doesn't exist, VariableByName returns nil.
      ///  </summary>
      function VariableByName(AName: string): TxbVariableInfo;

      ///  <remarks>
      ///  Returns the number of local variables declared in the routine. This function doesn't take into account the routine
      ///  parameters, only effective local variables. Note that the result variable (in case of a function routine) is also
      ///  considered as a local variable.
      ///  </remarks>
      function LocalVarCount: integer;

      ///  <summary>
      ///  Do not call DeclareVariable directly. It's used by internal parsers/compilers.
      ///  </summary>
      function DeclareVariable(AName: string; ASourcePos: integer; AArgIndex: integer = -1;
        AModifier: TxbArgumentModifier = moNone): TxbVariableInfo;
      ///  <remarks>
      ///  DeclarationInstruction contains the Prepare p-code instruction related to the routine. It's the first p-code to be
      ///  executed and allocates space in stack for local variables and parameters.
      ///  You will rarely need to use this property.
      ///  </remarks>
      property DeclarationInstruction: pSimplifiedCode read FDeclarationInstruction
        write FDeclarationInstruction;

    published
      ///  <summary>
      ///  Returns true if the routine returns a result value. In other words, IsFunction is true when the routine is a function,
      ///  false if the routine is a procedure/sub.
      ///  </summary>
      property IsFunction: boolean read FIsFunction write FIsFunction;

      ///  <summary>
      ///  ArgCount returns the number of input parameters declared in the routine. Note that it doesn't take local variables into account,
      ///  just input parameters.
      ///  </summary>
      property ArgCount: integer read FArgCount write FArgCount;

      ///  <remarks>
      ///  Contains the index of the variable (in Variables collection) that corresponds to the function result. When a
      ///  function is declared, the function result value is declared as a local variable. Its index in the Variables collection
      ///  is provided by this property.
      ///  </remarks>
      property ResultIndex: integer read FResultIndex write FResultIndex;
  end;

  ///  <remarks>
  ///  TxbVariablesInfo is a collection of TatVariableInfo objects that hold information about variables declared in script.
  ///  If the TatVariablesInfo relates to a routine, then it will contain all declared local variables, the input parameters
  ///  of the routine, and the variable result value (if it's a function). If TatVariablesInfo relates to the script as a whole,
  ///  it will contain the global variables declared in script.
  ///  </remarks>
  TxbVariablesInfo = class(TOwnedCollection)
    private
      function GetItem(i: Integer): TxbVariableInfo;
    public
      ///  <summary>
      ///  Adds a new variable in the collection. Do not call this directly, it will not effectively change source code to declare
      ///  a new variable. This is used by the compiler to add information in the collection.
      ///  </summary>
      function Add: TxbVariableInfo;

      ///  <summary>
      ///  Returns the index of the variable which name is specified by AName. If no variable is found, IndexOf returns -1.
      ///  </summary>
      function IndexOf(AName: string): integer;

      ///  <summary>
      ///  Returns the TatVariableInfo object which corresponds to the variable which name is specified by Name. If no variable
      ///  is found, FindByName returns nil.
      ///  </summary>
      function FindByName(AName: string): TxbVariableInfo;

      ///  <summary>
      ///  Provides indexed access to the TatVariableInfo objects in the collection.
      ///  </summary>
      property Items[i: integer]: TxbVariableInfo read GetItem; default;

  end;

  ///  <remarks>
  ///  Indicates if the global variable was declared as private or public in the script. When the script is registered
  ///  as a script library, its global variables are available from other scripts, except when it is declared as private.
  ///  vvPublic - Global variable was declared as public (visible to other scripts)
  ///  vvPrivate - Global variable was declared as private (visible only from script itself)
  ///  </remarks>
  TxbVariableVisibility = (vvPublic, vvPrivate);

  ///  <remarks>
  ///  Holds information about a variable declared in the script. Although the word "variable" here, it's important to
  ///  note that not only local and global variables, but also routine input parameters and function result value also have
  ///  an associated TatVariableInfo object.
  ///  </remarks>
  TxbVariableInfo = class(TCollectionItem)
     private
      FVarName: string;
      FDeclarationSourcePos: integer;
      FArgIndex: integer; // indica que a variável é um argumento de subrotina
      FModifier: TxbArgumentModifier; // indica o modificador do argumento de subrotina
//      FDeclaredClass: TatClass; // classe declarada para a variável
      FGlobal: boolean;
      FTypeDecl: string;
      // string contendo informações sobre o tipo da variável ou argumento de subrotina
      FSystemType: integer;
      FVisibility: TxbVariableVisibility;
      function GetValue: TScriptValue;
      procedure SetValue(AValue: TScriptValue);
      function Script: TxbScript;
      function routine: TxbRoutineInfo;
      function GetDeclaredClassName: string;
      procedure SetDeclaredClassName(const Value: string);
      procedure PullSystemType;
    public
      constructor Create(ACollection: TCollection); override;
      ///  <summary>
      ///  Used by compiler. Do not use this function.
      ///  </summary>
      procedure SetTypeFromString(ATypeStr: string);
      ///  <remarks>
      ///  VarIndex is used by the compiler to know the position of the variable in the stack, relative to the current
      ///  stack base. With regarding to the stack, local variables are located in a positive index from stack base,
      ///  and input parameters are with negative values.
      ///  You don't need to use this method.
      ///  </remarks>
      function VarIndex: integer;

      ///  <summary>
      ///  Indicates if the variable is a global variable or not.
      ///  </summary>
      property Global: boolean read FGlobal write FGlobal;

      ///  <remarks>
      ///  Contains the name of the declared type of variable. For example, if the variable was declared.
      ///  <code>
      ///  var MyVar: integer;
      ///  </code>
      ///  TypeDecl will contain 'integer'.
      ///  </remarks>
      property TypeDecl: string read FTypeDecl write FTypeDecl;
  end;

  ///  <remarks>
  ///  TatClasses holds a collection of TatClass objects that contain information abuot all Delphi classes registered in scripter system,
  ///  including its methods and properties. When a Delphi class is registered, scripter knows about it and can interact with
  ///  the class accordingly.
  ///  </remarks>
  TxbClasses = class(TCollection)
    private
      FScripter: TxbBaseScripter;
      FRegLevel: Integer;
      FMethods: TxbMethods;

      {getter}
      function GetItem(i: integer): TxbClass;

    public
      Constructor Create(AScripter: TxbBaseScripter);


      property Items[i: integer]: TxbClass read GetItem; default;
  end;

  ///  <summary>
  ///  Holds information about a Delphi class registered in scripter. A TatClass object holds information about the Delphi registered
  ///  class, its methods, properties, and provides methods for registering new methods and properties in the class.
  ///  </summary>
  TxbClass = class(TCollectionItem)
    private
      FName: string;
      FMethods: TxbMethods;

      {setter}
      procedure SetMethods(const Value: TxbMethods);
    public
      ///  <summary>
      ///  Holds the list of methods declared in the class.
      ///  </summary>
      property Methods: TxbMethods read FMethods write SetMethods;
  end;

  ///  <summary>
  ///  Holds the list of TatMethod objects which are the registered method for the class.
  ///  </summary>
  TxbMethods = class(TCollection)
    private
      FClass: TxbClass;
      function GetItem(i: integer): TxbMethod;
    public
      Constructor Create(AClass: TxbClass);
      property Items[i: integer]: TxbMethod read GetItem; default;

      function IndexOf(AName: string): integer;

      function MethodByName(AName: string): TxbMethod;

      ///  <summary>
      ///  Provides a reference to the TatClass object in which this method is registered.
      ///  </summary>
      property atClass: TxbClass read FClass;

  end;

  ///  <summary>
  ///  Contains information about a method registered in scripter system. This information is used by the scripter to know
  ///  if a class contains a specified method, and how this method should be accessed.
  ///  </summary>
  TxbMethod = class(TCollectionItem)
    private
      FName: string;
      FProc: TGenericProc;
    public


      ///  <summary>
      ///  Contains the method wrapper which will be called by the script when the method referenced. This is the method that
      ///  should actually execute real Delphi method.
      ///  </summary>
      property Proc: TGenericProc read FProc write FProc;
  end;

  ///  <summary>
  ///  Holds a list of object instances that are being accessible from scripts in a "default" way (methods and properties
  ///  are directly accessed).
  ///  </summary>
  TxbObjects = class(TCollection)
    private
      FClasses: TxbClasses;
      function GetItem(i: integer): TxbObject;
    public
      constructor Create(AClasses: TxbClasses);

      ///  <summary>
      ///  Provides indexed-access to the TatObject objects in the collection.
      ///  </summary>
      property Items[i: integer]: TxbObject read GetItem; default;
  end;

  ///  <summary>
  ///  Holds information about an object instance that is being accessible from scripts in a "default" way (methods and properties
  ///  are directly accessed).
  ///  </summary>
  TxbObject = class(TCollectionItem)
    private
      FName: string;
      FInstance: TObject;
      FatClass: TxbClass;
      procedure SetxbClass(const Value: TxbClass);
    public
      procedure Assign(ASource: TPersistent); override;

      ///  <summary>
      ///  Holds the name by which the object can be accessed from script.
      ///  </summary>
      property Name: string read FName write FName;

      ///  <summary>
      ///  Holds the instance of the object that can be accessed from script.
      ///  </summary>
      property Instance: TObject read FInstance write FInstance;

      ///  <summary>
      ///  Contains a reference to the TatClass object holding information about the class of the object, its methods and
      ///  properties than can be accessed in a default way.
      ///  </summary>
      property atClass: TxbClass read FatClass write SetxbClass;
  end;

var
  NullValue: TScriptValue;

implementation

{ TxbBaseScripter }

constructor TxbBaseScripter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScripts := TxbScripts.Create(self);

  FCurrentScript := FScripts.Add;
  {classes registered for script}
  FClasses := TxbClasses.Create(self);
  {instance of registered classes}
  FDefaultInstances := TxbObjects.Create(FClasses);

end;

function TxbBaseScripter.DefaultScriptClass: TScriptClass;
begin
  result := nil;
end;

procedure TxbBaseScripter.DefineInternalClasses;
begin

end;

destructor TxbBaseScripter.Destroy;
begin
  FScripts.DisposeOf;
  FDefaultInstances.Free;
  FClasses.Free;
  inherited;
end;

function TxbBaseScripter.Execute(Input: Variant): Variant;
begin
  if Not Assigned(FCurrentScript) then
    raise Exception.Create('Default script is not available');
  if Not Assigned(FCurrentScript.VirtualMachine) then
    raise Exception.Create('Virtual Machine class not specified.');
  Result := FCurrentScript.VirtualMachine.Execute(Input);
end;

function TxbBaseScripter.Execute: TScriptValue;
begin
  Result := Execute(null);
end;

function TxbBaseScripter.GetSourceCode: TStrings;
begin
  if Assigned(FCurrentScript) then
    Result := FCurrentScript.SourceCode
end;

procedure TxbBaseScripter.SetClasses(const Value: TxbClasses);
begin
  FClasses.Assign(Value);
end;

procedure TxbBaseScripter.SetSourceCode(const Value: TStrings);
begin

end;

{ TxbScripts }

function TxbScripts.Add: TxbScript;
begin
  if (FBaseScripter <> nil) and (FBaseScripter.DefaultScriptClass <> nil) then
    Result := FBaseScripter.DefaultScriptClass.Create(self)
  else
    Result := TxbScript(inherited Add);
end;

constructor TxbScripts.Create(AScripter: TxbBaseScripter);
begin
  FBaseScripter := AScripter;

  if not Assigned(AScripter.FScriptClass) then
    AScripter.FScriptClass := TxbScript;
  inherited Create(AScripter.FScriptClass);
end;

{ TxbVirtualMachine }

procedure TxbVirtualMachine.Clear;
begin
  FRunning := false;
  FInitialized := false;
end;

constructor TxbVirtualMachine.Create(AScript: TxbScript);
begin
  FFastSolving := True;
  FScript := AScript;
  FBaseScripter := FScript.Scripter;

end;

destructor TxbVirtualMachine.Destroy;
begin

  inherited;
end;

function TxbVirtualMachine.EntryPoint(ALabel: string): pSimplifiedCode;
begin
  { search instructions to find routine name
    (*** would be better if search by RoutineInfo -> future ***) }
  result := Script.FFirstInstruction;
  { search subroutine start instruction (inPrepare) }
  while Assigned(result) and ((result^.OpCode <> inPrepare) or
    (CompareText(result^.vString , ALabel) <> 0)) do
     result := result^.Next;

  if not Assigned(result) then
    raise Exception.Create(Format('Subroutine ''%s'' does not exist in script', [ALabel]));
end;

function TxbVirtualMachine.ExecProcess(ParamCount: integer): integer;
var
  OldStackTop: Integer;
  OldStackBase: Integer;
  OldInputBase: Integer;
  OldOutputBase: Integer;
  OldInputOffset: Integer;
  OldOutputOffset: Integer;
  OldPrepareInstruction: pSimplifiedCode;
  OldCurrentInputArgBase: integer;
  OldCurrentArrayIndexBase: integer;
  OldCurrentOutputArgIndex: integer;
  ReturnInstruction: pSimplifiedCode;
begin
  Result := -1;
  try
    OldStackTop := FStackTop;
    OldStackBase := FStackBase;
    OldInputBase := FInputBase;
    OldOutputBase := FOutputBase;
    OldInputOffset := FInputOffset;
    OldOutputOffset := FOutputOffset;
    OldPrepareInstruction := FPrepareInstruction;
    OldCurrentInputArgBase := FCurrentInputArgBase;
    OldCurrentArrayIndexBase := FCurrentArrayIndexBase;
    OldCurrentOutputArgIndex := FCurrentOutputArgIndex;

    ReturnInstruction := FNextInstruction;

    FInputBase := FStackTop - ParamCount;
    FInputOffset := 0;
    FStackBase := FStackTop;
    try
      while FCurrentInstruction <> nil do
      begin
        FNextInstruction := FCurrentInstruction^.Next;
        FCurrentInstruction.Compute;
        FCurrentInstruction := FNextInstruction;
      end;
    finally
      FNextInstruction := ReturnInstruction;
      Result := FStackTop - OldStackTop;
      FPrepareInstruction := OldPrepareInstruction;
      FStackTop := OldStackTop;
      FStackBase := OldStackBase;
      FInputBase := OldInputBase;
      FOutputBase := OldOutputBase;
      FInputOffset := OldInputOffset;
      FOutputOffset := OldOutputOffset;
      FCurrentInputArgBase := OldCurrentInputArgBase;
      FCurrentArrayIndexBase := OldCurrentArrayIndexBase;
      FCurrentOutputArgIndex := OldCurrentOutputArgIndex;
    end;
  except on e: Exception do
    raise ;
  end;
end;

function TxbVirtualMachine.Execute: TScriptValue;
begin
  Result := Execute(null);
end;

function TxbVirtualMachine.ExecuteSubroutine(ALabel: string;
  Input: Variant): Variant;
var
  oldNextInstruction: pSimplifiedCode;
  oldcurrent: TxbScript;
  c: Integer;
  _StackTop: integer;
  InputParamCount : Integer;
  dataSize: integer;
  outputParamCount: integer;
begin
  with Script do
  begin
    if not FCompiled then
      Compile;

    if not FSolvedAddressing then
      SolveAbsoluteAddressing;

    if (ALabel > '') and (ScriptInfo.RoutineByName(ALabel) = nil) then
      raise Exception.Create(Format('Subroutine ''%s'' does not exist in script', [ALabel]));

    oldNextInstruction := FNextInstruction;

    FRunning := True;
    oldcurrent := self.Script.Scripter.FCurrentScript;
    Self.Script.Scripter.FCurrentScript := Self.Script;
    if Not Assigned(PrepareInstruction) then  // not reentrant call
    begin
      { runtime stack initialization }
//      if InitializeStack then
        for c := ScriptInfo.Globals.Count to StackSize - 1 do
          FProcStack[c] := NullValue;

        if not FInitialized then
        begin
          _StackTop := 0;
          FStackTop := 0;
          InitializeScript(false);
        end else
        begin
          _StackTop := FInitialStackTop;
          FStackTop := FInitialStackTop;
          FNextInstruction := FMainBegin;
        end;
    end else
    begin
       _StackTop := FStackTop;
       FStackTop := FOutputBase;
    end;
    if ALabel > '' then
      FCurrentInstruction := EntryPoint(ALabel)
    else
      FCurrentInstruction := FMainBegin;
    if not Assigned(FCurrentInstruction) then
      exit;
    if not Assigned(FFirstInstruction) then
    begin
      result := null;
      exit;
    end;
    if FCurrentInstruction^.OpCode <> inPrepare then
      raise Exception.Create('Invalid subroutine call. Entry point should be a "Prepare" instruction');

    try
      { load input arguments }
      if not VarIsNull(Input) then
      begin
        if VarIsArray(Input) then
        begin
          InputParamCount := VarArrayHighBound(Input, 1) + 1;
          for c := 0 to InputParamCount - 1 do
            FProcStack[FStackTop + c] := Input[c];
        end
        else
        begin
          InputParamCount := 1;
          FProcStack[FStackTop] := Input;
        end;
        Inc(FStackTop, InputParamCount)
      end else
        InputParamCount := 0;
      { gets the local variable data size from inPrepare instruction }
      dataSize := FCurrentInstruction^.vInteger;
      { runs the subroutine }
      outputParamCount := ExecProcess(InputParamCount) - dataSize;
      { returns output arguments }
      if outputParamCount = 0 then
        Result := null
      else if outputParamCount = 1 then
        Result := FProcStack[FStackTop + dataSize]
      else
      begin
        Result := VarArrayCreate([0,outputParamCount - 1], varVariant);
        for c := 0 to outputParamCount - 1 do
          Result[c] := FProcStack[FStackTop + dataSize + c];
      end;
      dec(FStackTop, InputParamCount);
    finally
      if not Assigned(PrepareInstruction) then  // not reentrant call(非递归调用)
      begin
        FNextInstruction := nil;
        for c := StackSize - 1 downto ScriptInfo.Globals.Count do
            VarClear(FProcStack[c]);
        FRunning := false;
      end else
      begin
        { retrieve instruction state for reentrant calls }
        FNextInstruction := oldNextInstruction;
        { restore the stack-top of the process that was interrupted by this call }
        FStackTop := _StackTop;
      end;
      self.Script.Scripter.FCurrentScript := oldcurrent;
    end;
  end;
end;

function TxbVirtualMachine.ExecuteSubroutine(ALabel: string): TScriptValue;
begin

end;

function TxbVirtualMachine.ExecuteSubroutine(ALabel: string;
  Input: array of const): TScriptValue;
begin

end;

procedure TxbVirtualMachine.InitializeScript(ADoBeforeExecute: boolean);
begin
  if FInitialized then
   begin
      exit;
   end else
   begin
     with Script do
     begin
       if not FCompiled then
         exit;

       if Assigned(PrepareInstruction) then
         exit;

       if not FSolvedAddressing then
         SolveAbsoluteAddressing;

       FNextInstruction := nil;
       { *** script initialization *** }
       { execute all different instructions at the beginning of script, until the first iPrepare instruction }
       while (FCurrentInstruction <> nil) and (FCurrentInstruction^.OpCode <> inPrepare) do
       begin
         FNextInstruction := FCurrentInstruction^.Next;
         FCurrentInstruction.Compute;
         FCurrentInstruction := FNextInstruction;
       end;
       FMainBegin := FCurrentInstruction^.Next;
       FInitialStackTop := FStackTop;
       FInitialized := true;
     end;
   end;
end;

function TxbVirtualMachine.Execute(Input: Variant): Variant;
begin
  Result := ExecuteSubroutine('', Input);
end;

procedure TxbVirtualMachine.SetRunning(const Value: boolean);
begin
  if FRunning <> Value then
  begin
    if Value then
      Execute;
  end;
end;

procedure TxbVirtualMachine.SolveAbsoluteAddressing;
var
  Cur: pSimplifiedCode;
  c: integer;
  AddressTable: pAddrVector;
  space: integer;
  classidx: integer;
  itemidx: integer;
  AClass: TxbClass;
  AObject: TObject;
  TempFastSolve: boolean;
begin
  if Script.FSolvedAddressing then
    Exit;

  { this procedure executes the following adjustments:
    a) fix relative addresses in instructions
    b) adjust address of methods in instructions, based on OpCode
    c) fix address of methods related to getters, setters and calls for objects or classes }
  with Scripter, Script do
  begin
    if not FCompiled then
       Compile;
    space := (FCodeLine + 1);
    GetMem(AddressTable,sizeof(pSimplifiedCode) * space);
    try
      Cur := FFirstInstruction;
      while Cur <> nil do
      begin
        with Cur^ do
        begin
          Compute := FComputeProcs[OpCode];
          vPointer := 0;

          case OpCode of
            inJump, inCall, inJumpIfFalse, inJumpIfTrue, inVarTest, inVarTestInteger,
              inVarTestDouble, inVarTestString, inVarTestVar, inTestIfFalse, inTestIfTrue:

              if vInteger < space then
                 vPointer := IntObject(AddressTable^[vInteger]);

            inTryFinally, inTryExcept:
              begin
                if vInteger < space then
                  vPointer := IntObject(AddressTable^[vInteger]);

                if vInteger2 < space then
                  vPointer2 := IntObject(AddressTable^[vInteger2]);
              end;

            inCallProc:
              begin
                { solve default instance reference }
                TempFastSolve := false;
                if vInteger = -1 then
                  AObject := nil
                else
                begin
                  { if vInteger is positive, then it's an index of scripter.FDefaultInstances.
                    if vInteger is negative, then it's an index of script.FDefInstance. But the index is shifted by 2
                    to avoid -0 indexes. So, after changing the signal, we must subtract 2 so that we find the original index.
                    This sum + 2 is needed to avoid 0 and -1, which are special codes (0 is the 0-index for FDefaultinstances }
                  if vInteger >= 0 then
                    AObject := FDefaultInstances[vInteger].Instance
                  else
                    AObject := FDefInstances[(-vInteger) - 2].Instance;

                  vPointer := IntObject(AObject);

                  classidx := integer(TMethod(vProc).Data) shr 17;
                  itemidx := (integer(TMethod(vProc).Data) shr 2) and $7FFF;

                  { Special case for classidx: if the class to be solved is TatInternalObjectXXXX class,
                  then we must do something special. First, the class name is not guaranteed to be the same.
                  Second, if we're using that class, it means that it's the internal class of this script
                  default instance (FIntInstance), so we can force it to that class.
                  Even if the class name didn't change, we should force the class to be the default instance
                  in the script, because for forms and classes, we don't want to use the class of original
                  script, but instead, we want to use the one of the copied script }
                  if (pos('TATINTERNALOBJECT', UpperCase(vString2)) = 1) and
                  (Length(vString2) > 17 { Length('TatInternalObject') } ) then
                  begin
//                    classidx := FIntInstance.atClass.Index;
//                    TempFastSolve := True;
                  end;

                  { solve vProc relative reference }
                  case TClassReference(Integer(TMethod(vProc).Data) and 3) of
                    crGetter: ;
                    crSetter: ;
                    crMethod:
                      if TMethod(vProc).Data = Pointer(crMethod) then
                        vProc := UndefinedMethodProc
                      else if FFastSolving or TempFastSolve then
                        vProc := FClasses[classidx].Methods[itemidx].Proc
                      else
                        vProc := nil
                  end;

                end;
              end;

            inCallClassProc:
              begin

              end;
            inCastClass, inPushClass:
              begin

              end;
          end;
        end;
        Cur := Cur^.Next;
      end;
    finally
      FreeMem(AddressTable);
    end;
    FSolvedAddressing := True;
    FFastSolving := True;
  end;
end;

procedure TxbVirtualMachine.UndefinedMethodProc;
begin
  // no implementation
end;

{ TxbScript }

function TxbScript.AppendInstruction(i: TInstruction): pSimplifiedCode;
var
  prev: pSimplifiedCode;
begin
  Inc(FCodeLine);
  prev := FLastInstruction;
  New(FLastInstruction);
  Result := FLastInstruction;
  FillChar(Result^, sizeof(TSimplifiedCode), 0);
  Result^.OpCode := i;
  if not Assigned(prev) then
    FFirstInstruction := FLastInstruction
  else
    prev^.Next := FLastInstruction;
end;

procedure TxbScript.Clear;
var
  st: TStackType;
  c: integer;
begin
  if VirtualMachine = nil then
    raise Exception.Create('Virtual Machine class not specified.');

  { Avoid stack overflow }
  if not FClearingScript then
   begin
     FClearingScript := true;
     try
       if VirtualMachine.Running then
         raise Exception.Create('Cannot clear script while script is running.');

       FVirtualMachine.Clear;

       ClearLabelSpecs;

       ClearStacks;

       DisposeCode(FFirstInstruction);
       FLastInstruction := nil;

      { inicializa os stacks internos }
       for st := low(TStackType) to High(TStackType) do
        FStack[st] := nil;

//      { inicializa os especificadores de rótulos }
       FFirstLabelSpec := nil;
       FLastLabelSpec := nil;
//
      { inicialização de propriedades }
       FCodeLine := -1;
       FCompiled := false;
//       FSilentCompiled := false;
       FSolvedAddressing := false;
//       FExpandedDebugInfo := false;
       FCurrentRoutine := nil;


     finally
       FClearingScript := false;
     end;
   end;
end;

procedure TxbScript.ClearLabelSpecs;
begin

end;

procedure TxbScript.ClearStacks;
var
  st : TStackType;
begin
  for st := Low(TStackType) to High(TStackType) do
    while not EmptyStack(st) do
     StackPop(st);
end;

procedure TxbScript.Compile;
begin
  InternalCompile(false);
end;

procedure TxbScript.CompileError(msg: string; debuginfo: integer);
var
  row, col: integer;
begin
  GetRowColFromSource(debuginfo, row, col);
  raise Exception.CreateFmt('%s: @row:%d, @col:%d',[msg,row,col]);
end;

constructor TxbScript.Create(ACollection: TCollection);
begin
  FBaseScripter := TxbScripts(ACollection).FBaseScripter;
  inherited Create(ACollection);

  FParser := TxbSyntaxParser.Create(nil);

  FVirtualMachine := TxbVirtualMachine.Create(Self);

  FSourceCode := TStringList.Create;
  TStringList(FSourceCode).OnChange := SourceCodeChange;

  FRollbackSourceCode := false;
  FDefInstances := TxbObjects.Create(Scripter.FClasses);

end;

function TxbScript.DeclareVariable(AName: string; AArgIndex: integer;
  AModifier: TxbArgumentModifier; AGlobal: boolean;
  ASourcePos: integer): TxbVariableInfo;
begin
  if ASourcePos = -1 then
    ASourcePos := FParser.ScanningInputPos;
  if AGlobal then
  begin
    if Assigned(ScriptInfo.Globals.FindByName(AName)) then
      CompileError(Format('Redeclaration of global identifier ''%s''', [AName]),
        FParser.ScanningInputPos);
    Result := ScriptInfo.Globals.Add;
    with Result do
    begin
      FVarName := AName;
      FArgIndex := AArgIndex;
      FModifier := AModifier;
      FGlobal := True;
      FDeclarationSourcePos := ASourcePos
    end;
  end else
  begin
    if Assigned(CurrentRoutine.VariableByName(AName)) then
      CompileError(Format('Redeclaration of identifier ''%s''', [AName]), ASourcePos);
    Result := CurrentRoutine.DeclareVariable(AName, ASourcePos, AArgIndex, AModifier);
  end;
end;

procedure TxbScript.DefineReferenceAddress(Name: string);
var
  LabelSpec: pLabelSpec;
begin
  LabelSpec := FindLabelSpec(name);
  if LabelSpec = nil then
    LabelSpec := NewLabelSpec(name);
  LabelSpec^.Address := FCodeLine;
  if LabelSpec^.FirstRef <> nil then
    SolveReference(LabelSpec);
end;

destructor TxbScript.Destroy;
begin
  FParser.DisposeOf;
  FVirtualMachine.DisposeOf;
  FSourceCode.DisposeOf;
  FDefInstances.DisposeOf;
  inherited;
end;

procedure TxbScript.DisposeCode(var Code: pSimplifiedCode);
var
  aux: pSimplifiedCode;
begin
  { libera o programa previamente alocado }
  while Code <> nil do
  begin
    aux := Code;
    Code := Code^.Next;
    Dispose(aux);
  end;
end;

function TxbScript.EmptyStack(StackType: TStackType): boolean;
begin
  Result := (FStack[StackType] = nil);
end;

function TxbScript.FindLabelSpec(Name: string): pLabelSpec;
begin
  Result := FFirstLabelSpec;
  while (Result <> nil) and (UpperCase(Result^.Name) <> UpperCase(Name)) do
    Result := Result^.Next;
end;

procedure TxbScript.GetRowColFromSource(APos: integer; var row, col: integer);
begin
  GetRowColFromStrings(FParser.Strings, APos, row, col);
end;

procedure TxbScript.InternalCompile(Silent: boolean);
var
  row: integer;
  col: integer;
begin
  if not Assigned(VirtualMachine) then
    raise Exception.Create('Virtual Machine class not specified.');

  FCompiling := true;
  try
    if VirtualMachine.Running then
      raise Exception.Create('Cannot compile script while script is running.');

    Clear;

    with FParser, CheckLanguage do

      if Result <> srCorrect then
      begin
        GetRowColFromSource(MaxInputPos - 1, row, col);
        raise Exception.Createfmt('Syntax Error @Row:%d, @Col:%d',[row,col]);
      end;

    FParser.ScanSyntaxTree;

    SolveUndefinedReferences;
    FCompiled := not Silent;

    FScriptInfo.FCodeSize := FCodeLine + 1;
  finally
    FCompiling := false;
  end;
end;

function TxbScript.NewLabelSpec(Name: string): pLabelSpec;
begin
  New(Result);
  Result^.Name := Name;
  Result^.Address := -1;
  Result^.FirstRef := nil;
  Result^.LastRef := nil;
  Result^.Next := nil;
  if FFirstLabelSpec = nil then
    FFirstLabelSpec := Result
  else
    FLastLabelSpec^.Next := Result;
  FLastLabelSpec := Result;
end;

function TxbScript.RegisterReference(Name: string): integer;
var
  LabelSpec: pLabelSpec;
  LastRef: pAddress;
begin
  LabelSpec := FindLabelSpec(name);
  if LabelSpec = nil then
    LabelSpec := NewLabelSpec(name);
  if LabelSpec^.Address > -1 then
  begin
    Result := LabelSpec^.Address + 1;
    FLastInstruction.vInteger := Result;
  end else
  begin
    New(LastRef);
    LastRef^.Next := nil;
    LastRef^.Address := FCodeLine;
    LastRef^.Code := FLastInstruction;
    if LabelSpec^.FirstRef = nil then
      LabelSpec^.Firstref := LastRef
    else
      LabelSpec^.LastRef^.Next := LastRef;
    LabelSpec^.LastRef := LastRef;
    Result := 0;
  end;
end;

procedure TxbScript.SolveReference(LabelSpec: pLabelSpec);
var
  Ref: pAddress;
begin
  with LabelSpec^ do
  begin
    while FirstRef <> nil do
    begin
      FirstRef^.Code.vInteger := Address + 1;
      Ref := FirstRef;
      FirstRef := FirstRef^.Next;
      Dispose(Ref);
    end;
    LastRef := nil;
  end;
end;

procedure TxbScript.SolveUndefinedReferences;
var
  LabelSpec: pLabelSpec;
begin
  LabelSpec := FFirstLabelSpec;
  while LabelSpec <> nil do
  begin
    with LabelSpec^ do
     if FirstRef <> nil then
     begin
       CompileError(Format('Unknown method on subroutine: ''%s''',[name]),
         FirstRef^.Code^.vDebugInfo);
     end;
     LabelSpec := LabelSpec^.Next;
  end;
end;

procedure TxbScript.RelativeMethodRef(AMethod: TxbMethod;
  AInst: pSimplifiedCode);
begin
   { Use only the field vProc.Data to relative class indexing, vProc.Code is ignored
    vProc will entirelly replaced by SolveAbsoluteAddressing, after compiling or
    after loading code from file }
   if Assigned(AMethod) then
   begin
     TMethod(AInst^.vProc).Code := nil;
     TMethod(AInst^.vProc).Data := Pointer(ord(crMethod) + AMethod.Index shl 2 +
       TxbMethods(AMethod.Collection).FClass.Index shl 17);
     {Record Class Name}
     AInst^.vString2 := TxbMethods(AMethod.Collection).FClass.FName;
   end else
   begin
     { Indicate, into field vProc.Data that the reference is for undefined method }
      TMethod(AInst^.vProc).Code := nil;
      TMethod(AInst^.vProc).Data := pointer(crMethod);
   end;
end;

procedure TxbScript.SetCompiled(const Value: boolean);
begin
  if FCompiled <> Value then
  begin
    if Value then
      Compile
    else
      Clear;
  end;
end;

procedure TxbScript.SetSourceCode(Value: TStrings);
begin
  FSourceCode.Assign(Value);
end;

procedure TxbScript.SourceCodeChange(Sender: TObject);
begin
  if not FRollbackSourceCode then
    begin
      if (VirtualMachine <> nil) and VirtualMachine.Running then
        begin
          FRollbackSourceCode := true;
          FSourceCode.Assign(FParser.Strings);
          FRollbackSourceCode := false;
          raise Exception.Create('Cannot change script source code while script is running.');
        end else
        begin
          FParser.Strings.Assign(FSourceCode);
          clear;
        end;
    end;
end;

function TxbScript.StackPop(StackType: TStackType): TScriptValue;
var
  aux: pStackElement;
begin
  if FStack[StackType] = nil then
    raise Exception.Create('Empty stack when trying to pop element ' + GetEnumName(TypeInfo(TStackType),
      ord(StackType)));
  aux := FStack[StackType].Previous;
  Result := FStack[StackType]^.Element;
  Dispose(FStack[StackType]);
  FStack[StackType] := aux;
end;

procedure TxbScript.StackPush(StackType: TStackType; x: TScriptValue);
var
  prev: pStackElement;
begin
  prev := FStack[StackType];
  New(FStack[StackType]);
  with FStack[StackType]^ do
  begin
    Previous := prev;
    Element := x;
    if VarIsNull(x) then
      Index := -1
    else if Previous = nil then
      Index := 0
    else
      Index := Previous^.Index + 1;
  end;
end;

{ TxbVariableInfo }

constructor TxbVariableInfo.Create(ACollection: TCollection);
begin
  inherited;
  FVisibility := vvPublic;
end;

function TxbVariableInfo.GetDeclaredClassName: string;
begin

end;

function TxbVariableInfo.GetValue: TScriptValue;
begin
  if Global then
    if Assigned(Script.VirtualMachine) then
      begin
        if not Script.VirtualMachine.FInitialized then

      end;
end;

procedure TxbVariableInfo.PullSystemType;
var
  s : String;
begin
  s := LowerCase(FTypeDecl);
  if s = 'integer' then
    FSystemType := vtInteger
  else if s = 'boolean' then
    FSystemType := vtBoolean
  else if s = 'char' then
    FSystemType := vtChar
  else if s = 'extended' then
    FSystemType := vtExtended
  else if s = 'int64' then
    FSystemType := vtInt64
  else if s = 'longint'  then
    FSystemType := vtLongint
  else if s = 'single' then
    FSystemType := vtSingle
  else if s = 'byte' then
    FSystemType := vtByte
  else if s = 'string' then
    FSystemType := vtUnicodeString
  else if s = 'double' then
    FSystemType := vtDouble
  else if s = 'real' then
    FSystemType := vtReal
  else
    FSystemType := -1;


end;

function TxbVariableInfo.routine: TxbRoutineInfo;
begin
  Result := TxbRoutineInfo(TxbVariablesInfo(Collection).GetOwner);
  if TObject(Result) is TxbScriptInfo then
    Result := TxbScriptInfo(Result).MainRoutine;
end;

function TxbVariableInfo.Script: TxbScript;
begin

end;

procedure TxbVariableInfo.SetDeclaredClassName(const Value: string);
begin

end;

procedure TxbVariableInfo.SetTypeFromString(ATypeStr: string);
begin
//  FDeclaredClass := ScriptInfo.Script.Scripter.Classes.ClassByName(ATypeStr);
  PullSystemType;
end;

procedure TxbVariableInfo.SetValue(AValue: TScriptValue);
begin

end;

function TxbVariableInfo.VarIndex: integer;
begin
  if FGlobal then
    Result := Index
  else
    Result := Index - routine.ArgCount;
end;

{ TxbObject }

procedure TxbObject.Assign(ASource: TPersistent);
begin
  Name := TxbObject(ASource).Name;
  Instance := TxbObject(ASource).Instance;
  FatClass := TxbObject(ASource).FatClass;
end;

procedure TxbObject.SetxbClass(const Value: TxbClass);
begin

end;

{ TxbObjects }

constructor TxbObjects.Create(AClasses: TxbClasses);
begin
  inherited Create(TxbObject);
  FClasses := AClasses;
  Clear;
end;

function TxbObjects.GetItem(i: integer): TxbObject;
begin
  Result := TxbObject(inherited Items[i]);
end;

{ TxbClasses }

constructor TxbClasses.Create(AScripter: TxbBaseScripter);
begin
  FScripter := AScripter;
  inherited Create(TxbClass);
end;

function TxbClasses.GetItem(i: integer): TxbClass;
begin
  Result := TxbClass(Inherited Items[i]);
end;

{ TxbRoutinesInfo }

function TxbRoutinesInfo.GetItem(i: Integer): TxbRoutineInfo;
begin
   Result := TxbRoutineInfo(inherited Items[i]);
end;

function TxbRoutinesInfo.IndexOf(AName: string): integer;
begin
  for Result := 0 to Count-1 do
    if CompareText(Items[Result].FName, AName) = 0 then
      exit;
  Result := -1;
end;

{ TxbVariablesInfo }

function TxbVariablesInfo.Add: TxbVariableInfo;
begin
  result := TxbVariableInfo(inherited Add);
end;

function TxbVariablesInfo.FindByName(AName: string): TxbVariableInfo;
var
  i : Integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    Result := Items[i]
  else
    Result := nil;
end;

function TxbVariablesInfo.GetItem(i: Integer): TxbVariableInfo;
begin
  Result := TxbVariableInfo(inherited Items[i]);
end;

function TxbVariablesInfo.IndexOf(AName: string): integer;
begin
  for Result := Count-1 downto 0 do
   if CompareText(Items[Result].FVarName, AName) = 0 then
     exit;
  Result := -1;
end;

{ TxbMethods }

constructor TxbMethods.Create(AClass: TxbClass);
begin
  FClass := AClass;
  inherited Create(TxbMethod);
end;

function TxbMethods.GetItem(i: integer): TxbMethod;
begin
  result := TxbMethod(inherited Items[i]);
end;

function TxbMethods.IndexOf(AName: string): integer;
begin
  for Result := Count -1  to 0 do
    if CompareText(AName, Items[Result].FName) =0 then
       exit;
  Result := -1;
end;

function TxbMethods.MethodByName(AName: string): TxbMethod;
var
  i: integer;
begin
  i := IndexOf(AName);
  if i > -1 then
    Result := Items[i]
  else
    Result := nil;
end;

{ TxbClass }

procedure TxbClass.SetMethods(const Value: TxbMethods);
begin
  FMethods.Assign(Value);
end;

{ TxbScriptInfo }

constructor TxbScriptInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutines := TxbRoutinesInfo.Create(Self,TxbRoutineInfo);
  FRoutines.FScriptInfo := Self;

  FGlobals := TxbVariablesInfo.Create(Self,TxbVariableInfo);

end;

destructor TxbScriptInfo.Destroy;
begin
  FGlobals.DisposeOf;
  FRoutines.DisposeOf;
  inherited;
end;

function TxbScriptInfo.RoutineByName(AName: string): TxbRoutineInfo;
var
  i: integer;
begin
  i := Routines.IndexOf(AName);
  if i > -1 then
    Result := Routines[i]
  else
    Result := nil;
end;

procedure TxbScriptInfo.SetGlobals(const Value: TxbVariablesInfo);
begin
  FGlobals.Assign(Value);
end;

procedure TxbScriptInfo.SetRoutines(const Value: TxbRoutinesInfo);
begin
  FRoutines.Assign(Value);
end;

{ TxbRoutineInfo }

function TxbRoutineInfo.DeclareVariable(AName: string; ASourcePos,
  AArgIndex: integer; AModifier: TxbArgumentModifier): TxbVariableInfo;
begin
  Result := FVariables.Add;
  with Result do
  begin
    FVarName := AName;
    FDeclarationSourcePos := ASourcePos;
    FArgIndex := AArgIndex;
    FModifier := AModifier;
  end;
  if AArgIndex > -1 then
    Inc(FArgCount);

  if AModifier = moVar then
    FByRefArgMask := FByRefArgMask or (1 shl AArgIndex);
end;

function TxbRoutineInfo.LocalVarCount: integer;
begin
  Result := FVariables.Count - FArgCount;
end;

function TxbRoutineInfo.VariableByName(AName: string): TxbVariableInfo;
var
  i : Integer;
begin
  i := FVariables.IndexOf(AName);
  if i > -1 then
    Result := FVariables[i]
  else
    Result := nil;
end;

Initialization
  NullValue := Variants.Null;

end.
