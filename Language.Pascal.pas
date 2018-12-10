unit Language.Pascal;

interface
uses
  System.Classes,
  System.SysUtils,
  {$IFDEF NEXTGEN}
  Generics.Collections,
  {$ELSE}
  Contnrs,
  {$ENDIF}
  xbScript,
  xbParser;

Type

  {$IFDEF NEXTGEN}
  TPushVarList = TList<pSimplifiedCode>;
  TPushVarListObjects = TObjectList<TPushVarList>;
  {$ELSE}
  TPushVarList = TList;
  TPushVarListObjects = TObjectList;
  {$ENDIF}

  TxbPascalScript = class(TxbScript)
    private
      FForCount          : integer; // contador de estrturas FOR
      FCurrentFor        : integer; // estrutura FOR corrente
      FWhileCount        : integer; // contador de estrturas WHILE
      FCurrentWhile      : integer; // estrutura WHILE corrente
      FRepeatCount       : integer; // contador de estrturas REPEAT
      FCurrentRepeat     : integer; // estrutura REPEAT corrente
      FIfCount           : integer; // contador de estruturas IF
      FCurrentIf         : integer; // estrutura IF corrente
      FCaseCount         : integer; // contador de estruturas CASE
      FCurrentCase       : integer; // estrutura CASE corrente
      FWithCount         : integer; // contador de estruturas WITH
      FCurrentWith       : integer; // estrutura WITH corrente
      FOperatorCount     : integer; // Counter of operators
      FCurrentOperator   : integer;
      FCaseVarIndex      : integer; // 韓dice da vari醰el do case corrente
      FCaseBranchLevel   : integer; // indica n韛el de ramifica玢o do CASE corrente
      FCurrentLoop       : TLoopStatement; // loop corrente
      FCurrentClass      : TxbClass; // classe corrente
      FIsClassReference  : boolean; // indica se a classe corrente ?uma refer阯cia de classe
      FLastByRefArgMask  : integer; // last "Arg_List" no-terminal by-reference argument bit mask
      FByRefArgMask      : integer; // current Arg_List no-terminal by-reference argument bit mask
      FThereIsAnySubrot  : boolean; // indica se existe alguma subrotina no script
      FPushVarListObjects: TPushVarListObjects; // list of pushvarlist, used to avoid memory leaks
      FPushVarList: TPushVarList; //A list of pinstruction which pushes the variable in the stack for a method/routine call. used when arg_list node is being scanned
      FLastPushVarList: TPushVarList; //A list of pinstruction which pushes the variable in the stack for a method/routine call. Uses after arg_list

      procedure BeforeMain( Node:TNoTerminalNode );
      procedure AfterMain( Node:TNoTerminalNode );
      procedure BeforeSubRoutine( Node:TNoTerminalNode );
      procedure AfterSubRoutine( Node:TNoTerminalNode );
      procedure AfterInputArgs(Node: TNoTerminalNode);
      procedure AfterVarDecl(Node: TNoTerminalNode);
      procedure AfterConstExpr(Node: TNoTerminalNode);
      procedure AfterPushOutput( Node:TNoTerminalNode );
      procedure BeforeFor( Node:TNoTerminalNode );
      procedure AfterFor( Node:TNoTerminalNode );
      procedure AfterForControl( Node:TNoTerminalNode );
      procedure AfterStep( Node:TNoTerminalNode );
      procedure BeforeWhile( Node:TNoTerminalNode );
      procedure AfterWhile( Node:TNoTerminalNode );
      procedure BeforeWhileControl( Node:TNoTerminalNode );
      procedure AfterWhileControl( Node:TNoTerminalNode );
      procedure BeforeRepeat( Node:TNoTerminalNode );
      procedure BeforeUntil( Node:TNoTerminalNode );
      procedure AfterUntil( Node:TNoTerminalNode );
      procedure BeforeIf( Node:TNoTerminalNode );
      procedure AfterIf( Node:TNoTerminalNode );
      procedure BeforeThen( Node:TNoTerminalNode );
      procedure AfterThen( Node:TNoTerminalNode );
      procedure BeforeElse( Node:TNoTerminalNode );
      procedure BeforeExpression( Node:TNoTerminalNode );
      procedure AfterExpression( Node:TNoTerminalNode );
      procedure AfterOperator( Node:TNoTerminalNode );
      procedure AfterUnary( Node:TNoTerminalNode );
      procedure AfterLabel( Node:TNoTerminalNode );
    public
      constructor Create(ACollection:TCollection); override;
      destructor Destroy; override;
      procedure Clear; override;
  end;

type
   { No-terminal symbol index list }
   xbPASCAL_NOTERMINALS = (
      noProgram,
      noGlobalDecl,
      noMain,
      noLibraries,
      noUses,
      noImports,
      noSubroutine,
      noCallingConvetion,
      noExternal,
      noFileName,
      noForward,
      noProcedure,
      noFunction,
      noInputArgs,
      noByRef,
      noByVal,
      noConstDecl,
      noConstExpr,
      noConstName,
      noVarDecl,
      noVarList,
      noVarName,
      noVarType,
      noStatement,
      noBlock,
      noCall,
      noCall_code,
      noCall_ptr,
      noCall_ptr_code,
      noData_val,
      noData_val_code,
      noData_ref,
      noData_ref_code,
      noArg_list,
      noIndexing,
      noAssign,
      noPush_output,
      noFor,
      noFor_control,
      noFor_downto,
      noStep,
      noWhile,
      noWhile_control,
      noRepeat,
      noUntil,
      noIf,
      noThen,
      noElse,
      noExpression,
      noOperator,
      noUnary,
      noLabel,
      noId,
      noReal,
      noHex,
      noFrac,
      noExp,
      noString,
      noVector,
      noTry,
      noFinally,
      noExcept,
      noTry_statements,
      noTry_upshot,
      noCase,
      noCase_expr,
      noCase_branch,
      noCase_test,
      noCase_match,
      noWith,
      noWith_Obj,
      noAs_Class );

implementation

{************************************************************************}
{                                                                        }
{            PASCAL scripting engine                                     }
{                                                                        }
{************************************************************************}

const
   { Pascal Syntax Specification }
   xbPASCAL_GRAMMAR = '<program>: [program {@|_}[({@|#|_})][ ;]] [<libraries>] [<globaldecl>] [( <subroutine>)] <main>'#13#10+
                      '<globaldecl>:({<constdecl>|<vardecl>})'#13#10+
                      '<main>:[[({<constdecl>|<vardecl>})]{<block>[ .] |(<statement> )}]'#13#10+
                      '<libraries>:[({<uses>|<imports>} )]'#13#10+
                      '<uses>R:uses <id>[( , <id>)][ ;]'#13#10+
                      '<imports>R:imports <id>[( , <id>)][ ;]'#13#10+
                      '<subroutine>:{<procedure> <inputargs>|<function> <inputargs> [":" <id>]} [; ]{<external>|<forward>|[({<constdecl>|<vardecl>})] {<block>|<statement>}}'#13#10+
                      '<callingConvention>:{stdcall|safecall|cdecl|pascal|register}'#13#10+
                      '<external>:[<callingConvention> [; ]]external <filename> [name ''<constname>''] [; ]'#13#10+
                      '<filename>:$'#13#10+
                      '<forward>:forward[ ;]'#13#10+
                      '<procedure>:procedure <label>'#13#10+
                      '<function>:function <label>'#13#10+
                      '<inputargs>:["(" {<byref>|<byval>}[( ; {<byref>|<byval>})] ")"]'#13#10+
                      '<byref>:var~ <varname>[( , <varname>)][ ":" <vartype>]'#13#10+
                      '<byval>:<varname>[( , <varname>)][ ":" <vartype>]'#13#10+
                      '<constdecl>:(const (<constexpr>[ ;] ))'#13#10+
                      '<constexpr>:<constname> "=" <expression>'#13#10+
                      '<constname>:{@|_}[({@|#|_})]'#13#10+
                      '<vardecl>:(var~ (<varlist>[ ;] ))'#13#10+
                      '<varlist>:<id>[( , <id>)][ ":" <vartype>]'#13#10+
                      '<varname>:<id>'#13#10+
                      '<vartype>:{@|_}[({@|#|_})]'#13#10+
                      '<statement>:{<if>|<for>|<while>|<repeat>|<assign>|<try>|<call>|<case>|<with>}[ ;]'#13#10+
                      '<block>:begin~ [({<block>|<statement>} )]end[ ;]'#13#10+
                      '<call>:<id><arg_list><indexing><call_code>[.<call>]'#13#10+
                      '<call_code>:[ as <as_class>]'#13#10+
                      '<call_ptr>:<id><arg_list><indexing><call_ptr_code>[ . <call_ptr>]'#13#10+
                      '<call_ptr_code>:[ as <as_class>]'#13#10+
                      '<data_val>:<id><arg_list><indexing><data_val_code>[ . <data_val>]'#13#10+
                      '<data_val_code>:[ as <as_class>]'#13#10+
                      '<data_ref>:<id><arg_list><indexing><data_ref_code>[ . <data_ref>]'#13#10+
                      '<data_ref_code>:[ as <as_class>]'#13#10+
                      '<arg_list>:[ "(" [<expression> [(, <expression> )]]")"]'#13#10+
                      '<indexing>:[( "[" <expression> [(, <expression> )]"]")]'#13#10+
                      '<assign>R:<data_ref> := <expression>'#13#10+
                      '<push_output>:'#13#10+
                      '<for>:{<for_control> [step <step>]|<for_downto_control>} do {<block>|<statement>}'#13#10+
                      '<for_control>:for <id> := <expression> to <expression>'#13#10+
                      '<for_downto_control>:for <id> := <expression> downto <expression>'#13#10+
                      '<step>:[{-|+}](#)[.(#)][e[{-|+}](#)]'#13#10+
                      '<while>:while <while_control> do {<block>|<statement>}'#13#10+
                      '<while_control>:<expression>'#13#10+
                      '<repeat>:repeat [({<block>|<statement>} )] <until>'#13#10+
                      '<until>:until <expression>'#13#10+
                      '<if>:if <expression> <then> [<else>]'#13#10+
                      '<then>:then {<block>|<statement>}'#13#10+
                      '<else>:else {<block>|<statement>}'#13#10+
                      '<expression>:{[<unary>]{<data_val>|"(" <expression> ")"}|<hex>|<real>|<string>|<vector>}[( <operator> {[<unary>]{<data_val>|"(" <expression> ")"}|<hex>|<real>|<string>|<vector>})]'#13#10+
                      '<operator>:{"^"|*|/|and~|+|-|or~|"<>"|">="|"<="|"="|">"|"<"|div~|mod~|xor~|shl~|shr~|is~}'#13#10+
                      '<unary>:{not~ |-|+}'#13#10+
                      '<label>:<id>'#13#10+
                      '<id>:{@|_}[({@|#|_})]'#13#10+
                      '<real>:[{-|+}](#)[<frac>][<exp>]'#13#10+
                      '<hex>:"$"({#|a|b|c|d|e|f})'#13#10+
                      '<frac>:.(#)'#13#10+
                      '<exp>:e[{-|+}](#)'#13#10+
                      '<string>:({$|"#"(#)|"#$"({#|a|b|c|d|e|f})})'#13#10+
                      '<vector>:"[" [<expression> [(, <expression> )]]"]"'#13#10+
                      '<try>:try <try_statements>{<finally>|<except>} <try_upshot>end'#13#10+
                      '<finally>:finally'#13#10+
                      '<except>:except'#13#10+
                      '<try_statements>:[({<block>|<statement>} )]'#13#10+
                      '<try_upshot>:[({<block>|<statement>} )]'#13#10+
                      '<case>:case <case_expr> of <case_branch> end'#13#10+
                      '<case_expr>:<expression>'#13#10+
                      '<case_branch>:{<case_test> [(, <case_test> )]: <case_match>[ <case_branch>]|[else {<block>|<statement>}]}'#13#10+
                      '<case_test>:<expression>'#13#10+
                      '<case_match>:{<block>|<statement>}'#13#10+
                      '<with>:with <with_obj> [(, <with_obj> )]do {<block>|<statement>}'#13#10+
                      '<with_obj>:<data_val>'#13#10+
                      '<as_class>:{@|_}[({@|#|_})]';

const
   {------------------------------------------------------------------------------}
   { Constantes para tratamento de operadores no gerador de c骴igo                }
   {------------------------------------------------------------------------------}

   { Precedencia de operadores: "0" ?o de maior prioridade }
   PascalOperatorLevel : array[inOperNE..inOperNot] of byte = (
     { inOperNE      } 4,
     { inOperGE      } 4,
     { inOperLE      } 4,
     { inOperEqual   } 4,
     { inOperGreater } 4,
     { inOperLess    } 4,
     { inOperAdd     } 3,
     { inOperConcat  } 3, { not used in pascal )
     { inOperSub     } 3,
     { inOperOr      } 3,
     { inOperXor     } 3,
     { inOperMul     } 2,
     { inOperSlash   } 2,
     { inOperDiv     } 2,
     { inOperMod     } 2,
     { inOperAnd     } 2,
     { inOperShl     } 2,
     { inOperShr     } 2,
     { inOperExp     } 1,
     { inOperIs      } 1,
     { inOperNeg     } 0,
     { inOperNot     } 0 );

   PascalOperatorId : array[inOperNE..inOperNot] of string = (
     { inOperNE      } '<>',
     { inOperGE      } '>=',
     { inOperLE      } '<=',
     { inOperEqual   } '=',
     { inOperGreater } '>',
     { inOperLess    } '<',
     { inOperAdd     } '+',
     { inOperConcat  } 'XXXX',
     { inOperSub     } '-',
     { inOperOr      } 'OR',
     { inOperXor     } 'XOR',
     { inOperMul     } '*',
     { inOperSlash   } '/',
     { inOperDiv     } 'DIV',
     { inOperMod     } 'MOD',
     { inOperAnd     } 'AND',
     { inOperShl     } 'SHL',
     { inOperShr     } 'SHR',
     { inOperExp     } '^',
     { inOperIs      } 'IS',
     { inOperNeg     } '',
     { inOperNot     } 'NOT' );

{ TxbPascalScript }

procedure TxbPascalScript.AfterConstExpr(Node: TNoTerminalNode);
begin
  OptimizeStoreVar(
    DeclareVariable(Node.Nodes[0].InputToken, -1, moConst, not Assigned(CurrentRoutine), Node.Nodes[0].InputInitialPos ), True );
end;

procedure TxbPascalScript.AfterExpression(Node: TNoTerminalNode);
var
  opInStack: TInstruction;
  opNumber: Integer;  // 操作符数
begin
  { insert all pending operators }
  while not StackDelimitterFound(stPendingOperators) do
  begin
    opInStack := TInstruction(StackViewAsInt(stPendingOperators) shr 20);
    opNumber := StackPopAsInt(stPendingOperators) and $FFFFF;
    with AppendInstruction(opInStack)^ do
      vDebugInfo := Parser.ScanningInputPos;
    if Scripter.ShortBooleanEval and (opInStack in [inOperAnd, inOperOr]) then
      DefineReferenceAddress('@Oper'+opNumber.ToString);
  end;
 StackPop(stPendingOperators);
end;

procedure TxbPascalScript.AfterFor(Node: TNoTerminalNode);
var
  VarName : string;
  VarIndex : Integer;
  IsGlobal : boolean;
  tempinst : TInstruction;
  step : double;
begin
  DefineReferenceAddress('@ForStep'+FCurrentFor.ToString);
  step := StackPopAsDouble(stContext);
  if Frac(step) = 0 then
  begin
    with AppendInstruction(inPushInteger)^ do
    begin
      vInt64 := round(step);
      vDebugInfo := Parser.ScanningInputPos;
    end;
  end else
  begin
    with  AppendInstruction(inPushDouble)^ do
    begin
      vDouble := step;
      vDebugInfo := Parser.ScanningInputPos;
    end;
  end;

  IsGlobal := StackPopAsBool(stContext);
  VarIndex := StackPopAsInt(stContext);
  VarName := StackPopAsString(stContext);

  if IsGlobal then
     tempinst := inPushGlobalVar
   else
     tempinst := inPushVar;

  with AppendInstruction(tempinst)^ do
   begin
      vInteger   := VarIndex;
      vString    := VarName;
      vDebugInfo := Parser.ScanningInputPos;
   end;

  with AppendInstruction(inJump)^ do
  begin
    vInteger := RegisterReference('@ForLoop'+FCurrentFor.ToString);
    vDebugInfo  := Parser.ScanningInputPos;
  end;

  DefineReferenceAddress('@EndFor'+FCurrentFor.ToString);
  FCurrentLoop := TLoopStatement(StackPopAsInt(stContext));
  FCurrentFor := StackPopAsInt(stContext);
end;

procedure TxbPascalScript.AfterForControl(Node: TNoTerminalNode);
var
  variable_end : TxbVariableInfo;
  variable_count : TxbVariableInfo;
  step : double;
  er : Integer;
  tempinst : TInstruction;
begin
  variable_end := CurrentRoutine.DeclareVariable('#ForStop'+FCurrentFor.ToString,Parser.ScanningInputPos);
  OptimizeStoreVar(variable_end);

  variable_count := RegisterVariableReference(StackPopAsString(stIdentifierList));

  if variable_count.Global then
  begin
    tempinst := inStoreGlobalVar;
  end else
  begin
    if variable_count.Modifier=moVar then
     tempinst := inStoreVarRef else
     tempinst := inStoreVar;
  end;

  With AppendInstruction(tempinst)^ do
  begin
    vInteger := variable_count.VarIndex;
    vString := variable_count.VarName;
    vDebugInfo := Parser.ScanningInputPos;
  end;

  StackPush(stContext, variable_count.VarName);
  StackPush(stContext, variable_count.VarIndex);
  StackPush(stContext, variable_count.Global);

  if variable_count.Global then
    tempinst := inPushGlobalVar
  else
    tempinst := inPushVar;

  with AppendInstruction(tempinst)^ do
  begin
    vInteger := variable_count.VarIndex;
    vString := variable_count.VarName;
    vDebugInfo := Parser.ScanningInputPos;
  end;

  with AppendInstruction(inPushVar)^ do
  begin
    vInteger := variable_end.VarIndex;
    vString := variable_end.VarName;
    vDebugInfo := Parser.ScanningInputPos;
  end;

  {caculate loop step}
  if Node.NoTerminalIndex = Ord(noFor_Downto) then
  begin
    StackPush(stContext,-1);
    step := -1;
  end else
  begin
    if Node.OwnerNodes.Count=2 then
    begin
      StackPush(stContext,1);
      step := 1;
    end else
      val(Node.OwnerNodes[Node.OwnerNodes.IndexOf(ord(noStep))].InputToken,step,er);
  end;

  if step<0 then
    with AppendInstruction(inOperGE)^ do
    begin
      vDebugInfo := Parser.ScanningInputPos;
    end
  else
    with AppendInstruction(inOperLE)^ do
    begin
      vDebugInfo := Parser.ScanningInputPos;
    end;
  with AppendInstruction(inJumpIfFalse)^ do
  begin
    vInteger := RegisterReference('@EndFor'+ FCurrentFor.ToString);
    vDebugInfo := Parser.ScanningInputPos;
  end;
end;

procedure TxbPascalScript.AfterIf(Node: TNoTerminalNode);
begin
  DefineReferenceAddress('@EndIf'+FCurrentIf.ToString);
  FCurrentIf := StackPopAsInt(stContext);
end;

procedure TxbPascalScript.AfterInputArgs(Node: TNoTerminalNode);
var
  c,c1 : Integer;
  argc,nc : Integer;
  variable : TxbVariableInfo;
  typeNode : TNoTerminalNode;
begin
  with CurrentRoutine do
  begin
    argc := 0;
    for c := 0 to Node.Nodes.Count-1 do
      begin
        nc := Node[c].Nodes.Count;
        if Node[c][nc-1].NoTerminalIndex = ord(noVarType) then
          typeNode := Node[c][nc-1]
        else
          typeNode := nil;

        { Node[c] is a <byref> or a <byvalue> }
        for c1 := 0 to nc-1 do
        begin
          { Node[c][c1] is a <varname> or a <vartype> }
          if Node[c][c1]<>typeNode then
            begin
              if Node[c].NoTerminalIndex = ord(noByRef) then
                variable := DeclareVariable(Node[c][c1][0].InputToken, Node[c][c1][0].InputInitialPos,argc,moVar)
              else
                variable := DeclareVariable(Node[c][c1][0].InputToken, Node[c][c1][0].InputInitialPos,argc);

              if typeNode <> nil then
              begin
                variable.TypeDecl := typeNode.InputToken;
                variable.SetTypeFromString(variable.TypeDecl);
              end;

              StackPop(stIdentifierList);
              Inc(argc);

            end;

        end;
      end;
    if IsFunction then
    begin
      variable := DeclareVariable('Result',Parser.ScanningInputPos);
      ResultIndex := variable.VarIndex;
      variable.TypeDecl := ResultTypeDecl;
      variable.SetTypeFromString(variable.TypeDecl);
    end;
  end;
end;

procedure TxbPascalScript.AfterLabel(Node: TNoTerminalNode);
var id : string;
    isFunction : boolean;
    prototyping : boolean;
    routine :TxbRoutineInfo;
    nodes : TNoTerminalNodes;
    functionNode : TNoTerminalNode;
    argTypeNodeIndex : Integer;
    c : Integer;
begin
  { SUBROUTINE
      PROCEDURE / FUNCTION
        LABEL
  OR
    MAIN
      LABEL  }
  id := StackPopAsString(stIdentifierList);
  prototyping := False;
  if Node.NoTerminalIndex <> ord(noMain) then
  begin
   { estamos em um <Subroutine> }
   nodes := Node.ParentNode.ParentNode.Nodes;
   for c:=0 to nodes.Count-1 do
     if xbPASCAL_NOTERMINALS(nodes[c].NoTerminalIndex) in [noForward,noExternal] then
     begin
       { estamos em uma declara玢o Forward ou External }
       prototyping := True;
       Break
     end;
  end;

  if not prototyping then
  begin
    DefineReferenceAddress(id);
    with AppendInstruction(inPrepare)^ do
    begin
      vString := id;
      vDebugInfo := Node.InputInitialPos;
    end;
  end;


  isFunction :=
    (Node.ParentNode.NoTerminalIndex=ord(noFunction)) or
    (Node.ParentNode.NoTerminalIndex=ord(noMain)) or
    (Node.NoTerminalIndex=ord(noMain));

  routine := ScriptInfo.RoutineByName(id);
  if not Assigned(routine) then
  begin
    if prototyping then
    begin
      CurrentRoutine := ScriptInfo.DeclareRoutine( id, nil, isFunction );
    end else
    begin
      CurrentRoutine := ScriptInfo.DeclareRoutine( id, LastInstruction, isFunction);
    end;
//  '<subroutine>:{<procedure> <inputargs>|<function> <inputargs> [":" <id>]} [; ]{<external>|<forward>|[({<constdecl>|<vardecl>})] {<block>|<statement>}}'#13#10+
    if (Node.ParentNode <> nil) and (Node.ParentNode.ParentNode <> nil) then
    begin
      functionNode := Node.ParentNode.ParentNode;  // subroutine
      argTypeNodeIndex := functionNode.Nodes.IndexOf(Ord(noId));
      if IsFunction and (argTypeNodeIndex >=0) then
        CurrentRoutine.ResultTypeDecl := functionNode.Nodes[argTypeNodeIndex].InputToken;
    end;

  end else
  begin
    if prototyping then
      CompileError( Format('Illegal redeclaration of routine ''%s''',[id]),Parser.ScanningInputPos )
    else begin
      if not Assigned(routine.DeclarationInstruction) then
      begin
        routine.Name := '#' + routine.Name;
        CurrentRoutine := ScriptInfo.DeclareRoutine(id, LastInstruction, isFunction);
  //      CurrentRoutine.Prototype := routine;
      end else
        CompileError( Format('Illegal reimplementation of routine ''%s''.'#13#10+'Use forward clause for prototyping',[id]),Parser.ScanningInputPos );
    end;
  end;
end;

procedure TxbPascalScript.AfterMain(Node: TNoTerminalNode);
begin
  AfterSubRoutine( Node );
end;

procedure TxbPascalScript.AfterOperator(Node: TNoTerminalNode);
var
  i,op: TInstruction;
  id: string;
  opInStack: TInstruction;
  opNumber: Integer;
begin
  { identifies the operator }
  op := low(TInstruction);  // only to remove a delphi compiler warning
  id := UpperCase(TrimRight(Node.StrToken));
  for i := inOperNE to inOperNot do
  begin
    if id = PascalOperatorId[i] then
    begin
      op := i;
      break;
    end;
  end;
  Inc(FOperatorCount);
  FCurrentOperator := FOperatorCount;
  { resolve operator precedence }
  while not StackDelimitterFound(stPendingOperators) and
    (PascalOperatorLevel[TInstruction(StackViewAsInt(stPendingOperators) shr 20)] <=
      PascalOperatorLevel[op]) do  // level越小优先级越高
  begin
    opInStack := TInstruction(StackViewAsInt(stPendingOperators) shr 20);
    opNumber := StackPopAsInt(stPendingOperators) and $FFFFF;
    with AppendInstruction(opInStack)^ do
      vDebugInfo:= Parser.ScanningInputPos;
    if Scripter.ShortBooleanEval and (opInStack in [inOperAnd, inOperOr]) then
      DefineReferenceAddress('@per'+opNumber.ToString);
  end;
  {if short boolean evaluation, then test if boolean expression was satisfied}
  if Scripter.ShortBooleanEval then
  begin
    case op of
      inOperOr: begin
        with AppendInstruction(inTestIfTrue)^ do
        begin
          vInteger := RegisterReference('@Oper'+FCurrentOperator.ToString);
          vDebugInfo := Parser.ScanningInputPos;
        end;
      end;

      inOperAnd: begin
         with AppendInstruction(inTestIfFalse)^ do
         begin
          vInteger := RegisterReference('@Oper'+FCurrentOperator.ToString);
          vDebugInfo := Parser.ScanningInputPos;
         end;
      end;
    end;
  end;

 { push the operator and its number in stack of pending operators}
 StackPush(stPendingOperators, (Ord(op) shl 20) or FCurrentOperator);
end;

procedure TxbPascalScript.AfterPushOutput(Node: TNoTerminalNode);
var
  c:Integer;
begin
  for c := 0 to Node.OwnerNodes.Count-3 do
   begin
     with AppendInstruction(inPushOutput)^ do
     begin
       vInteger := c;
       vDebugInfo := Parser.ScanningInputPos;
     end;
   end;
end;

procedure TxbPascalScript.AfterStep(Node: TNoTerminalNode);
var
  step : double;
  er   : integer;
begin
   val(Node.InputToken,step,er);
   StackPush(stContext,step);
end;

procedure TxbPascalScript.AfterSubRoutine(Node: TNoTerminalNode);
begin
  with CurrentRoutine do
    if Assigned(DeclarationInstruction) then
    begin
      DefineReferenceAddress( '@EndSub'+inttostr(Index) );
      if IsFunction then
        with AppendInstruction(inPushVar)^ do
        begin
          vInteger := ResultIndex;
          vString := 'Result';
          vDebugInfo := Parser.ScanningInputPos;
        end;

      DeclarationInstruction^.vInteger := LocalVarCount;
      with AppendInstruction(inRet)^ do
      begin
        vDebugInfo := Parser.ScanningInputPos;
      end;
    end;
end;

procedure TxbPascalScript.AfterThen(Node: TNoTerminalNode);
begin
  if Node.OwnerNodes.Count = 3 then  {if <expression> <then> <else>}
  begin
    // <then>:then {<block>|<statement>}
    { <then>之后，需跳转到if语句之后的代码，即AfterIf定义的@EndIf标号处}
    with AppendInstruction(inJump)^ do
    begin
      vInteger := RegisterReference('@EndIf'+FCurrentIf.ToString);
      vDebugInfo := Parser.ScanningInputPos;
    end;
  end;
end;

procedure TxbPascalScript.AfterUnary(Node: TNoTerminalNode);
begin
  Inc(FOperatorCount);
  FCurrentOperator := FOperatorCount;

  if Node.InputToken = '-' then
    { numeric complementation unary operator }
    StackPush(stPendingOperators, (Ord(inOperNeg) shl 20) or FCurrentOperator)
  else
    if Node.InputToken <> '+' then
      { boolean complementation unary operator }
      StackPush(stPendingOperators, (Ord(inOperNot) shl 20) or FCurrentOperator);
end;

procedure TxbPascalScript.AfterUntil(Node: TNoTerminalNode);
begin
  with AppendInstruction(inJumpIfFalse)^ do
  begin
    vInteger := RegisterReference('@RepeatLoop'+FCurrentRepeat.ToString);
    vDebugInfo := Parser.ScanningInputPos;
  end;
  DefineReferenceAddress('@EndRepeat'+FCurrentRepeat.ToString);
  FCurrentLoop := TLoopStatement(StackPopAsInt(stContext));
  FCurrentRepeat := StackPopAsInt(stContext);
end;

procedure TxbPascalScript.AfterVarDecl(Node: TNoTerminalNode);
var
  c,d : Integer;
  variable : TxbVariableInfo;
  typeNode : TNoTerminalNode;
begin
  for c := 0 to Node.Nodes.Count-1 do
    with Node[c] do  // <==> Node.Items[c]
    begin
      {Node[c] is <varlist>}

      { check to see if a type was explicited to variable }
      if Nodes[Nodes.Count-1].NoTerminalIndex = ord(noVarType) then
        typeNode := Nodes[Nodes.Count-1]
      else
        typeNode := nil;

      for d := 0 to Nodes.Count-1 do
        begin
          if Nodes[d].NoTerminalIndex=ord(noID) then  // <==> Nodes.Items[d]
            begin
              {declare a variable}
              variable := DeclareVariable(Nodes[d].InputToken, -1, moNone, not Assigned(CurrentRoutine), Nodes[d].InputInitialPos);

              if typeNode <> nil then
              begin
                variable.SetTypeFromString( typeNode.InputToken);
                variable.TypeDecl := typeNode.InputToken;
              end;
            end;
        end;
      {remove}
      StackPop( stIdentifierList );
    end;
end;

procedure TxbPascalScript.AfterWhile(Node: TNoTerminalNode);
begin
  with AppendInstruction(inJump)^ do
  begin
    {在 while <while_control> do S 形式下，执行完S需跳转回while_control,即BeforeWhileControl定义的@WhileLoop标号}
    vInteger := RegisterReference('@WhileLoop'+ FCurrentWhile.ToString);
    vDebugInfo := Parser.ScanningInputPos;
  end;
  DefineReferenceAddress('@EndWhile'+FCurrentWhile.ToString);
  FCurrentLoop := TLoopStatement(StackPopAsInt(stContext));
  FCurrentWhile := StackPopAsInt(stContext);
end;

procedure TxbPascalScript.AfterWhileControl(Node: TNoTerminalNode);
begin
  with AppendInstruction(inJumpIfFalse)^ do
  begin
    {如果while_control为false,则需要跳转执行while之后的指令，即AfterWhile定义的@EndWhile}
    vInteger := RegisterReference('@EndWhile'+FCurrentWhile.ToString);
    vDebugInfo := Parser.ScanningInputPos;
  end;
end;

procedure TxbPascalScript.BeforeElse(Node: TNoTerminalNode);
begin
  DefineReferenceAddress('@Else'+FCurrentIf.ToString);
end;

procedure TxbPascalScript.BeforeExpression(Node: TNoTerminalNode);
begin
   StackPushDelimitter(stPendingOperators);
end;

procedure TxbPascalScript.BeforeFor(Node: TNoTerminalNode);
begin
  StackPush(stContext,FCurrentFor);
  inc(FForCount);
  FCurrentFor := FForCount;
  StackPush(stContext,Ord(FCurrentLoop));
  FCurrentLoop := lsFor;
end;

procedure TxbPascalScript.BeforeIf(Node: TNoTerminalNode);
begin
  StackPush(stContext, FCurrentIf);
  Inc(FIfCount);
  FCurrentIf := FIfCount;
end;

procedure TxbPascalScript.BeforeMain(Node: TNoTerminalNode);
begin
   { simulate AfterId}
   StackPush( stIdentifierList, '___MAIN___');
   { execute AfterLabel(Node) }
   Parser.NoTerminals[ord(noLabel)].OnAfterNodeScan( Node );
   ScriptInfo.MainRoutine := CurrentRoutine;
   if CurrentRoutine.IsFunction then
     CurrentRoutine.ResultIndex := CurrentRoutine.DeclareVariable('Result', Parser.ScanningInputPos ).VarIndex;
end;

procedure TxbPascalScript.BeforeRepeat(Node: TNoTerminalNode);
begin
  StackPush(stContext, FCurrentRepeat);
  inc(FRepeatCount);
  FCurrentRepeat := FRepeatCount;
  StackPush(stContext, Ord(FCurrentLoop));
  FCurrentLoop := lsRepeat;
  DefineReferenceAddress('@RepeatLoop'+FCurrentRepeat.ToString);
end;

procedure TxbPascalScript.BeforeSubRoutine(Node: TNoTerminalNode);
begin
  if not FThereIsAnySubrot then
  begin
     with AppendInstruction(inJump)^ do
     begin
       vInteger := RegisterReference('___MAIN___');
       vDebugInfo := Parser.ScanningInputPos;
     end;
     FThereIsAnySubrot := true;
  end;
end;

procedure TxbPascalScript.BeforeThen(Node: TNoTerminalNode);
begin
  if Node.OwnerNodes.Count = 2 then { if <expression> <then> }
  begin
    { 若发现E为false,则需要跳转到if语句之后的代码，即AfterIf定义的@EndIf标号处}
    with AppendInstruction(inJumpIfFalse)^ do
    begin
      vInteger := RegisterReference('@EndIf'+FCurrentIf.ToString);
      vDebugInfo := Parser.ScanningInputPos;
    end;
  end else  {if <expression> <then> <else>}
  begin
    { 若发现E为false,则需要跳转到else的代码，即beforeif定义的@Else标号处}
    with AppendInstruction(inJumpIfFalse)^ do
    begin
      vInteger := RegisterReference('@Else'+FCurrentIf.ToString);
      vDebugInfo := Parser.ScanningInputPos;
    end;
  end;
end;

procedure TxbPascalScript.BeforeUntil(Node: TNoTerminalNode);
begin
  DefineReferenceAddress('@RepeatTest'+inttostr(FCurrentRepeat));
end;

procedure TxbPascalScript.BeforeWhile(Node: TNoTerminalNode);
begin
  StackPush(stContext, FCurrentWhile);
  FCurrentWhile := FWhileCount;
  inc(FWhileCount);
  StackPush(stContext, Ord(FCurrentLoop));
  FCurrentLoop := lsWhile;
end;

procedure TxbPascalScript.BeforeWhileControl(Node: TNoTerminalNode);
begin
  DefineReferenceAddress('@WhileLoop'+inttostr(FCurrentWhile));
end;

procedure TxbPascalScript.Clear;
begin
  inherited;

end;

constructor TxbPascalScript.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FPushVarListObjects := TPushVarListObjects.Create(true);

  with Parser, NoTerminals do
  begin
    Grammar.Text := xbPascal_GRAMMAR;

    Items[ ord(noMain)           ].AssignNodeScanningEvents( BeforeMain,        AfterMain );
    Items[ ord(noSubRoutine)     ].AssignNodeScanningEvents( BeforeSubRoutine,  AfterSubRoutine );
    Items[ ord(noInputArgs)      ].AssignNodeScanningEvents( nil,               AfterInputArgs );
    Items[ ord(noVarDecl)        ].AssignNodeScanningEvents( nil,               AfterVarDecl );
    Items[ ord(noConstExpr)      ].AssignNodeScanningEvents( nil,               AfterConstExpr );
    Items[ ord(noPush_Output)    ].AssignNodeScanningEvents( nil,               AfterPushOutput );
    Items[ ord(noBlock)          ].AssignNodeScanningEvents( nil,               nil );
    Items[ ord(noStatement)      ].AssignNodeScanningEvents( nil,               nil );
    Items[ ord(noAssign)         ].AssignNodeScanningEvents( nil,               nil );
    Items[ ord(noFor)            ].AssignNodeScanningEvents( BeforeFor,         AfterFor );
    Items[ ord(noFor_Control)    ].AssignNodeScanningEvents( nil,               AfterForControl );
    Items[ ord(noFor_Downto)     ].AssignNodeScanningEvents( nil,               AfterForControl );
    Items[ ord(noStep)           ].AssignNodeScanningEvents( nil,               AfterStep );
    Items[ ord(noWhile)          ].AssignNodeScanningEvents( BeforeWhile,       AfterWhile );
    Items[ ord(noWhile_Control)  ].AssignNodeScanningEvents( BeforeWhileControl,AfterWhileControl );
    Items[ ord(noRepeat)         ].AssignNodeScanningEvents( BeforeRepeat,      nil );
    Items[ ord(noUntil)          ].AssignNodeScanningEvents( BeforeUntil,       AfterUntil );
    Items[ ord(noIf)             ].AssignNodeScanningEvents( BeforeIf,          AfterIf );
    Items[ ord(noThen)           ].AssignNodeScanningEvents( BeforeThen,        AfterThen );
    Items[ ord(noElse)           ].AssignNodeScanningEvents( BeforeElse,        nil );
    Items[ ord(noExpression)     ].AssignNodeScanningEvents( BeforeExpression,  AfterExpression );
    Items[ ord(noOperator)       ].AssignNodeScanningEvents( nil,               AfterOperator );
    Items[ ord(noUnary)          ].AssignNodeScanningEvents( nil,               AfterUnary );
  end;

end;

destructor TxbPascalScript.Destroy;
begin
  FPushVarListObjects.DisposeOf;
  inherited;
end;

end.
