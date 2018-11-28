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
      FCaseVarIndex      : integer; // índice da variável do case corrente
      FCaseBranchLevel   : integer; // indica nível de ramificação do CASE corrente
      FCurrentLoop       : TLoopStatement; // loop corrente
      FCurrentClass      : TxbClass; // classe corrente
      FIsClassReference  : boolean; // indica se a classe corrente ?uma referência de classe
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
   { Constantes para tratamento de operadores no gerador de código                }
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

procedure TxbPascalScript.AfterInputArgs(Node: TNoTerminalNode);
begin

end;

procedure TxbPascalScript.AfterMain(Node: TNoTerminalNode);
begin
  AfterSubRoutine( Node );
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
  end;

end;

destructor TxbPascalScript.Destroy;
begin
  FPushVarListObjects.DisposeOf;
  inherited;
end;

end.
