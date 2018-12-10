unit xbParser;

interface
uses
  System.Classes,
  SysUtils;

Type
  TSyntaxResult = (srNotDefined, srCorrect, srIncomplete, srOverFlow, srIncorrect);

  TNoTerminals = class;
  TNoTerminalNode = class;
  TNoTerminalNodes = class;
  TxbSyntaxParser = class;

  { TParsingResults }
   TParsingResults = record
      Result          : TSyntaxResult;
      Suggestion      : char;
      InputPos        : integer;
      SyntaxPos       : integer;
      NoTerminalIndex : integer;
      MaxInputPos     : integer;
      LastResult      : TSyntaxResult;
   end;

   TNodeScanProc = procedure( Node:TNoTerminalNode ) of object;
   TAcceptNodeProc = function( Node:TNoTerminalNode ):boolean of object;
   TAcceptCharProc = function( Node:TNoTerminalNode; ACurrentPos: integer ):boolean of object;

   { TNoTerminal : used to save syntax structure}

   TNoTerminal = class(TCollectionItem)
     private
        FIdS                : string;
        FSyntax             : string; // sintaxe com referencias indexadas aos nao-terminais
        FSyntaxText         : string; // Syntax original
        FOnBeforeNodeScan   : TNodeScanProc;
        FOnAfterNodeScan    : TNodeScanProc;
        FOnAcceptNode       : TAcceptNodeProc;
        FOnAcceptCharacter  : TAcceptCharProc;
        FReverseScanning    : boolean;
        function GetNoTerminals: TNoTerminals;
     public
        procedure AssignNodeScanningEvents( AOnBeforeNodeScan,AOnAfterNodeScan:TNodeScanProc );

        property IdS:string read FIdS write FIdS;
        property Syntax:string read FSyntax write FSyntax;
        property SyntaxText:string read FSyntaxText write FSyntaxText;
        property OnBeforeNodeScan: TNodeScanProc read FOnBeforeNodeScan write FOnBeforeNodeScan;
        property OnAfterNodeScan: TNodeScanProc read FOnAfterNodeScan write FOnAfterNodeScan;
        property ReverseScanning: boolean read FReverseScanning write FReverseScanning;
        property OnAcceptNode: TAcceptNodeProc read FOnAcceptNode write FOnAcceptNode;
        property OnAcceptCharacter: TAcceptCharProc read FOnAcceptCharacter write FOnAcceptCharacter;
        property NoTerminals: TNoTerminals read GetNoTerminals;
   end;

   { TNoTerminals }

   TNoTerminals = class(TCollection)
     private
        function GetItem(Index:Integer):TNoTerminal;
     public
        constructor Create;
        function Add(AIdS,ASyntax:string):TNoTerminal;
        procedure Delete(i:integer);
        function IndexOf(IdS:string):integer;
        procedure LoadFromStrings(List:TStrings);
        property Items[Index: Integer]: TNoTerminal read GetItem; default;
   end;

  { TNoTerminalNode }

   TNoTerminalNode = class(TCollectionItem)
     private
       FNoTerminalIndex : Integer;
       FInputInitialPos : Integer;
       FInputFinalPos   : Integer;
       FNodes           : TNoTerminalNodes;
       FxbSyntaxParser  : TxbSyntaxParser;
       function GetInputToken:String;
       function GetOwnerNodes:TNoTerminalNodes;
       function GetInputChar(i:integer):char;
       function GetNoTerminal:TNoTerminal;
       function GetParentNode:TNoTerminalNode;

       procedure SetInputInitialPos(const Value: integer);
       procedure SetInputFinalPos(const Value: integer);
       function GetItem(i: integer): TNoTerminalNode;
       function GetStrToken: string;
     public
       constructor Create(ACollection:TCollection); override;
       destructor Destroy; override;

       property OwnerNodes: TNoTerminalNodes read GetOwnerNodes;
       property ParentNode: TNoTerminalNode read GetParentNode;
       property NoTerminalIndex:integer read FNoTerminalIndex write FNoTerminalIndex;
       property InputInitialPos:integer read FInputInitialPos write SetInputInitialPos;
       property InputFinalPos:integer read FInputFinalPos write SetInputFinalPos;
       property Nodes:TNoTerminalNodes read FNodes write FNodes;
       property InputToken:string read GetInputToken;
       property StrToken:string read GetStrToken;
       property NoTerminal:TNoTerminal read GetNoTerminal;
       property Items[i:integer]:TNoTerminalNode read GetItem; default;
       property xbSyntaxParser:TxbSyntaxParser read FxbSyntaxParser;
   end;

  { TNoTerminalNodes }

  TNoTerminalNodes = class(TCollection)
    private
      FNode  : TNoTerminalNode;   // ¸¸½Úµã£¨¼´±¾Éí£©
      function  GetItem(i:integer):TNoTerminalNode;
    public
      constructor Create(Owner:TNoTerminalNode);
      function Add(NoTermIndex:integer):TNoTerminalNode;
//      procedure Delete(i:integer);
      function IndexOf(NoTerminalIndex:integer):integer;
      function FindByNoTerminalName(const AName:string):TNoTerminalNode;
      property Node:TNoTerminalNode read FNode write FNode;
      property Items[i:integer]:TNoTerminalNode read GetItem; default;
  end;

  TSyntaxComment = class(TCollectionItem)
   private
      FOpenString: string;
      FCloseString: string;
      FPriorDelims: string;
   public
      property OpenString: string read FOpenString write FOpenString;
      property CloseString: string read FCloseString write FCloseString;
      property PriorDelims: string read FPriorDelims write FPriorDelims;
   end;

   TSyntaxComments = class(TCollection)
   private
      function GetItem(Index: integer): TSyntaxComment;
   public
      function Add(AOpenString, ACloseString: string): TSyntaxComment;
      property Items[Index: integer]: TSyntaxComment read GetItem; default;
   end;

  TxbSyntaxParser = class(TComponent)
    private
      FParsing             : boolean;
      FBranchCount         : integer;
      FNode                : TNoTerminalNode;
      FNoTerminals         : TNoTerminals;
      FSyntax              : string;
      FGrammar             : TStrings;
      FInput               : string;
      FStrings             : TStrings;
      FScanningInputPos    : integer;
      FStringDelimitter    : char;
      FCustomLexemes       : array[0..9] of string;
      FComments            : TSyntaxComments;
      FContinueOnRemarks   : boolean;

      procedure SetGrammar(Value : TStrings);
      procedure SetStrings(Value : TStrings);
      procedure GrammarChange(Sender : TObject);
      procedure StringsChange(Sender : TObject);

      function ParseInput(Nodes:TNoTerminalNodes;Syntax:string;
               InputPosition:integer;literal:boolean;
               CurrentNoTerm:integer; var results:TParsingResults ):boolean;

      function GetCustomLexeme(i: integer): string;
      procedure SetCustomLexeme(i: integer; const Value: string);
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear;
      function CheckLanguage: TParsingResults;
      procedure ScanNode(ANode: TNoTerminalNode);
      function ScanSyntaxTree:boolean;
      { properties }
      property NoTerminals:TNoTerminals read FNoTerminals;
      property Grammar : TStrings read FGrammar write SetGrammar;
      property Strings : TStrings read FStrings write SetStrings;
      property ScanningInputPos : integer read FScanningInputPos;
      property CustomLexemes[i:integer]:string read GetCustomLexeme write SetCustomLexeme;
      { for remarks}
      property StringDelimitter: char read FStringDelimitter write FStringDelimitter;
  end;

procedure GetRowColFromStrings(lines:TStrings;position:integer;var row,col:integer);

implementation

type
   TTokenType = (ttOptional, ttReplicate, ttAchoice, ttNoTerminal, ttLiterate,
     ttNumeric, ttAlfabetic, ttSpecial, ttCharacter,
     {ttSpaces has special syntax. After a space can follow a special character that differentiates the spaces syntax.

      ^ -> do not allow #13 or #10 characters
      \ -> requires #13 characters

      Example:
      <sample1>:one two three
      <sample2>:one ^two ^three
      <sample3>:one two \three

      in sample1, the words can be on the same line or different line
      in sample2, words must be in the same line
      in sample3, "three" must be in different (next) line than "two"
     }
     ttSpaces,
     ttString,
     ttAnything, ttCustom, ttLineFeed, ttSameLine);

procedure GetRowColFromStrings(lines:TStrings;position:integer;var row,col:integer);
var
  c, i, f: integer;
  newLineLen: integer;
begin
   if position<length(lines.text) then
   begin
      {$IFDEF MSWINDOWS}
      newLineLen := 2;
      {$ELSE}
      newLineLen := 1;
      {$ENDIF}

      row := 0;
      col := 0;
      i := 0;
      for c := 0 to lines.count - 1 do
      begin
         f := i + length(lines[c]) - 1;
         if (position >= i) and (position <= f + newLineLen) then
         begin
            row := c + 1;
            col := position - i + 1;
            if col > length(lines[c]) then
              col := length(lines[c]);
            break;
         end;
         i := f + 1 + newLineLen;
      end;
   end
   else
   begin
      row := lines.Count + 1;
      col := 1;
   end;
end;

{ TNoTerminalNode }

constructor TNoTerminalNode.Create(ACollection: TCollection);
begin
   inherited Create(ACollection);
   FInputInitialPos := -1;
   FInputFinalPos := -1;
   FNodes:=TNoTerminalNodes.Create(self);
end;

destructor TNoTerminalNode.Destroy;
begin
  FNodes.DisposeOf;
  inherited;
end;

function TNoTerminalNode.GetInputChar(i: integer): char;
begin
  result := FxbSyntaxParser.FInput[ FInputInitialPos + i ];
end;

function TNoTerminalNode.GetInputToken: String;
begin
  result := Copy(FxbSyntaxParser.FInput,FInputInitialPos,FInputFinalPos-FInputInitialPos+1);
end;

function TNoTerminalNode.GetItem(i: integer): TNoTerminalNode;
begin
  Result := FNodes[i];
end;

function TNoTerminalNode.GetNoTerminal: TNoTerminal;
begin
  result := FxbSyntaxParser.FNoTerminals[FNoTerminalIndex];
end;

function TNoTerminalNode.GetOwnerNodes: TNoTerminalNodes;
begin
  Result := TNoTerminalNodes(Collection);
end;

function TNoTerminalNode.GetParentNode: TNoTerminalNode;
begin
  if Assigned(Collection) then
    result := TNoTerminalNodes(Collection).Node
  else
    result := nil;
end;

function TNoTerminalNode.GetStrToken: string;
  function RemarkStarted(const AInput: string; var ACurPos: integer): boolean;

     function _MatchString(const S: string; AInputPos: integer): boolean;
     var i: integer;
     begin
        result:=true;
        for i:=0 to Length(S)-1 do
        begin
           if ((AInputPos+i)>length(AInput)) or (Upcase(AInput[AInputPos+i])<>Upcase(S[i+1])) then
           begin
              result:=false;
              break;
           end;
        end;
     end;

  var
    c: integer;
  begin
     result := false;
     for c:=0 to FxbSyntaxParser.FComments.Count-1 do
        if _MatchString(FxbSyntaxParser.FComments[c].OpenString, ACurPos) and
          (
            (FxbSyntaxParser.FComments[c].PriorDelims = '') or (ACurPos = 1) or
            ((ACurPos > 1) and (ACurPos <= Length(AInput)) and
            (Pos(AInput[ACurPos - 1], FxbSyntaxParser.FComments[c].PriorDelims) <> 0))
          ) then
        begin
           result := true;
           break;
        end;
  end;
var
  AInput: string;
  APos: Integer;
begin
  if Assigned(FxbSyntaxParser) then
  begin
    result := '';
    AInput := InputToken;
    APos := 1;
    while APos <= Length(AInput) do
    begin
      if not RemarkStarted(AInput, APos) then
      begin
        if APos <= Length(AInput) then
          result := result + AInput[APos];
      end else
        exit;
      inc(APos);
    end;
  end else
    result := InputToken;
end;

procedure TNoTerminalNode.SetInputFinalPos(const Value: integer);
begin
  FInputFinalPos := Value;
end;

procedure TNoTerminalNode.SetInputInitialPos(const Value: integer);
begin
  FInputInitialPos := Value;
end;

{ TNoTerminal }

procedure TNoTerminal.AssignNodeScanningEvents(AOnBeforeNodeScan,
  AOnAfterNodeScan: TNodeScanProc);
begin
   FOnBeforeNodeScan := AOnBeforeNodeScan;
   FOnAfterNodeScan  := AOnAfterNodeScan;
end;

function TNoTerminal.GetNoTerminals: TNoTerminals;
begin
  result:=TNoTerminals(Collection);
end;

{ TNoTerminals }

function TNoTerminals.Add(AIdS, ASyntax: string): TNoTerminal;
begin
  result := TNoTerminal(inherited Add);
   with result do
   begin
      IdS:=AIds;
      Syntax:=ASyntax;
   end;
end;

constructor TNoTerminals.Create;
begin
  inherited Create(TNoTerminal);
end;

procedure TNoTerminals.Delete(i: integer);
begin
  if (i>=0) and (i<Count) then
   begin
      inherited Delete(i);
//      TNoTerminal(Items[i]).Free;
   end;
end;

function TNoTerminals.GetItem(Index: Integer): TNoTerminal;
begin
  Result := TNoTerminal(inherited Items[Index]);
end;

function TNoTerminals.IndexOf(IdS: string): integer;
begin
  for result:=0 to Count-1 do
      if AnsiCompareText(TNoTerminal(Items[result]).IdS,UpperCase(IdS))=0 then Exit;
   result:=-1;
end;

procedure TNoTerminals.LoadFromStrings(List: TStrings);
var c,c1,i,f,s    : integer;
    aux0,aux1     : string;
    Transliterate : boolean;
begin
   Clear;
   { define todos os nomes de no-terminais }
   for c:=0 to list.count-1 do
   begin
      i:=pos('<',list[c]);
      f:=pos('>',list[c]);
      s:=pos(':',list[c]);
      with Add( copy(list[c],i+1,f-i-1),copy(list[c],s+1,length(list[c])-s) ) do
      begin
         { "R" (Reverse scanning)}
         if pos('R',copy(list[c],f+1,s-f-1))>0 then FReverseScanning:=True else
      end;
   end;
   { troca todos os nomes de não-terminais da Syntaxe pelos respectivos índices }
   Transliterate:=False;
   for c:=0 to count-1 do
   begin
      aux0:=Items[c].FSyntax;
      aux1:='';
      i:=-1;
      for c1:=1 to length(aux0) do
      begin
         if aux0[c1]='"' then
         begin
            Transliterate:=not Transliterate;
            aux1:=aux1+'"';
         end
         else
            if Transliterate then
               aux1:=aux1+aux0[c1]
            else
               if aux0[c1]='<' then
                  i:=c1
               else
                  if aux0[c1]='>' then
                  begin
                     aux1:=aux1+'<'+inttostr( IndexOf(copy(aux0,i+1,c1-i-1)) )+'>';
                     i:=-1;
                  end
                  else
                     if i=-1 then aux1:=aux1+aux0[c1];
      end;
      Items[c].FSyntaxText:=aux0;
      Items[c].FSyntax:=aux1;
   end;
end;

{ TSyntaxComments }

function TSyntaxComments.Add(AOpenString, ACloseString: string): TSyntaxComment;
begin
   result:=TSyntaxComment(inherited Add);
   result.OpenString:=AOpenString;
   result.CloseString:=ACloseString;
end;

function TSyntaxComments.GetItem(Index: integer): TSyntaxComment;
begin
  result:=TSyntaxComment(inherited Items[index]);
end;

{ TNoTerminalNodes }

function TNoTerminalNodes.Add(NoTermIndex: integer): TNoTerminalNode;
begin
  result:=TNoTerminalNode( inherited Add );
   with result do
   begin
      FxbSyntaxParser := Node.xbSyntaxParser;
      FNoTerminalIndex := NoTermIndex;
   end;
end;

constructor TNoTerminalNodes.Create(Owner: TNoTerminalNode);
begin
  inherited Create(TNoTerminalNode);
   FNode:=Owner;
end;

function TNoTerminalNodes.FindByNoTerminalName(
  const AName: string): TNoTerminalNode;
var i : integer;
begin
   result:=nil;
   { localiza o índice do não-terminal }
   i:=Node.NoTerminal.NoTerminals.IndexOf(AName);
   if (i>-1) then
   begin
      { localiza o n?correspondente ao índice encontrado }
      i:=IndexOf(i);
      if (i>-1) then
         result:=Items[i];
   end;
end;

function TNoTerminalNodes.GetItem(i: integer): TNoTerminalNode;
begin
  Result := TNoTerminalNode(inherited Items[i]);
end;

function TNoTerminalNodes.IndexOf(NoTerminalIndex: integer): integer;
begin
  for result:=0 to Count-1 do
     if TNoTerminalNode(Items[result]).FNoTerminalIndex=NoTerminalIndex then Exit;
  result:=-1;
end;

{ TxbSyntaxParser }

function TxbSyntaxParser.CheckLanguage: TParsingResults;
begin
  FBranchCount := 0;
  result.MaxInputPos := 0;
  Clear;
  FNode.FxbSyntaxParser := Self;
  FNode.FNoTerminalIndex := 0;
  FNode.InputInitialPos := 1;
  FParsing := true;
  try
    ParseInput(FNode.Nodes,FSyntax,1,False,0,result);
  finally
    FParsing := False;
    FNode.InputFinalPos := result.InputPos - 1;
  end;
end;

procedure TxbSyntaxParser.Clear;
begin
  if FParsing then
    raise Exception.Create('Cannot clear parser when parsing');
  if Assigned(FNode) then
    FNode.DisposeOf;
  FNode := TNoTerminalNode.Create(nil);
  FScanningInputPos := 0;
  FContinueOnRemarks := false;
end;

constructor TxbSyntaxParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNode := TNoTerminalNode.Create(nil);
  FComments := TSyntaxComments.Create(TSyntaxComment);
  FComments.Add('{','}');
  FNoTerminals := TNoTerminals.Create;
  FGrammar := TStringList.Create;
  TStringList(FGrammar).OnChange := GrammarChange;
  FStrings:=TStringList.Create;
  TStringList(FStrings).OnChange:=StringsChange;
  FInput:='';
  FSyntax:='';
  FBranchCount:=0;
  FStringDelimitter:='''';
end;

destructor TxbSyntaxParser.Destroy;
begin
  if Assigned(FNode) then
    FNode.DisposeOf;
  FNoTerminals.DisposeOf;
  FGrammar.DisposeOf;
  FStrings.DisposeOf;
  FComments.DisposeOf;
  inherited;
end;

function TxbSyntaxParser.GetCustomLexeme(i: integer): string;
begin
  result := FCustomLexemes[i];
end;

procedure TxbSyntaxParser.GrammarChange(Sender: TObject);
begin
  NoTerminals.LoadFromStrings(FGrammar);
  if NoTerminals.Count>0 then
    FSyntax:=NoTerminals[0].Syntax
  else
    FSyntax:='';
end;

{------------------------------------------------------------------------------}
{                                 P A R S E R                                  }
{------------------------------------------------------------------------------}
function TxbSyntaxParser.ParseInput(Nodes:TNoTerminalNodes;Syntax:string;InputPosition:integer;literal:boolean;CurrentNoTerm:integer; var results:TParsingResults ):boolean;
const ValidChars     : array[ttNumeric..ttAlfabetic] of string = (
      '0123456789',
      '_ABCDEFGHIJKLMNOPQRSTUVWXYZ' );
var EndOfInput       : boolean;
    EndOfSyntax      : boolean;
    SyntaxResult     : TSyntaxResult;
    SyntaxPosition   : integer;
    SuggestedChar    : char;
    InputChar        : char;
    SyntaxToken      : string;
    SyntaxTokenType  : TTokenType;
    ObligatorySubExp : boolean;
    SubExpFound      : boolean;
    SyntaxList       : array[0..29] of string;
    c,c0             : integer;
    TotSyntax        : integer;
    PartialResults   : TParsingResults;
    Transliterate    : boolean;
    LastSyntaxPos    : integer;
    LastInputPos     : integer;
    Level            : integer;
    NewNode          : TNoTerminalNode;
    AllowSuggestion  : boolean;
    InitialNodes     : integer;
    ok               : boolean;
    noTerminal       : TNoTerminal;
    curNode          : TNoTerminalNode;
    OriginalPos      : integer;
    LineFeedFound    : boolean;

    procedure CheckRemarks; forward;

    function CheckEndOfInput: boolean;     // ¼ì²éÊÇ·ñ¼ìË÷µ½ÊäÈëµÄÄ©Î²
    begin
       { testa se chegou ao final do texto de entrada }
       result := InputPosition > Length(FInput);
    end;

    procedure ExtractSubExpression( OpenChar, CloseChar:char; TokenType:TTokenType );
    var
      LenSyntaxToken: integer;
      LenSyntax: integer;
    begin

       SyntaxTokenType := TokenType;

       LenSyntax := Length(Syntax);
       SetLength(SyntaxToken, LenSyntax); {Maximum size of SyntaxToken will be the size of Syntax}
       LenSyntaxToken := 0;

       Inc( SyntaxPosition );
       { extração de seqüências literiais }
       if CloseChar='"' then
       begin
          while Syntax[ SyntaxPosition ]<>'"' do
          begin
             //SyntaxToken := SyntaxToken + Syntax[ SyntaxPosition ];
             LenSyntaxToken := LenSyntaxToken + 1;
             {$WARNINGS OFF}
             SyntaxToken[LenSyntaxToken] := Syntax[SyntaxPosition];
             {$WARNINGS ON}

             if SyntaxPosition > LenSyntax then
                raise EParserError.CreateFmt(
                   'Incorrect grammar. Literal string not terminated on ''%s''',
                   [ FNoTerminals[CurrentNoTerm].IdS ]);
             Inc( SyntaxPosition );
          end;
          Inc( SyntaxPosition );
       end
       else

       begin
          level := 1;
          transliterate := False; // indica que est?dentro de uma sequência literal
          repeat
             { igora sequencias literiais internas a outros tokens }
             if (Syntax[ SyntaxPosition ]='"') then
                transliterate := not transliterate;
             if not transliterate then
                if Syntax[ SyntaxPosition ]=CloseChar then level := level - 1 else
                if Syntax[ SyntaxPosition ]=OpenChar then level := level + 1;
             if level>0 then
             begin
                //SyntaxToken := SyntaxToken + Syntax[ SyntaxPosition ];
               LenSyntaxToken := LenSyntaxToken + 1;
               {$WARNINGS OFF}
               SyntaxToken[LenSyntaxToken] := Syntax[SyntaxPosition];
               {$WARNINGS ON}
             end;
             if SyntaxPosition > LenSyntax then
                raise EParserError.CreateFmt(
                   'Incorrect grammar. Subexpression not terminated on ''%s''',
                   [ FNoTerminals[CurrentNoTerm].IdS ]);
             Inc( SyntaxPosition );
          until (level=0);
          if {SyntaxToken=''} LenSyntaxToken = 0 then
             raise EParserError.CreateFmt(
                'Incorrect grammar. Subexpression is empty on ''%s''',
                [ FNoTerminals[CurrentNoTerm].IdS ]);
       end;
       SetLength(SyntaxToken, LenSyntaxToken);
    end;

    procedure CheckRemarks;   // Ìø¹ý×¢ÊÍ

       function MatchString(const S: string; AInputPos: integer): boolean;
       var i: integer;
       begin
          result := True;
          for i := 0 to Length(S) - 1 do
          begin
             if ((AInputPos + i) > Length(FInput)) or ((Upcase(FInput[AInputPos + i])) <> Upcase(S[i + 1])) then
             begin
                result := False;
                break;
             end;
          end;
       end;

    var CommentIndex : integer;
        c            : integer;
        startPos     : integer;
    begin
      repeat

         CommentIndex := -1;
         for c:=0 to FComments.Count-1 do
            if MatchString(FComments[c].OpenString, InputPosition) and
              (
                (FComments[c].PriorDelims = '') or (InputPosition = 1) or
                ((InputPosition > 1) and (InputPosition <= Length(FInput)) and
                (Pos(FInput[InputPosition - 1], FComments[c].PriorDelims) <> 0))
              ) then
            begin
               CommentIndex := c;
               Break;
            end;
         if CommentIndex<>-1 then
         begin
            startPos := InputPosition;
            level:=1;
            Inc( InputPosition, Length(FComments[CommentIndex].OpenString) );
            repeat
               if (InputPosition>length(FInput)) then
                  Break;
               if MatchString(FComments[CommentIndex].CloseString, InputPosition) then
                  Dec( level );
               Inc( InputPosition );
            until (level=0);
            if (level=0) then
               Inc( InputPosition, Length(FComments[CommentIndex].CloseString)-1 );
         end
         else
            if FContinueOnRemarks then
               Inc( InputPosition );
         { permite que o evento FOnExtractRemark permaneça ignorando o texto de entrada
           como se estivesse comentado, mas continua extraingo comentários e os processando }
      until not FContinueOnRemarks or (InputPosition>Length(FInput));
    end;

    procedure ExtractSimpleToken( TokenType:TTokenType; ExtractInput:boolean );
    begin
       { avança a análise 1 caracter dentro da gramática }
       SyntaxTokenType := TokenType;
       SyntaxToken := Syntax[ SyntaxPosition ];
       Inc( SyntaxPosition );

       Case TokenType of
         ttSpaces:
           if SyntaxPosition < Length(Syntax) then
             case Syntax[SyntaxPosition] of
               '^':
                 begin
                   SyntaxTokenType := ttSameLine;
                   Inc(SyntaxPosition);
                 end;
               '\':
                 begin
                   SyntaxTokenType := ttLineFeed;
                   Inc(SyntaxPosition);
                 end;
             end;
       end;

       if ExtractInput then
       begin
          { token de entrada - extrai 1 caracter de text a partir de position }
          if CheckEndOfInput then
             EndOfInput := True
          else
          begin
             InputChar := FInput[ InputPosition ];
             Inc( InputPosition );
             { sempre ignora comentários no interior de qualquer token,
               exceto quando estiver dentro de uma string }
             if (TokenType<>ttString) and (TokenType<>ttCustom) then
                CheckRemarks;
          end;
       end;
    end;

    procedure PreserveInitialNodes;
    begin
       if Nodes<>nil then
          InitialNodes := Nodes.Count;
    end;

    procedure RestoreInitialNodes;
    begin
      if Nodes<>nil then
      begin
        while Nodes.Count > InitialNodes do
          Nodes.Delete( Nodes.Count - 1 );
      end;
    end;

begin
   Inc( FBranchCount );
   noTerminal := FNoTerminals[CurrentNoTerm];
   curNode := Nodes.FNode;
   SyntaxResult := srNotDefined;
   SyntaxPosition := 1;
   SuggestedChar := #0;
   AllowSuggestion := True;
   EndOfInput := False;
   EndOfSyntax := False;
   CheckRemarks;
   while SyntaxResult = srNotDefined do
   begin
      LastSyntaxPos := SyntaxPosition;
      LastInputPos := InputPosition;
      { token da sintaxe - extrai a seqüência de caracteres do token atual da sintaxe }
      if SyntaxPosition > Length(Syntax) then
      begin
         EndOfSyntax := True;
         if CheckEndOfInput then
            EndOfInput := True;
      end
      else
      begin
         { se est?dentro de uma string, então extrai um caracter da entrada }
         if Literal then
            ExtractSimpleToken( ttCharacter, True )
         else
            { senão est?em string,
              identifica qual o tipo de sequência sintática que vir?agora na sintaxe }
            case Syntax[ SyntaxPosition ] of
               '[' : ExtractSubExpression( '[', ']', ttOptional );
               '(' : ExtractSubExpression( '(', ')', ttReplicate );
               '{' : ExtractSubExpression( '{', '}', ttAchoice );
               '<' : ExtractSubExpression( '<', '>', ttNoTerminal );
               '"' : ExtractSubExpression( '"', '"', ttLiterate );
               '#' : ExtractSimpleToken( ttNumeric, True );
               '@' : ExtractSimpleToken( ttAlfabetic, True );
               ' ' : ExtractSimpleToken( ttSpaces, False );
               '$' : ExtractSimpleToken( ttString, True );
               '~' : ExtractSimpleToken( ttSpecial, True );
               '%' : ExtractSimpleToken( ttAnything, True );
               '&' : ExtractSimpleToken( ttCustom, True );
               else  ExtractSimpleToken( ttCharacter, True );
            end;
      end;
      { verifica se encontrou o fim da sintaxe ou fim da entrada }
      if EndOfSyntax then
         if EndOfInput then
            SyntaxResult := srCorrect
         else
            SyntaxResult := srOverFlow
      else
      { tenta extratir da entrada um sequência correspondente ao
        elemento sintático corrente (indicado por SyntaxTokenType)  }
      Case SyntaxTokenType of
         ttCharacter:
         begin
            { caracter terminal (não literal) }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               if AllowSuggestion then
                  SuggestedChar := SyntaxToken[1];
               Dec( SyntaxPosition ); {correção}
            end
            else
               { testa o caracter sem diferenciar maiúsculas de minúsculas }
               if (Upcase(InputChar) <> UpCase(SyntaxToken[1])) then
               begin
                  SyntaxResult := srIncorrect;
                  Dec( InputPosition ); {correção}
                  Dec( SyntaxPosition ); {correção}
               end
               else
                  AllowSuggestion := True;
         end;
         ttAnything:
         begin
            { qualquer caracter }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               Dec( SyntaxPosition );
            end
            else
               AllowSuggestion := False;
         end;
         ttNumeric,
         ttAlfabetic:
         begin
            { algarismo numérico ou letra do alfabeto }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               Dec( SyntaxPosition ); {correção}
            end
            else
            begin
               if Pos(UpCase(InputChar),ValidChars[SyntaxTokenType])=0 then
               begin
                  SyntaxResult := srIncorrect;
                  Dec( InputPosition ); {correção}
                  Dec( SyntaxPosition ); {correção}
               end
               else
                  AllowSuggestion := True;
            end;
         end;
         ttSpecial:
         begin
            { espaço ou caracter separador de lexema }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               Dec( SyntaxPosition ); {correção}
            end
            else
            begin
               if (InputChar>#32) then
                  { se encontrar o "(" então satisfaz o ttSpecial, mas não avança
                    a entrada para que o "(" possa ser reconhecido posteriormente }
                  if (InputChar='(') or (InputChar=';') then // outros símbolos especiais poderiam encerrar o ttSpecial
                     Dec( InputPosition )
                  else
                  begin
                     SyntaxResult := srIncorrect;
                     Dec( InputPosition ); {correção}
                     Dec( SyntaxPosition ); {correção}
                  end
               else
                  AllowSuggestion := True;
            end;
         end;
         ttCustom:
         begin
            { caracter pertinente a um conjunto de caracteres dado pelo usuário }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               Dec( SyntaxPosition ); {correção}
            end
            else
            begin
               { obtém o índice correspondente ao conjunto de caracteres permitidos }
               if (SyntaxPosition <= Length(Syntax)) and (Syntax[SyntaxPosition] in ['0'..'9']) then
               begin
                  c := ord(Syntax[SyntaxPosition])-48;
                  Inc(SyntaxPosition);
               end
               else
                  c := -1;

               { quando usado com indice então o caracter deve estar contido no string que
                 representa os caracteres validos do lexema (string indexada por "c")
                 quando usado sem indice permite que um evento externo possa validar o caracter }
               if ( ((c>-1) and (Pos(InputChar,FCustomLexemes[c])=0))  or
                    ((c=-1) and (not Assigned(noTerminal.OnAcceptCharacter) or not noTerminal.OnAcceptCharacter(curNode,InputPosition-1)))) then
               begin
                  SyntaxResult := srIncorrect;
                  Dec( InputPosition ); {correção}
                  Dec( SyntaxPosition ); {correção}
               end
               else
                  AllowSuggestion := True;
            end;
         end;
         ttOptional: // ¿ÉÓÐ¿ÉÎÞµÄ½áµã
         begin
            { sequência opcional (não obrigatória) }
            PreserveInitialNodes;
            ParseInput( Nodes, SyntaxToken, InputPosition, False, CurrentNoTerm, results );
            with results do
               if (Result = srOverFlow) or (Result = srCorrect) then
               begin
                  { aceita o n?}
                  InputPosition := InputPos;
                  AllowSuggestion := True;
               end
               else
               begin
                  { rejeita o n?}
                  RestoreInitialNodes;
                  AllowSuggestion := False;
               end;
         end;
         ttLiterate:
         begin
            { sequência literal de caracteres }
            ParseInput( Nodes, SyntaxToken, InputPosition, True, CurrentNoTerm, results );
            with results do
               if (Result<>srOverFlow) and (Result<>srCorrect) then
               begin
                  if AllowSuggestion then
                     SuggestedChar := Suggestion;
                  SyntaxResult := Result;
                  SyntaxPosition := LastSyntaxPos + SyntaxPos; {correção}
                  InputPosition := LastInputPos + InputPos - InputPosition; {correção}

               end
               else
               begin

                  InputPosition := InputPos;
                  AllowSuggestion := True;
               end;
         end;
         ttReplicate:  // ÖØ¸´³öÏÖµÄ½áµã
         begin
            { sequência replicável (presente uma ou mais vezes) }
            ObligatorySubExp := True;
            repeat
               PreserveInitialNodes;
               ParseInput( Nodes, SyntaxToken, InputPosition, False, CurrentNoTerm, results );
               with results do
               begin
                  if (Result=srOverFlow) or (Result=srCorrect) then
                  begin
                     InputPosition := InputPos;
                     SubExpFound := True;
//                     FLastRemarkFound := 0;
                  end
                  else
                  begin
                     { permite monitoramento de trechos parcialmente extraídos,
                       mas recusados }
//                     if (FLastRemarkFound>InputPosition) then
//                     begin
//                        //OutputDebugString(PChar(Format('Recusado %d',[InputPosition])));
//                        if Assigned(FOnRefuseRemarks) then
//                           FOnRefuseRemarks(FCurrentInclude,InputPosition);
//                        FLastRemarkFound := 0;
//                     end;
                     RestoreInitialNodes;
                     SubExpFound := False;
                  end;
                  if ObligatorySubExp then
                  begin
                     if SubExpFound then
                     begin
                        ObligatorySubExp := False;
                        AllowSuggestion := False;
                     end
                     else
                     begin
                        if AllowSuggestion then
                           SuggestedChar := Suggestion;
                        SyntaxResult := Result;
                        SyntaxPosition := LastSyntaxPos + SyntaxPos; {correção}
                        InputPosition := LastInputPos + InputPos - InputPosition; {correção}
                     end;
                  end;
               end;
            until (not ObligatorySubExp and not SubExpFound) or (SyntaxResult<>srNotDefined);
         end;
         ttAchoice:  // ³öÏÖÒ»¸ö»ò¶à¸ö
         begin
            { sequências alternativas
              (uma única sequencia deve ser obrigatoriamente extraída da lista de opções) }

            { monta a lista com as Syntaxes possiveis }
            c0 := 1;
            TotSyntax := 0;
            Transliterate := False;
            level := 0;
            for c := 1 to length(SyntaxToken) do
            begin
               if SyntaxToken[c]='"' then
                  transliterate := not transliterate;
               if not transliterate then
                  if SyntaxToken[c]='{' then Inc(level) else
                  if SyntaxToken[c]='}' then Dec(level);
               if not transliterate and (level=0) and (SyntaxToken[c]='|') then
               begin
                  SyntaxList[TotSyntax] := copy(SyntaxToken,c0,c-c0);
                  Inc(TotSyntax);
                  c0:=c+1;
               end;
            end;
            SyntaxList[TotSyntax] := copy(SyntaxToken,c0,length(SyntaxToken)-c0+1);
            Inc(TotSyntax);
            { verifica se pelo menos uma das sintaxes ?válida }
            PreserveInitialNodes;
            PartialResults.Result := srNotDefined;
            for c:=0 to TotSyntax-1 do
            begin
               ParseInput( Nodes, SyntaxList[c], InputPosition, False, CurrentNoTerm, results );
               with results do
               begin
                  if (Result = srOverFlow) or (Result = srCorrect) then
                  begin
                     PartialResults.Result := srNotDefined;
                     InputPosition := InputPos;
                     AllowSuggestion := True;
//                     FLastRemarkFound := 0;
                     Break;
                  end
                  else
                  begin
                     { permite monitoramento de trechos parcialmente extraídos,
                       mas recusados }
//                     if (FLastRemarkFound>InputPosition) then
//                     begin
//                        //OutputDebugString(PChar(Format('Recusado %d',[InputPosition])));
//                        if Assigned(FOnRefuseRemarks) then
//                           FOnRefuseRemarks(FCurrentInclude,InputPosition);
//                        FLastRemarkFound := 0;
//                     end;
                     RestoreInitialNodes;
                     if PartialResults.result = srNotDefined then
                        PartialResults := results
                     else
                        if (PartialResults.Result<>results.Result) or (results.Result=srIncorrect) then
                        begin
                           PartialResults.Result := srIncorrect;
                           PartialResults.Suggestion := #0;
                           PartialResults.SyntaxPos := 0; {correção}
                           if PartialResults.InputPos <> results.InputPos then
                              PartialResults.InputPos := InputPosition; {correção}
                        end
                        else
                           if PartialResults.Suggestion <> results.Suggestion then
                              PartialResults.Suggestion := #0;
                  end;
               end;
            end;
            with PartialResults do
               if result <> srNotDefined then
               begin
                  if AllowSuggestion then
                     SuggestedChar := Suggestion;
                  SyntaxResult := Result;
                  SyntaxPosition := LastSyntaxPos + SyntaxPos; {correção}
                  InputPosition := LastInputPos + InputPos - InputPosition; {correção}
               end;
         end;
         ttSpaces:
         begin
            { espaços ou caracteres especiais opcionais (mas não obrigatório) }

            { ignora todos os espaços em branco e comentários na entrada }
            AllowSuggestion := False;
            repeat
               if CheckEndOfInput then
               begin
                  EndOfInput := True;
                  SubExpFound := False;
               end
               else
               begin
                  if FInput[InputPosition] < #33 then // aceita espaços e caracteres especiais
                  begin
                     InputChar := FInput[InputPosition];
                     SubExpFound := True;
                     Inc( InputPosition );
                     CheckRemarks;
                  end
                  else
                     SubExpFound := False;
               end;
            until not SubExpFound;
         end;
         ttSameLine:
         begin
           OriginalPos := InputPosition - 1;

           {one or more spaces without having #13 or #10 characters}
           if EndOfInput then
           begin
             SyntaxResult := srIncomplete;
             Dec( SyntaxPosition );
           end
           else
           if (FInput[InputPosition] > #32) or (FInput[InputPosition] = #13) or (FInput[InputPosition] = #10) then
           begin
             SyntaxResult := srIncorrect;
             Dec( SyntaxPosition );
           end
           else
           begin
             Inc(InputPosition);
             repeat
                if CheckEndOfInput then
                begin
                   EndOfInput := True;
                   SubExpFound := False;
                end
                else
                begin
                  if (FInput[InputPosition] < #33) then // aceita espaços e caracteres especiais
                  begin
                    if (FInput[InputPosition] = #13) or (FInput[InputPosition] = #10) then
                    begin
                      SyntaxResult := srIncorrect;
                      Dec( SyntaxPosition );
                      SubExpFound := False;
                    end else
                    begin
                      InputChar := FInput[InputPosition];
                      SubExpFound := True;
                      Inc( InputPosition );
                      CheckRemarks;
                    end;
                  end
                  else
                     SubExpFound := False;
                end;
             until not SubExpFound;
           end;

           if SyntaxResult = srIncorrect then
             InputPosition := OriginalPos;
         end;
         ttLineFeed:
         begin
           {Accepts 0 or more spaces and *must have* line feed}
           OriginalPos := InputPosition - 1;

           {one or more spaces without having #13 or #10 characters}
           if EndOfInput then
           begin
             SyntaxResult := srIncomplete;
             Dec( SyntaxPosition );
           end
           else
           if (FInput[InputPosition] > #32) then
           begin
             SyntaxResult := srIncorrect;
             Dec( SyntaxPosition );
           end
           else
           begin
             LineFeedFound := (FInput[InputPosition] = #13) or (FInput[InputPosition] = #10);

             Inc(InputPosition);
             repeat
                if CheckEndOfInput then
                begin
                   EndOfInput := True;
                   SubExpFound := False;
                end
                else
                begin
                  if (FInput[InputPosition] < #33) then // aceita espaços e caracteres especiais
                  begin
                    if (FInput[InputPosition] = #13) or (FInput[InputPosition] = #10) then
                      LineFeedFound := true;
                    InputChar := FInput[InputPosition];
                    SubExpFound := True;
                    Inc( InputPosition );
                    CheckRemarks;
                  end
                  else
                     SubExpFound := False;
                end;
             until not SubExpFound;

             if not LineFeedFound then
             begin
               SyntaxResult := srIncorrect;
               Dec( SyntaxPosition );
             end;
           end;

           if SyntaxResult = srIncorrect then
             InputPosition := OriginalPos;
         end;
         ttString:
         begin
            { estrai uma string inteira do texto de entrada }
            if EndOfInput then
            begin
               SyntaxResult := srIncomplete;
               SuggestedChar := #0;
               Dec( SyntaxPosition ); {correção}
            end
            else
            begin
               if InputChar <> StringDelimitter then
               begin
                  SyntaxResult := srIncorrect;
                  Dec( InputPosition ); {correção}
                  Dec( SyntaxPosition ); {correção}
               end
               else
               begin
                  { extração da string }
                  repeat
                     if CheckEndOfInput then
                     begin
                        EndOfInput := True;
                        SyntaxResult := srIncomplete;
                        SuggestedChar := #0;
                        Dec( SyntaxPosition ); {correção}
                        Break;
                     end
                     else
                     begin
                        InputChar := FInput[InputPosition];
                        Inc( InputPosition );
                        if FInput[InputPosition-1] = StringDelimitter then
                           if (InputPosition <= Length(FInput)) and
                              (FInput[InputPosition] = StringDelimitter) then
                              Inc(InputPosition)
                           else
                              Break;
                     end;
                  until False; { saída com break }
               end;
            end;
         end;
         ttNoTerminal:
         begin
            { extrai uma sequência sintática inteira }

            { montagem da árvore sintática }
            PreserveInitialNodes;
            c := StrToInt(SyntaxToken);
            NewNode := Nodes.Add( c );
            NewNode.InputInitialPos := InputPosition;
//            if Assigned(FOnNewNode) then
//              FOnNewNode(NewNode);
            ParseInput( NewNode.Nodes, NoTerminals[c].Syntax, InputPosition, False, c, results );
            with results do
            begin
               NewNode.InputFinalPos := InputPos - 1;
               ok:=(Result=srOverFlow) or (Result=srCorrect);
               { se o não terminal possuir um evento para validação então o executa agora }
               if ok and Assigned(FNoTerminals[c].FOnAcceptNode) and not FNoTerminals[c].FOnAcceptNode(NewNode) then
               begin
                  { provoca um erro de sintaxe porque o não terminal foi rejeitado pelo evento de aceitação }
                  Result:=srIncorrect;
                  ok:=false;
               end;
               if ok then
               begin
                  { atualiza o node da árvore sintática }
                  InputPosition:=InputPos;
                  AllowSuggestion:=true;
//                  FLastRemarkFound := 0;
                  {$IFDEF SCRIPTDEBUG}
                  ShowMessage( Format('%s: %s',[FNoTerminals[c].FIdS,NewNode.InputToken]) );
                  {$ENDIF}
               end
               else
               begin
                  { permite monitoramento de trechos parcialmente extraídos,
                    mas recusados }
//                  if (FLastRemarkFound>InputPosition) then
//                  begin
//                     //OutputDebugString(PChar(Format('Recusado %d',[InputPosition])));
//                     if Assigned(FOnRefuseRemarks) then
//                        FOnRefuseRemarks(FCurrentInclude,InputPosition);
//                     FLastRemarkFound := 0;
//                  end;
                  RestoreInitialNodes;
                  if AllowSuggestion then SuggestedChar:=Suggestion;
                  SyntaxResult:=Result;
                  SyntaxPosition:=LastSyntaxPos; {correção}
                  InputPosition:=LastInputPos+InputPos-InputPosition; {correção}
                  CurrentNoTerm:=c;
               end;
            end;

         end;
      end;
   end;
   { retorna os resultados da análise }
   with results do
   begin
      Result := SyntaxResult;
      InputPos := InputPosition;
      SyntaxPos := SyntaxPosition;
      Suggestion := SuggestedChar;
   end;
   result := (SyntaxResult = srCorrect);

   { resultados detalhados do erro para a checagem de linguagens }
   if not result and (InputPosition > results.MaxInputPos) then
      with results do
      begin
         NoTerminalIndex := CurrentNoTerm;
         MaxInputPos := InputPosition;
         LastResult := SyntaxResult;
      end;
end;


procedure TxbSyntaxParser.ScanNode(ANode: TNoTerminalNode);
var
  c : Integer;
begin
  with ANode, NoTerminal do
  begin
    { check if there is an event on before node scanning }
    if Assigned(FOnBeforeNodeScan) then
    begin
      FScanningInputPos := ANode.FInputInitialPos;
      FOnBeforeNodeScan(ANode);
    end;
    { recursively scanning }
    if NoTerminal.ReverseScanning then
      for c := FNodes.Count-1 downto 0 do
        ScanNode( FNodes[c] )
    else
      for c := 0 to FNodes.Count-1 do
        ScanNode( FNodes[c] );

    { check if there is an event on before node scanning }
    if Assigned(FOnAfterNodeScan) then
      begin
         FScanningInputPos:=ANode.FInputFinalPos;
         FOnAfterNodeScan(ANode);
      end;
  end;
end;

function TxbSyntaxParser.ScanSyntaxTree: boolean;
begin
  ScanNode( FNode );
  result:=True;
end;

procedure TxbSyntaxParser.SetCustomLexeme(i: integer; const Value: string);
begin
  FCustomLexemes[i] := Value;
end;

procedure TxbSyntaxParser.SetGrammar(Value: TStrings);
begin
  FGrammar.Assign(Value);
end;

procedure TxbSyntaxParser.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TxbSyntaxParser.StringsChange(Sender: TObject);
begin
  FInput:=FStrings.Text;
end;

end.
