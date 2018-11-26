unit Language.Scripter;

interface
uses
  System.Classes,
  xbScript;

Type
  {Indicates the language/syntax of the script.}
  TScriptLanguage = (
    slPascal,  //Pascal language
    slJavaScript   // javaScript language
  );


  ///  <summary>
  ///    Main scripter component used to execute scripts, supporting several languages.
  ///  </summary>
  ///  <remarks>
  ///    TxbLanguageScripter component is a TxbBaseScripter descendant that effectively implement the script languages.
  ///    Alternatively to the deprecated TxbPascalScripter components, which only deal with one
  ///    language type, TatScripter component can create and execute scripts in all languages supported by Scripter Studio
  ///    package. You can use AddScript method to add a new script and define its language.
  ///    Most of properties, methods, collections you will use in TatScripter are implemented in its abstract ancestor,
  ///    TatCustomScripter.
  ///  </remarks>
  TxbLanguageScripter = class(TxbBaseScripter)
    private
      FDefaultLanguage : TScriptLanguage;
      function LanguageToClass(ALanguage: TScriptLanguage): TScriptClass;
    protected
      procedure DefineInternalClasses; override;
      function DefaultScriptClass: TScriptClass; override;
    public
      constructor Create(AOwner: TComponent; ADefaultLanguage:TScriptLanguage = slPascal); reintroduce;
      destructor Destroy; override;

    published
      property DefaultLanguage: TScriptLanguage read FDefaultLanguage write FDefaultLanguage;

      property SourceCode;
  end;

implementation
uses
  Language.Pascal;

{ TxbLanguageScripter }

constructor TxbLanguageScripter.Create(AOwner: TComponent; ADefaultLanguage:TScriptLanguage);
begin
  FDefaultLanguage := ADefaultLanguage;
  inherited Create(AOwner);

end;

function TxbLanguageScripter.DefaultScriptClass: TScriptClass;
begin
  result := LanguageToClass(FDefaultLanguage);
end;

procedure TxbLanguageScripter.DefineInternalClasses;
begin
  inherited;
  ScriptClass := TxbScript;
end;

destructor TxbLanguageScripter.Destroy;
begin

  inherited;
end;

function TxbLanguageScripter.LanguageToClass(
  ALanguage: TScriptLanguage): TScriptClass;
begin
  Case ALanguage of
    slPascal: result := TxbPascalScript;
  else
    result := nil;
  end;
end;

end.
