program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  xbScript in 'xbScript.pas',
  xbParser in 'xbParser.pas',
  Language.Scripter in 'Language.Scripter.pas',
  Language.Pascal in 'Language.Pascal.pas',
  TestUnit in 'TestUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
