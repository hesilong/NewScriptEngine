unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Language.Scripter, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FScripter : TxbLanguageScripter;

    procedure TestRecord();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  TestUnit;
{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
//   TestRecord;
  FScripter := TxbLanguageScripter.Create(Self);
  FScripter.SourceCode := Memo1.Lines;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FScripter) then
    FScripter.DisposeOf;
end;

procedure TForm1.TestRecord;
var
  LRecord,LRecord1 : PStudent;
begin
   New(LRecord);
   LRecord^.Name := '张三';
   LRecord^.Sex := '男';

   ShowMessage(LRecord.Name);
   New(LRecord1);
   LRecord1^.Name := '李四';
   LRecord1^.Sex := '男';

   LRecord^.Next := LRecord1;

   ShowMessage(LRecord^.Next.Name);

   Dispose(LRecord);
   Dispose(LRecord1);
end;

Initialization
  ReportMemoryLeaksOnShutDown := true;

end.
