unit FrRnd;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  //Winapi.Windows, 
  Messages, 
  SysUtils, 
  Variants, 
  Classes, 
  Graphics,
  Controls, 
  Forms, 
  Dialogs, 
  StdCtrls, 
  ExtCtrls,
  RndBase, RndParser;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel1: TPanel;
    Memo2: TMemo;
    BEval: TButton;
    procedure BEvalClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BEvalClick(Sender: TObject);
var
  s: String;
  Parser: TParser;
  Term: IDatum;
begin
  s := Memo1.Lines.Text;
  Parser := TParser.Create(s);
  Parser.GetNextTerm(Term);

  s := Display(Term);
  Memo2.Lines.Add(s);

  Term := Term.Eval;
  s := Display(Term);
  Memo2.Lines.Add(s);
end;

end.
