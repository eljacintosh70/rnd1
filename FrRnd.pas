unit FrRnd;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
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
  Strm: TStringStream;
begin
  s := Memo1.Lines.Text;
  Parser := TParser.Create(s);
  Parser.GetNextTerm(Term);

  Strm := TStringStream.Create('', TEncoding.Unicode);
  Term.Write(Strm);
  s := Strm.DataString;
  Memo2.Lines.Add(s);
end;

end.
