unit FrRnd;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DynTypes, LispParser, RndParser, Core;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel1: TPanel;
    Memo2: TMemo;
    BEvalL: TButton;
    BParseL: TButton;
    Bevel1: TBevel;
    BEvalRnd: TButton;
    BParseRnd: TButton;
    procedure BEvalLClick(Sender: TObject);
    procedure BEvalRndClick(Sender: TObject);
    procedure BParseLClick(Sender: TObject);
    procedure BParseRndClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BParseLClick(Sender: TObject);
var
  s: String;
  Parser: TLispParser;
  Term: dyn;
  ResRef: TDatumRef;
begin
  ManageRefs([@ResRef]);

  s := Memo1.Lines.Text;
  Parser := TLispParser.Create;
  Parser.Evaluate(ResRef, s);
  Term := ResRef.Value;

  s := DisplayL(Term);
  Memo2.Lines.Add(s);
end;

procedure TForm2.BEvalLClick(Sender: TObject);
var
  s: String;
  Parser: TLispParser;
  Term, Res: dyn;
  ResRef: TDatumRef;
begin
  ManageRefs([@ResRef]);

  s := Memo1.Lines.Text;
  Parser := TLispParser.Create;
  Parser.Evaluate(ResRef, s);
  Term := ResRef.Value;

  s := DisplayL(Term);
  Memo2.Lines.Add(s);

  if Term <> nil then
  begin
    Scope.Eval(ResRef, Term);

    Res := ResRef.Value;
    s := DisplayR(Res);
  end
  else
    s := '()';
  Memo2.Lines.Add(s);
end;

procedure TForm2.BParseRndClick(Sender: TObject);
var
  s: String;
  Parser: RndParser.TParser;
  Term: dyn;
begin
  s := Memo1.Lines.Text;
  Parser := RndParser.TParser.Create(s);
  Parser.GetNextTerm(Term);

  s := DisplayL(Term);
  Memo2.Lines.Add(s);
end;

procedure TForm2.BEvalRndClick(Sender: TObject);
var
  s: String;
  Parser: RndParser.TParser;
  Term, Res: dyn;
  ResRef: TDatumRef;
begin
  ManageRefs([@ResRef]);

  s := Memo1.Lines.Text;
  Parser := RndParser.TParser.Create(s);
  Parser.GetNextTerm(Term);

  s := DisplayL(Term);
  Memo2.Lines.Add(s);

  if Term <> nil then
  begin
    Scope.Eval(ResRef, Term);

    Res := ResRef.Value;
    s := DisplayR(Res);
  end
  else
    s := '()';
  Memo2.Lines.Add(s);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  InitCore;
end;

end.
