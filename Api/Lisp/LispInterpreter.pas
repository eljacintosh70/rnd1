unit LispInterpreter;

interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DynTypes, DTScope, LispEval;
  // Classes, Lisp, DTArray DTProc DTProcRTTI

type
  TInterpreter = class(TLispEval, IInterpreter)
  public
    procedure Parse(out Result: TDatumRef; const Source: String);
    procedure Eval(out Result: TDatumRef; Datum: TDynDatum);
    procedure EnableRead;
    function LoadSrcLib(const Name: String): Boolean;
    function LoadDynLib(const Name: String): Boolean;
  end;

implementation /////////////////////////////////////////////////////////////////

uses
  LispParser, LispFiles, LispEnv;
  // SchList,

{ TInterpreter }

procedure TInterpreter.EnableRead;
begin
  LispFiles.ExportSymbols(Self);
end;

procedure TInterpreter.Eval(out Result: TDatumRef; Datum: TDynDatum);
type
  PDatumRef = ^TDatumRef;
begin
  LispEval.Eval(PDatumRef(@Result)^, Datum, Self);
end;

function TInterpreter.LoadDynLib(const Name: String): Boolean;
begin
  Result := LispEnv.LoadDynLib(Name, Self)
end;

function TInterpreter.LoadSrcLib(const Name: String): Boolean;
begin
  Result := LispEnv.LoadSrcLib(Name, Self)
end;

procedure TInterpreter.Parse(out Result: TDatumRef;
  const Source: String);
var
  Parser: TLispParser;
begin
  Parser := TLispParser.Create;
  Parser.Evaluate(TDatumRef(Result), Source);
  Parser.Free;
end;

end.////////////////////////////////////////////////////////////////////////////
