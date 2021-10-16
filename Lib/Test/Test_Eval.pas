unit Test_Eval;

interface

uses
  {$ifdef FPC}
  fpcunit, testutils, testregistry,
  {$else}
  TestFramework,
  {$endif}
  DTPort, DTPortW, DUtils, DynTypes, DTDatum,
  LispParserA, LispParser, LispWrite,
  RndParser, RndWrite, Core, TestFiles;

type
  // Test methods for class TDynOutPortD

  TestEvalCore = class(TTestCase)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSymbols;
    procedure TestEvalLisp;
    procedure TestEvalRnd;
  end;

implementation

{ TestEvalCore }

procedure TestEvalCore.SetUp;
begin
  //InitCore;
end;

procedure TestEvalCore.TearDown;
begin
end;

procedure TestEvalCore.TestSymbols;
const
  Names: array[0..9] of Utf8String = (
   'if', 'begin', '+', '-', '*', '&', 'Abc', 'ABC', 'áei', '%%' );
var
  Values: array[0..9] of IDynSymbol;
  i: Integer;
begin
  for i := 0 to High(Names) do
    Values[i] := InitSymbol(Names[i]);
  for i := 0 to High(Names) do     
    CheckEquals(Names[i], Values[i].Name, 'Symbol Name')
end;

procedure TestEvalCore.TestEvalLisp;
var
  SrcText, RefText, ParsedText: string;
  ReturnValue: Boolean;
  Obj: dyn;
  ResText, s: string;
  Res, Res2: TDatumRef;
  Parser: TLispParser;
  FTextOut: TStrTextOutW;
  FDynOutPort: TDynOutPortLisp;
  Ref: IDynOutPort;
begin
  SrcText := LoadTestFile('TestLispEval.txt');
  RefText := LoadTestFile('TestLispEval_Res.txt');
  ParsedText := LoadTestFile('TestLispEval_Parsed.txt');

  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;

  Obj := Res.Value;
  s := DisplayL(obj);
  s := NormalizeLines(s);
  CheckEquals(ParsedText, s, 'DisplayL');

  Core.Scope.Eval(Res2, Obj);
  Obj := Res2.Value;

  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortLisp.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;
  ResText := NormalizeLines(ResText);

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RefText, ResText, 'Error')
end;

procedure TestEvalCore.TestEvalRnd;
var
  SrcText, RefText, ParsedText: string;
  ReturnValue: Boolean;
  Obj: dyn;
  ResText, s: string;
  Res2: TDatumRef;
  Parser: TParser;
  FTextOut: TStrTextOutW;
  FDynOutPort: TDynOutPortRnd;
  Ref: IDynOutPort;
begin
  SrcText := LoadTestFile('TestRndEval.txt');
  RefText := LoadTestFile('TestRndEval_Res.txt');
  ParsedText := LoadTestFile('TestRndEval_Parsed.txt');

  Parser := TParser.Create(SrcText);
  Parser.GetNextTerm(Obj);
  Parser.Free;

  s := DisplayL(obj);
  s := NormalizeLines(s);
  //SaveTestFile('TestRndEval_Parsed.txt', s);
  CheckEquals(ParsedText, s, 'DisplayL');

  Core.Scope.Eval(Res2, Obj);
  Obj := Res2.Value;

  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortRnd.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RefText, ResText, 'Error')
end;

initialization
  InitCore;
  RegisterTest(TestEvalCore {$ifndef FPC}.Suite{$endif});
end.

