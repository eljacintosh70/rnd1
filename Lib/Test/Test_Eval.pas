unit Test_Eval;

interface

uses
  TestFramework,
  DTPort, DTPortW, DUtils, DynTypes, DTDatum,
  LispParserA, LispParser, LispWrite,
  RndParser, RndWrite, Core, TestFiles;

type
  // Test methods for class TDynOutPortD

  TestEvalCore = class(TTestCase)
  strict private
    FTextOut: TStrTextOutW;
    FDynOutPort: TDynOutPortLisp;
    Ref: IDynOutPort;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFromLisp;
    procedure TestFromRnd;
  end;

implementation

{ TestEvalCore }

procedure TestEvalCore.SetUp;
begin
  InitCore;
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortLisp.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
end;

procedure TestEvalCore.TearDown;
begin
  Ref := nil; //FDynOutPort.Free;
  FDynOutPort := nil;
end;

procedure TestEvalCore.TestFromLisp;
var
  SrcText, RefText: string;
  ReturnValue: Boolean;
  Obj: dyn;
  ResText, s: string;
  Res, Res2: TDatumRef;
  Parser: TLispParser;
begin
  SrcText := LoadTestFile('TestLispEval.txt');
  RefText := LoadTestFile('TestLispEval_Res.txt');

  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;

  Obj := Res.Value;
  s := DisplayL(obj);

  Core.Scope.Eval(Res2, Obj);
  Obj := Res2.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RefText, ResText, 'Error')
end;

procedure TestEvalCore.TestFromRnd;
var
  SrcText, RefText: string;
  ReturnValue: Boolean;
  Obj: dyn;
  ResText, s: string;
  Res, Res2: TDatumRef;
  Parser: TParser;
begin
  SrcText := LoadTestFile('TestRndEval.txt');
  RefText := LoadTestFile('TestRndEval_Res.txt');

  Parser := TParser.Create(SrcText);
  Parser.GetNextTerm(Obj);
  Parser.Free;

  s := DisplayL(obj);

  Core.Scope.Eval(Res2, Obj);
  Obj := Res2.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RefText, ResText, 'Error')
end;

initialization
  RegisterTest(TestEvalCore.Suite);
end.

