unit Test_DTPort;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework,
  DTPort, DTPortW, DUtils, DynTypes, DTDatum,
  LispParserA, LispParser, LispWrite,
  RndParser, RndWrite;

type
  // Test methods for class TDynOutPortD

  TestTDynOutPortD = class(TTestCase)
  strict private
    FTextOut: TStrTextOutW;
    FDynOutPort: TDynOutPort;
    Ref: IDynOutPort;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFromLisp;
  end;

  // Test methods for class TDynOutPortLisp

  TestTDynOutPortLisp = class(TTestCase)
  strict private
    FTextOut: TStrTextOutW;
    FDynOutPort: TDynOutPortLisp;
    Ref: IDynOutPort;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFromLisp;
    procedure TestFromLispA;
    procedure TestFromRnd;
  end;

  // Test methods for class TDynOutPortRnd

  TestTDynOutPortRnd = class(TTestCase)
  strict private
    FTextOut: TStrTextOutW;
    FDynOutPort: TDynOutPortRnd;
    Ref: IDynOutPort;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFromLisp;
    procedure TestFromLispA;
  end;

implementation

uses
  SysUtils;

const
  SrcText =
'( 1' + #13#10 +
'  2' + #13#10 +
'  #\A' + #13#10 +
'  #(2.1' + #13#10 +
'    2.2' + #13#10 +
'    ( 2.21' + #13#10 +
'      "abc"' + #13#10 +
'      symbol' + #13#10 +
'      #t' + #13#10 +
'      #f' + #13#10 +
'      #m(41 42 43)' + #13#10 +
'      2.22))' + #13#10 +
'  3)';

  DispText = '12A2.12.22.21abcsymbol#t#fABC2.223';

  RndText =
'( 1,' + #13#10 +
'  2,' + #13#10 +
'  ''A'',' + #13#10 +
'  [ 2.1,' + #13#10 +
'    2.2,' + #13#10 +
'    ( 2.21,' + #13#10 +
'      "abc",' + #13#10 +
'      symbol,' + #13#10 +
'      true,' + #13#10 +
'      false,' + #13#10 +
'      #m(41 42 43),' + #13#10 +
'      2.22)],' + #13#10 +
'  3)';

{ TestTDynOutPortD }

procedure TestTDynOutPortD.SetUp;
begin
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPort.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
end;

procedure TestTDynOutPortD.TearDown;
begin
  Ref := nil; //FDynOutPort.Free;
  FDynOutPort := nil;
end;

procedure TestTDynOutPortD.TestFromLisp;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  Res: TDatumRef;
  Parser: TLispParser;
begin
  // TODO: Setup method call parameters
  ManageRefs([@Res]);

  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;

  Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(DispText, ResText, 'Error')
end;


{ TestTDynOutPortLisp }

procedure TestTDynOutPortLisp.SetUp;
begin
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortLisp.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
end;

procedure TestTDynOutPortLisp.TearDown;
begin
  Ref := nil; //FDynOutPort.Free;
  FDynOutPort := nil;
end;

procedure TestTDynOutPortLisp.TestFromLisp;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  Res: TDatumRef;
  Parser: TLispParser;
begin
  // TODO: Setup method call parameters
  ManageRefs([@Res]);

  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;

  Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(SrcText, ResText, 'Error')
end;

procedure TestTDynOutPortLisp.TestFromLispA;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  Res: TDatumRef;
  Parser: LispParserA.TLispParser;
  SrcTextA: Utf8String;
begin
  // TODO: Setup method call parameters
  ManageRefs([@Res]);

  Parser := LispParserA.TLispParser.Create;
  SrcTextA := Utf8String(SrcText);
  Parser.Evaluate(Res, SrcTextA);
  Parser.Free;

  Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(SrcText, ResText, 'Error')
end;


procedure TestTDynOutPortLisp.TestFromRnd;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  //Res: dyn;
  Parser: TParser;
begin
  // TODO: Setup method call parameters
  //ManageRefs([@Res]);

  Parser := TParser.Create(RndText);
  Parser.GetNextTerm(Obj);
  Parser.Free;

  //Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(SrcText, ResText, 'Error')
end;

{ TestTDynOutPortRnd }

procedure TestTDynOutPortRnd.SetUp;
begin
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortRnd.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;
end;

procedure TestTDynOutPortRnd.TearDown;
begin
  Ref := nil; //FDynOutPort.Free;
  FDynOutPort := nil;
end;

procedure TestTDynOutPortRnd.TestFromLisp;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  Res: TDatumRef;
  Parser: TLispParser;
begin
  // TODO: Setup method call parameters
  ManageRefs([@Res]);

  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;

  Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RndText, ResText, 'Error')
end;

procedure TestTDynOutPortRnd.TestFromLispA;
var
  ReturnValue: Boolean;
  Obj: dyn;
  ResText: string;
  Res: TDatumRef;
  Parser: LispParserA.TLispParser;
  SrcTextA: Utf8String;
begin
  // TODO: Setup method call parameters
  ManageRefs([@Res]);

  Parser := LispParserA.TLispParser.Create;
  SrcTextA := Utf8String(SrcText);
  Parser.Evaluate(Res, SrcTextA);
  Parser.Free;

  Obj := Res.Value;

  ReturnValue := FDynOutPort.Write(Obj);
  ResText := FTextOut.GetText;

  // TODO: Validate method results
  Check(ReturnValue, 'Write');
  CheckEquals(RndText, ResText, 'Error')
end;

initialization
  {$if Declared(FormatSettings)}FormatSettings.{$ifend} DecimalSeparator := '.';
  // Register any test cases with the test runner
  RegisterTest(TestTDynOutPortD.Suite);
  RegisterTest(TestTDynOutPortLisp.Suite);
  RegisterTest(TestTDynOutPortRnd.Suite);
end.


