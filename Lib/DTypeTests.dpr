program DTypeTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  DUtils in '..\Api\DUtils.pas',
  DTDatum in '..\Api\DType\DTDatum.pas',
  DTBool in '..\Api\DType\DTBool.pas',
  DTInt in '..\Api\DType\DTInt.pas',
  DTFloat in '..\Api\DType\DTFloat.pas',
  DTPair in '..\Api\DType\DTPair.pas',
  DTArray in '..\Api\DType\DTArray.pas',
  MapFiles in '..\Api\DType\MapFiles.pas',
  DTString in '..\Api\DType\DTString.pas',
  DTSymbol in '..\Api\DType\DTSymbol.pas',
  DTProc in '..\Api\DType\DTProc.pas',
  DTProcRTTI in '..\Api\DType\DTProcRTTI.pas',
  DTPort in '..\Api\DType\DTPort.pas',
  DTPortA in '..\Api\DType\DTPortA.pas',
  DTPortW in '..\Api\DType\DTPortW.pas',
  DTFile in '..\Api\DType\DTFile.pas',
  DTScope in '..\Api\DType\DTScope.pas',
  DTScript in '..\Api\DType\DTScript.pas',
  DTParser in '..\Api\DType\DTParser.pas',
  DynTypes in '..\Api\DynTypes.pas',
  LispWrite in '..\Api\Lisp\LispWrite.pas',
  LispLexer in '..\Api\Lisp\LispLexer.pas',
  LispLexerA in '..\Api\Lisp\LispLexerA.pas',
  LispParser in '..\Api\Lisp\LispParser.pas',
  LispParserA in '..\Api\Lisp\LispParserA.pas',
  LispEval in '..\Api\Lisp\LispEval.pas',
  LispEnv in '..\Api\Lisp\LispEnv.pas',
  LispFiles in '..\Api\Lisp\LispFiles.pas',
  LispInterpreter in '..\Api\Lisp\LispInterpreter.pas',
  RndWrite in '..\Api\Rnd\RndWrite.pas',
  RndLexer in '..\Api\Rnd\RndLexer.pas',
  RndParser in '..\Api\Rnd\RndParser.pas',
  Test_DTPort in 'Test\Test_DTPort.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.


