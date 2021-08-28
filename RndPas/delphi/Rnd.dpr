program Rnd;

uses
  Vcl.Forms,
  DUtils in '..\..\Api\DUtils.pas',
  DTDatum in '..\..\Api\DType\DTDatum.pas',
  DTBool in '..\..\Api\DType\DTBool.pas',
  DTInt in '..\..\Api\DType\DTInt.pas',
  DTFloat in '..\..\Api\DType\DTFloat.pas',
  DTPair in '..\..\Api\DType\DTPair.pas',
  DTArray in '..\..\Api\DType\DTArray.pas',
  MapFiles in '..\..\Api\DType\MapFiles.pas',
  DTString in '..\..\Api\DType\DTString.pas',
  DTSymbol in '..\..\Api\DType\DTSymbol.pas',
  DTProc in '..\..\Api\DType\DTProc.pas',
  DTProcRTTI in '..\..\Api\DType\DTProcRTTI.pas',
  DTPort in '..\..\Api\DType\DTPort.pas',
  DTPortA in '..\..\Api\DType\DTPortA.pas',
  DTPortW in '..\..\Api\DType\DTPortW.pas',
  DTFile in '..\..\Api\DType\DTFile.pas',
  DTScope in '..\..\Api\DType\DTScope.pas',
  DTScript in '..\..\Api\DType\DTScript.pas',
  DTParser in '..\..\Api\DType\DTParser.pas',
  DynTypes in '..\..\Api\DynTypes.pas',
  LispWrite in '..\..\Api\Lisp\LispWrite.pas',
  LispLexer in '..\..\Api\Lisp\LispLexer.pas',
  LispLexerA in '..\..\Api\Lisp\LispLexerA.pas',
  LispParser in '..\..\Api\Lisp\LispParser.pas',
  LispParserA in '..\..\Api\Lisp\LispParserA.pas',
  LispEval in '..\..\Api\Lisp\LispEval.pas',
  LispEnv in '..\..\Api\Lisp\LispEnv.pas',
  LispFiles in '..\..\Api\Lisp\LispFiles.pas',
  LispInterpreter in '..\..\Api\Lisp\LispInterpreter.pas',
  RndWrite in '..\..\Api\Rnd\RndWrite.pas',
  RndLexer in '..\..\Api\Rnd\RndLexer.pas',
  RndParser in '..\..\Api\Rnd\RndParser.pas',
  rnrs_lists in '..\common\rnrs_lists.pas',
  SchFunc in '..\common\SchFunc.pas',
  LispFunc in '..\common\LispFunc.pas',
  LispSintax in '..\common\LispSintax.pas',
  RndSintax in '..\common\RndSintax.pas',
  Core in '..\common\Core.pas',
  FrRnd in 'FrRnd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.


