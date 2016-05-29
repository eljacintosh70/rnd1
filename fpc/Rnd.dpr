program Rnd;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  RndBase in '../common/RndBase.pas',
  RndClass in '../common/RndClass.pas',
  RndParser in '../common/RndParser.pas',
  FrRnd in '../common/FrRnd.pas' {Form2};

{.$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
