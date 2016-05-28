program Rnd;

uses
  Vcl.Forms,
  FrRnd in 'FrRnd.pas' {Form2},
  RndBase in '..\common\RndBase.pas',
  RndParser in '..\common\RndParser.pas',
  RndClass in '..\common\RndClass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
