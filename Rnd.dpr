program Rnd;

uses
  Vcl.Forms,
  FrRnd in 'FrRnd.pas' {Form2},
  RndBase in 'RndBase.pas',
  RndParser in 'RndParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
