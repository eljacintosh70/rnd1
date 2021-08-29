unit TestFiles;

interface

uses
  SysUtils, Classes;

function LoadTestFile(Name: string): string;

implementation

var
  TestFilePath: string;

procedure InitTestFilePath;
var
  Path: string;
begin
  Path := ExtractFilePath(ParamStr(0));
  Path := ExcludeTrailingPathDelimiter(Path);
  Path := ExtractFilePath(Path);
  Path := ExcludeTrailingPathDelimiter(Path);
  Path := ExtractFilePath(Path);
  TestFilePath := Path + 'Lib\Test\';
end;

function LoadTestFile(Name: string): string;
var
  Path: string;
  SL: TStringList;
  n: Integer;
begin
  if TestFilePath = '' then
    InitTestFilePath;

  Path := TestFilePath + Name;
  SL := TStringList.Create;
  SL.LoadFromFile(Path);
  Result := SL.Text;
  SL.Free;

  n := Length(Result);
  if n > 2 then
    if Copy(Result, n - 1, 2) = #13#10 then
      SetLength(Result, n - 2);
end;

initialization
  InitTestFilePath;
end.

