unit TestFiles;

interface

uses
  SysUtils, Classes;

function LoadTestFile(Name: string): string;
procedure SaveTestFile(Name, Data: string);
function NormalizeLines(s: string): string;

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
  TestFilePath := Path + {$IFDEF LINUX} 'Lib/Test/' {$ELSE} 'Lib\Test\' {$ENDIF};
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

procedure SaveTestFile(Name, Data: string);
var
  Path: string;
  SL: TStringList;
  n: Integer;
begin
  if TestFilePath = '' then
    InitTestFilePath;

  Path := TestFilePath + Name;
  SL := TStringList.Create;  
  SL.Text := Data;
  SL.SaveToFile(Path);
  SL.Free;
end;

function NormalizeLines(s: string): string;
var
  SL: TStringList;   
  n: Integer;
begin
  SL := TStringList.Create;
  SL.Text := s;
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

