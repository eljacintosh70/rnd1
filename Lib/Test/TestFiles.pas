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

// remover el salto de linea extra que agrega TStrings.Text
// puede ser #13#10 (Windows) o #10 (Linux)
function RemoveExtraLine(const s: string): string;
var
  n: Integer;
begin
  Result := s;
  n := Length(Result);
  if n >= 1 then
    if Result[n] = #10 then
    begin
      if (n >= 2) and (Result[n - 1] = #13) then
        SetLength(Result, n - 2)
      else
        SetLength(Result, n - 1)
    end
end;

function LoadTestFile(Name: string): string;
var
  Path: string;
  SL: TStringList;
begin
  if TestFilePath = '' then
    InitTestFilePath;

  Path := TestFilePath + Name;
  SL := TStringList.Create;
  SL.LoadFromFile(Path);
  Result := RemoveExtraLine(SL.Text);
  SL.Free;
end;

procedure SaveTestFile(Name, Data: string);
var
  Path: string;
  SL: TStringList;
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
begin
  SL := TStringList.Create;
  SL.Text := s;
  Result := RemoveExtraLine(SL.Text);
  SL.Free;
end;


initialization
  InitTestFilePath;
end.

