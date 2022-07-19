
function make_string(pData: PAnsiChar; cbData: Integer): IDynString;
begin
  Result := TDynStringA.CreateI(pData, cbData)
end;

function make_string(pData: PWideChar; cbData: Integer): IDynString;
begin
  Result := TDynString.CreateI(pData, cbData)
end;

{$IFNDEF FPC}
exports
  make_string(pData: PAnsiChar; cbData: Integer)  name 'make_stringA',
  make_string(pData: PWideChar; cbData: Integer)  name 'make_stringW';
{$ENDIF}

