
function make_string(pData: PAnsiChar; cbData: Integer): IDynString;
begin
  Result := TDynStringA.CreateI(pData, cbData)
end;

function make_string(pData: PWideChar; cbData: Integer): IDynString;
begin
  Result := TDynString.CreateI(pData, cbData)
end;

function CreateStringANR(p: PAnsiChar; cb: Integer): TDynDatum;
begin
  Result := Pointer(TDynStringA.Create(p, cb));
end;

function CreateStringWNR(p: PWideChar; cb: Integer): TDynDatum;
begin
  Result := Pointer(TDynString.Create(p, cb));
end;

exports
  CreateStringANR name 'StrA->',
  CreateStringWNR name 'StrW->',

  make_string(pData: PAnsiChar; cbData: Integer)  name 'make_stringA',
  make_string(pData: PWideChar; cbData: Integer)  name 'make_stringW';

