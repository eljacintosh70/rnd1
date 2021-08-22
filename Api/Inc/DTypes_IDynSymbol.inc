function InitSymbol(pName: PAnsiChar; {Utf8} cbName: Integer): TDynDatum;
begin
  Result := SymbolSource.CreateSymbol(pName, cbName);
end;

procedure InitSymbols(const Names: array of Utf8String; const Ref: array of
    PISymbol);
var
  i, n: Integer;
  n2: Integer;
begin
  n := Length(Names);
  n2 := Length(Ref);
  if n <> n2 then
    DynError('InitSymbols: diferent length is parameters %d %d', [n, n2]);
  for i := 0 to n - 1 do
    Ref[i]^ := InitSymbolI(Names[i]);
end;

exports
  InitSymbols(const Names: array of Utf8String; const Ref: array of PISymbol) name 'Symbol.Create*',
  InitSymbol(pName: PAnsiChar; cbName: Integer)    name 'Symbol.Create';
