unit LispEval;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils, Classes,
  DynTypes, DTArray, DTProc, DTScope;

type
  TLispEval = class(TBigScope, ILibScope, IDelphiScope)
  protected
    FImported: IDynDatum;
    function Imported: IDynDatum;
    procedure Import(Lib: IDynDatum);
    procedure Define(const Name: UTF8String; AValue: TDynDatum);
  public
    procedure RegisterSintax(List: TObject);
    procedure RegisterFunctions(List: TObject);
    procedure Rename(const OldNames, NewNames: array of UTF8String);
    function Symbols: IDynArray;
  end;

procedure EvalParams(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);

implementation /////////////////////////////////////////////////////////////////

type
  PFunctionTable = ^TFunctionTable;
  PFunctionInfo = ^TFunctionInfo;

  TFunctionInfo = packed record
    Len: Word;
    Def: Pointer;  // direcci�n
    Name: ShortString;
    Parms: record end; // en Delphi 200x
  end;

  TFunctionTable = packed record
    Len: Word;
    Entry: TFunctionInfo;
  end;

procedure Eval(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
begin
  Result := DynTypes.Eval(Datum, Scope)
end;

procedure SimplifyName(var Name: Utf8String);
var
  i, n: Integer;
begin
  n := Length(Name);
  if n < 2 then Exit;
  if Name[1] <> '_' then Exit;
  Name := Copy(Name, 2, n - 1);
  Dec(n);
  if (n > 2) and (Name[n - 1] = '_') then
  begin
    case Name[n] of
      'P', 'p':
        Name := Copy(Name, 1, n - 2) + '?';
      'X', 'x':
        Name := Copy(Name, 1, n - 2) + '!';
    end;
  end;
  for i := 1 to Length(Name) do
    if Name[i] = '_' then
      Name[i] := '-';
end;

procedure EvalParams(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  A, D: TDatumRef;
begin
  Result := (Datum);
  if IsPair(Datum) then
  begin
    Eval(A, car(Datum), Scope);
    Datum := cdr(Datum);
    if Assigned(Datum) then
    begin
      EvalParams(D, Datum, Scope);
      Result.cons(A.Value, D.Value);
    end
    else
      Result.cons(A.Value, nil);
  end;
end;

{ TLispEval }

function TLispEval.Imported: IDynDatum;
begin
  Result := FImported;
end;

procedure TLispEval.Import(Lib: IDynDatum);
begin
  FImported := cons(Lib, FImported);
end;

procedure TLispEval.RegisterSintax(List: TObject);
begin
end;

procedure TLispEval.Define(const Name: UTF8String; AValue: TDynDatum);
var
  Key: dyn;
begin
  Key := InitSymbol(Name);
  Value[Key] := AValue;
end;

procedure TLispEval.RegisterFunctions(List: TObject);
begin
end;

procedure TLispEval.Rename(const OldNames, NewNames: array of UTF8String);
var
  i, n: Integer;
  Datum: TDynDatum;
  OldNameRef: TDatumRef;
  NewNameRef: TDatumRef;
  Name: TDynDatum;
begin
  n := Length(Oldnames);
  if Length(NewNames) < n then
    n := Length(NewNames);
  for i := 0 to n - 1 do
  begin
    OldNameRef := (InitSymbol(OldNames[i]));
    Name := OldNameRef.Value;
    Datum := Value[Name];
    if Datum <> Unbound then
    begin
      NewNameRef := (InitSymbol(NewNames[i]));
      Name := NewNameRef.Value;
      Value[Name] := Datum;
      Name := OldNameRef.Value;
      Value[Name] := Unbound;
    end
  end;
end;

function TLispEval.Symbols: IDynArray;
var
  i, n: Integer;
  Item: IDynPair;
  Arr: TKVArray;
begin
  Arr := List.ToArray;
  n := Length(Arr);
  Result := TDynArray.Create(n).AsIDynArray;
  for i := 0 to n - 1 do
  begin
    Item := DynTypes.list([Arr[i].Key, Arr[i].Value]);
    Result.Item[i] := Pointer(Item);
  end;
end;

end. ///////////////////////////////////////////////////////////////////////////

