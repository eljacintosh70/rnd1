unit LispEval;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils, Classes,
  DynTypes, DTArray, DTProc, DTProcRTTI, DTScope;

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

procedure EvalList(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
procedure EvalCall(out Result: TDatumRef; Fn, Params: TDynDatum; Scope: IDynScope);
procedure EvalVector(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
procedure EvalSymbol(out Result: TDatumRef; Symbol: TDynDatum; Scope: IDynScope);
procedure EvalParams(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
//procedure Eval(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);

implementation /////////////////////////////////////////////////////////////////

type
  PFunctionTable = ^TFunctionTable;
  PFunctionInfo = ^TFunctionInfo;

  TFunctionInfo = packed record
    Len: Word;
    Def: Pointer;  // dirección
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
  {case Datum.Kind of
    atPair:
      EvalList(Result, Datum, Scope);
    atVector:
      EvalVector(Result, Datum, Scope);
    atSymbol:
      EvalSymbol(Result, Datum, Scope);
    else
      Result := (Datum);
  end }
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

procedure EvalList(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  Fn, Params: TDynDatum;
begin
  Fn := car(Datum);
  Params := cdr(Datum);
  EvalCall(Result, Fn, Params, Scope);
end;

procedure EvalCall(out Result: TDatumRef; Fn, Params: TDynDatum; Scope: IDynScope);
var
  Sintax: TDynSyntax;

  FnRef: TDatumRef;
  Par: TDatumRef;
  Res: TDatumRef;
begin
  ManageRefs([@FnRef, @Par, @Res]);
  Eval(FnRef, Fn, Scope);
  Fn := FnRef.Value;
  case Fn.Kind of
    atSyntax:
      begin
        Sintax := AsSintax(Fn);
        Sintax.Eval(Result, Params, Scope);
      end;
    atExtFunc:
      begin
        Par := (Params);
        EvalParams(Res, Par.Value, Scope);
        TDynFuncRTTI(Fn).Call(Result, Res.Value);
      end;
    atAutoFunc:
      begin
        Par := (Params);
        EvalParams(Res, Par.Value, Scope);
        TDynFuncAuto(Fn).Call(Result, Res.Value);
      end;
    atLambda:
      begin
        Par := (Params);
        EvalParams(Res, Par.Value, Scope);
        TDynLambda(Fn).Call(Result, Res.Value);
      end;
    else // debería ser un error
      Result := (Fn);
  end;
end;

procedure EvalVector(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  A: TDatumRef;
  Src, Des: TAbstractDynArray;
  i, n: Integer;
begin
  ManageRefs([@A]);

  Src := TAbstractDynArray(Datum);
  n := Src.length;
  Des := TDynArray.Create(n);
  for i := 0 to n - 1 do
  begin
    Eval(A, Src[i], Scope);
    Des[i] := A.Value;
  end;
  Result := (Des);
end;


procedure EvalSymbol(out Result: TDatumRef; Symbol: TDynDatum; Scope: IDynScope);
var
  Val: TDynDatum;
begin
  while Assigned(Scope) do
  begin
    Val := Scope.Value[Symbol];
    if Val <> Unbound then
    begin
      Result := (Val);
      Exit;
    end;
    Scope := Scope.Parent;
  end;
  Result := (Unbound);
end;

procedure EvalParams(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  A, D: TDatumRef;
begin
  ManageRefs([@A, @D]);

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
var
  p: PAnsiChar;
  i, n: Integer;
  pEntry: PFunctionInfo;
  Datum: TDynDatum;
  Method: TSintaxMethod;
  Name: Utf8String;
  NameW: string;
begin
  p := Pointer(List.ClassType);
  Inc(p, vmtMethodTable);
  p := PPointer(p)^;
  if not Assigned(p) then Exit;

  n := PFunctionTable(p).Len;
  p := @PFunctionTable(p).Entry;
  for i := 0 to n - 1 do
  begin
    pEntry := PFunctionInfo(p);
    TMethod(Method).Code := pEntry.Def;
    TMethod(Method).Data := List;
    Name := pEntry.Name;
    if PAnsiChar(Name)^ = '_' then
      SimplifyName(Name);
    NameW := string(Name);
    Datum := SyntaxDatum(NameW, Method);
    Define(Name, Datum);
    Inc(p, pEntry.Len);
  end;
end;

procedure TLispEval.Define(const Name: UTF8String; AValue: TDynDatum);
var
  Key: TDynDatum;
begin
  Key := InitSymbol(Name);
  Value[Key] := AValue;
end;

procedure TLispEval.RegisterFunctions(List: TObject);
var
  p: PAnsiChar;
  i, n: Integer;
  pMethodTable: PFunctionTable;
  pMethodEntry: PFunctionInfo;
  AutoTable: PAutoTable;
  AutoEntry: PAutoEntry;
  Datum: TDynDatum;
  Method: TMethod;
  Name: UTF8String;
begin
  p := Pointer(List.ClassType());
  Inc(p, vmtMethodTable);
  pMethodTable := PPointer(p)^;
  if Assigned(pMethodTable) then
  begin
    n := pMethodTable.Len;
    p := @pMethodTable.Entry;
    for i := 0 to n - 1 do
    begin
      pMethodEntry := PFunctionInfo(p);
      TMethod(Method).Code := pMethodEntry.Def;
      TMethod(Method).Data := List;
      Datum := ExtFuncDatum(pMethodEntry, Method);
      Name := pMethodEntry.Name;
      if PAnsiChar(Name)^ = '_' then
        SimplifyName(Name);
      Define(Name, Datum);
      Inc(p, pMethodEntry.Len);
    end;
  end;

  p := Pointer(List.ClassType);
  Inc(p, vmtAutoTable);
  AutoTable := PPointer(p)^;
  if Assigned(AutoTable) then
  begin
    n := AutoTable.Len;
    for i := 0 to n - 1 do
    begin
      AutoEntry := @AutoTable.Entry[i];
      TMethod(Method).Code := AutoEntry.MethodAddr;
      TMethod(Method).Data := List;
      Datum := AutoFuncDatum(AutoEntry, Method);
      Name := AutoEntry.Name^;
      if PAnsiChar(Name)^ = '_' then
        SimplifyName(Name);
      Define(Name, Datum);
    end;
  end;
end;

procedure TLispEval.Rename(const OldNames, NewNames: array of UTF8String);
var
  i, n: Integer;
  //Index: Integer;
  Datum: TDynDatum;
  OldNameRef: TDatumRef;
  NewNameRef: TDatumRef;
  Name: TDynDatum;
begin
  ManageRefs([@OldNameRef, @NewNameRef]);

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
begin
  n := List.Count;
  Result := TDynArray.Create(n).AsIDynArray;
  for i := 0 to n - 1 do
  begin
    Item := make_list([List.Strings[i], List.Objects[i]]);
    Result.Item[i] := Pointer(Item);
  end;
end;

end. ///////////////////////////////////////////////////////////////////////////

