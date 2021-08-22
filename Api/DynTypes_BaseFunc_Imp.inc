
var
  DynFalse: IDyn;
  DynTrue: IDyn;

function FirstMakeBool(Value: Boolean): IDyn;
var
  Obj: TCustomBool;
begin
  Obj := TFalseObj.Create;
  DynFalse := IDynDatum(Obj);
  Obj := TTrueObj.Create;
  DynTrue := IDynDatum(Obj);
  if Value then
    Result := DynTrue
  else
    Result := DynFalse;
end;

function MakeBool(Value: Boolean): IDyn;
begin
  if Value then
    Result := DynTrue
  else
    Result := DynFalse;
  if not Assigned(Result) then
    Result := FirstMakeBool(Value);
end;

function MakeChar(Value: Char): IDyn;
var
  Obj: TDynChar;
begin
  Obj := TDynChar.Create(Value);
  Result := IDynDatum(Obj);
end;

function MakeInt64(Value: Int64): IDyn;
var
  Obj: TDynInt64;
begin
  Obj := TDynInt64.Create(Value);
  Result := IDynInt(Obj);
end;

function MakeDouble(Value: Double): IDyn;
var
  Obj: TDynFloat;
begin
  Obj := TDynFloat.Create(Value);
  Result := IDynFloat(Obj);
end;

function MakeString(Value: string): IDyn;
begin
  Result := TDynString.CreateI(PWideChar(Value), Length(Value));
end;

function DatumKind(Val: Integer): TDatumType;
begin
  Result := atInvalid;
  case (Val and StorageMask) of
    smInterface:
      Result := atOpaque;
    smInteger:
      Result := atInteger;
    smInline:
      case (Val and $FF) of
        akChar: Result := atChar;
        akBool: Result := atBool;
        akNil : Result := atNil;
        akUnbound: Result := atUnbound;
        akUndefined: Result := atUndefined;
      end
  end;
end;

function ValToRefType(A: weak_IDyn): IDyn;
var
  Val: NativeInt;
begin
  Val := NativeInt(Pointer(A));
  case DatumKind(Val) of
    atInteger: Result := MakeInt64(FixNumValue(Pointer(A)));
    atChar:    Result := MakeChar(WideChar(Val shr 8));
    atBool:    Result := MakeBool(Pointer(A) <> _f);
    atNil, atUnbound, atUndefined:
               Result := nil;
  else         Result := IDyn(Pointer(A));
  end;
end;

exports
  MakeBool     name 'MakeBool',
  MakeChar     name 'MakeChar',
  MakeInt64    name 'MakeInt64',
  MakeDouble   name 'MakeDouble',
  MakeString   name 'MakeString',
  ValToRefType name 'ValToRefType';


