unit SchFunc;

interface

uses
  TypInfo, SysUtils,
  DynTypes;

const
  Undefined = TDynDatum(smInline or $40);

function eqv(A, B: TDynDatum): Boolean;
function equal(A, B: TDynDatum): Boolean;
function list_equal(A, B: dyn): Boolean;
function vector_equal(const A, B: IDynArray): Boolean;

procedure NeedType(Val: TDynDatum; t: TDatumType);

// boolean? indica si el átomo es #t o #f
function IsBoolean(Datum: TDynDatum): Boolean; {$ifdef INLINE} inline; {$endif}
// char? Código de caracter unicode.
function IsChar(Datum: TDynDatum): Boolean; {$ifdef INLINE} inline; {$endif}
// number? Cualquier valor numérico.
function IsNum(Datum: TDynDatum): Boolean;
  function IsInteger(Datum: TDynDatum): Boolean;
  function IsReal(Datum: TDynDatum): Boolean;
// record? Record
function IsRecord(Datum: TDynDatum): Boolean;
// procedure? Función que toma valores y retorna un resultado.
function IsProcedure(Datum: TDynDatum): Boolean;

implementation

function eqv(A, B: TDynDatum): Boolean;
var
  KindA, KindB: TDatumType;
begin
  if Pointer(A) <> Pointer(B) then
  begin
    KindA := A.Kind;
    KindB := B.Kind;
    Result := False;
    if KindA = KindB then
      case KindA of
        atInteger:
          Result := (Int64Value(A) = Int64Value(B));
        atReal:
          Result := (FloNumValue(A) = FloNumValue(B));
      end;
  end
  else
    Result := True;
end;

function equal(A, B: TDynDatum): Boolean;
var
  KindA, KindB: TDatumType;
begin
  if Pointer(A) <> Pointer(B) then
  begin
    KindA := A.Kind;
    KindB := B.Kind;
    Result := False;
    if KindA = KindB then
      case KindA of
        atInteger:
          Result := (Int64Value(A) = Int64Value(B));
        atReal:
          Result := (FloNumValue(A) = FloNumValue(B));
        atString:
          Result := IDynString(Pointer(A)).AsString = IDynString(Pointer(B)).AsString;
        atPair:
          Result := list_equal(A, B);
        atVector:
          Result := vector_equal(IDynArray(Pointer(A)), IDynArray(Pointer(B)));
      end;
  end
  else
    Result := True;
end;

function list_equal(A, B: dyn): Boolean;
var
  enA, enB: IDynEnumerator;
begin
  Result := False;
  enA := A.GetEnumerator;
  enB := B.GetEnumerator;

  while enA.MoveNext do
  begin
    if not enB.MoveNext then    // (a b c) (a b)
      Exit;
    if not equal(enA.Current, enB.Current) then  // (a b c) (a b x)
      Exit;
  end;
  if enB.MoveNext then          // (a b)   (a b c)
    Exit;
  Result := True;
end;

function vector_equal(const A, B: IDynArray): Boolean;
var
  i, nA, nB: Integer;
begin
  Result := False;
  nA := A.Length;
  nB := B.Length;
  if nA <> nB then
    Exit;
  for i := 0 to nA - 1 do
    if A[i] <> B[i] then
      Exit;
  Result := True;
end;

procedure NeedType(Val: TDynDatum; t: TDatumType);
var
  s: String;
  Info: Pointer;
begin
  if Val.Kind <> t then
  begin
    Info := TypeInfo(TDatumType);
    s := Format('%s Required but %s found.', [GetEnumName(Info, Ord(t)),
      GetEnumName(Info, Ord(Val.Kind))]);
    raise Exception.Create(s);
  end;
end;

function IsBoolean(Datum: TDynDatum): Boolean;
begin
  Result := (Datum = _f) or (Datum = _t);
end;

function IsChar(Datum: TDynDatum): Boolean;
begin
  Result := (Datum.Kind = atChar)
end;

function IsNum(Datum: TDynDatum): Boolean;
begin
  case Datum.Kind of
    atReal, atInteger:
      Result := True;
    else
      Result := False;
  end;
end;

function IsInteger(Datum: TDynDatum): Boolean;
begin
  case Datum.Kind of
    atInteger:
      Result := True;
    else
      Result := False;
  end;
end;

function IsReal(Datum: TDynDatum): Boolean;
begin
  case Datum.Kind of
    atInteger, atReal:
      Result := True;
    else
      Result := False;
  end;
end;

function IsRecord(Datum: TDynDatum): Boolean;
begin
  Result := Datum.Kind = atRecord;
end;

function IsProcedure(Datum: TDynDatum): Boolean;
begin
  case Datum.Kind of
    atExtFunc, atAutoFunc, atLambda:
      Result := True;
    else
      Result := False;
  end
end;

end.

