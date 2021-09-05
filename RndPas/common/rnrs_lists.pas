// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-4.html
// List utilities

// http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html
// 11.9  Pairs and lists
unit rnrs_lists;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, SchFunc;

type
  TDynDatumFunc = function (Datum: TDynDatum): IDynDatum of object;
  TDynDatumPredicate = function (Datum: TDynDatum): Boolean of object;

  TCompareHelper = class
  private
    function EqualInteger(B: TDynDatum): Boolean;
    function EqualList(B: TDynDatum): Boolean;
    function EqualPtr(B: TDynDatum): Boolean;
    function EqualReal(B: TDynDatum): Boolean;
    function EqualString(B: TDynDatum): Boolean;
    function EqualVector(B: TDynDatum): Boolean;
  public
    class function EqualFunc(A: TDynDatum): TDynDatumPredicate;
  end;

// (map proc list1 list2 ...)
// The lists should all have the same length. Proc should accept as many arguments as there are lists and return a single value. Proc should not mutate any of the lists.
function Map1(proc: TDynDatumFunc; List: IDynPair): IDynPair;
function Map2(proc: IDynFunc; List: IDynPair): IDynPair;

{
(find proc list)
(for-all proc list1 list2 ... listn)
(exists proc list1 list2 ... listn)
(filter proc list)
(partition proc list)
(fold-left combine nil list1 list2 ...listn)
(fold-right combine nil list1 list2 ...listn)

(remp proc list)
  (remove obj list) => (remp (lambda (x) (equal? obj x)) list)
  (remv obj list)   => (remp (lambda (x) (eqv? obj x)) list)
  (remq obj list)   => (remp (lambda (x) (eq? obj x)) list)
}
function list_remq(Obj: Pointer; const List: IDynPair): IDynPair;

{
(memp proc list)
  (member obj list) => (memp (lambda (x) (equal? obj x)) list)
  (memv obj list)   => (memp (lambda (x) (eqv? obj x)) list)
  (memq obj list)   => (memp (lambda (x) (eq? obj x)) list)
}
function memq(obj: TDynDatum; list: IDynPair): Boolean;

{
(assp proc alist)
  (assoc obj alist) => (assp (lambda (x) (equal? obj x)) alist)
  (assv obj alist)  => (assp (lambda (x) (eqv? obj x)) alist)
  (assq obj alist)  => (assp (lambda (x) (eq? obj x)) alist)
}
function Assp(proc: TDynDatumPredicate; List: IDynPair): TDynDatum;
function Assoc(Key: TDynDatum; List: IDynPair): TDynDatum;
function Assq(Key: TDynDatum; List: IDynPair): TDynDatum;

function HasAssq(Key: TDynDatum; List: IDynPair; out Res: TDynDatum): Boolean;

{
(cons* obj1 ... objn rest)  => (obj1 ... objn . rest)
(cons* rest)                => rest
}

// (length list)
function list_length(list: TDynDatum): Integer;
// (list->vector list)
function list_Vector(list: TDynDatum): IDynArray;
// (list->vector-R list)
function list_VectorR(list: TDynDatum): IDynArray;
// (append list ... obj)
  procedure Prepend(var Dest: IDynPair; const List: IDynPair);
function list_append(Lists: array of IDynPair; Obj: IDynDatum): IDynPair;
function VectorToList(Vec: IDynArray; iMin, iMax: Integer): IDynPair;


implementation /////////////////////////////////////////////////////////////////

//uses
//  DTVector;

function Map1(proc: TDynDatumFunc; List: IDynPair): IDynPair;
var
  Item, cdr: TDynDatum;
  Tmp: IDynDatum;
begin
  Result := nil;
  while Assigned(List) do
  begin
    Item := List.car;
    Tmp := proc(Item);
    Result := cons(Tmp, Result);
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := Reverse(Result);
end;

function Map2(proc: IDynFunc; List: IDynPair): IDynPair;
var
  Item, cdr: TDynDatum;
  Tmp: IDynDatum;
  Res: TDatumRef;
begin
  Result := nil;
  while Assigned(List) do
  begin
    Item := List.car;
    proc.Call(Res, Pointer(cons(Item, nil)));
    Tmp := IDynDatum(Pointer(Res.Value));
    Result := cons(Tmp, Result);
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := Reverse(Result);
end;

function aux_remq(var Res: IDynPair; const List: IDynPair; Obj: Pointer): Boolean;
var
  First: Pointer;
begin  // List no debe ser nil;
  First := car(List);
  if First = Obj then
  begin
    Res := IDynPair(Pointer(List.cdr));
    Result := True;
  end
  else
  begin
    Result := aux_remq(Res, List, Obj);
    if Result then
      Res := cons(IDynDatum(First), Res)
  end;
end;

// (remq obj list)
function list_remq(Obj: Pointer; const List: IDynPair): IDynPair;
begin
  if not aux_remq(Result, List, Obj) then
    Result := List;
end;

function memq(obj: TDynDatum; list: IDynPair): Boolean;
var
  Item: TDynDatum;
begin
  while GetNext(list, Item) do
  begin
    if Item = obj then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


{ TODO : REVISAR!!! }
function Assp(proc: TDynDatumPredicate; List: IDynPair): TDynDatum;
var
  cdr: TDynDatum;
begin
  while Assigned(List) do
  begin
    Result := List.car;
    if Result.Kind = atPair then
      if proc(IDynPair(Pointer(Result)).car) then
        Exit;
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := nil;
end;
{
function Assoc(Key: TDynDatum; List: IDynPair): TDynDatum;
var
  proc: TDynDatumPredicate;
begin
  proc := TCompareHelper.EqualFunc(Key);
  Result := Assp(proc, List);
end;
}
function Assoc(Key: TDynDatum; List: IDynPair): TDynDatum;
var
  Item: TDynDatum;
  cdr: TDynDatum;
begin
  while Assigned(List) do
  begin
    Item := List.car;
    if Item.Kind = atPair then
      if Equal(Key, car(Item)) then
      begin
        Result := Item;
        Exit;
      end;
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := nil;
end;

function Assq(Key: TDynDatum; List: IDynPair): TDynDatum;
var
  Item: TDynDatum;
  cdr: TDynDatum;
begin
  while Assigned(List) do
  begin
    Item := List.car;
    if Item.Kind = atPair then
      if (Key = car(Item)) then
      begin
        Result := Item;
        Exit;
      end;
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := nil;
end;

function HasAssq(Key: TDynDatum; List: IDynPair; out Res: TDynDatum): Boolean;
var
  Item: TDynDatum;
  cdr: TDynDatum;
begin
  while Assigned(List) do
  begin
    Item := List.car;
    if Item.Kind = atPair then
      if (Key = car(Item)) then
      begin
        Res := Item;
        Result := True;
        Exit;
      end;
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Res := nil;
  Result := False;
end;

// (length list)
function list_length(list: TDynDatum): Integer;
begin
  Result := 0;
  while IsPair(list) do
  begin
    Inc(Result);
    list := cdr(list)
  end;
end;

function list_Vector(list: TDynDatum): IDynArray;
var
  i, n: Integer;
  A: TDynDatum;
begin
  n := list_length(list);
  Result := make_vector(n);
  for i := 0 to n - 1 do
  begin
    A := car(list);
    Result[i] := A;
    list := cdr(list)
  end;
end;

function list_VectorR(list: TDynDatum): IDynArray;
var
  i, n: Integer;
  A: TDynDatum;
begin
  n := list_length(list);
  Result := make_vector(n);
  for i := n - 1 downto 0 do
  begin
    A := car(list);
    Result[i] := A;
    list := cdr(list)
  end;
end;

function VectorToList(Vec: IDynArray; iMin, iMax: Integer): IDynPair;
var
  i: Integer;
begin
  Result := nil;
  for i := iMax downto iMin do
    Result := cons(Vec[i], Result);
end;

procedure Prepend(var Dest: IDynPair; const List: IDynPair);
begin
  if Assigned(List) then
  begin
    Prepend(Dest, IDynPair(Pointer(cdr(List))));
    Dest := cons(car(List), Dest);
  end;
end;

// (append list ... obj)
function list_append(Lists: array of IDynPair; Obj: IDynDatum): IDynPair;
var
  i: Integer;
begin
  Result := IDynPair(Pointer(Obj));
  for i := High(Lists) downto Low(Lists) do
    Prepend(Result, Lists[i]);
end;

{ TCompareHelper }

function TCompareHelper.EqualInteger(B: TDynDatum): Boolean;
begin
  Result := (B.Kind = atInteger) and (Int64Value(TDynDatum(Self)) = Int64Value(B));
end;

function TCompareHelper.EqualReal(B: TDynDatum): Boolean;
begin
  Result := (B.Kind = atReal) and (FloNumValue(TDynDatum(Self)) = FloNumValue(B));
end;

function TCompareHelper.EqualString(B: TDynDatum): Boolean;
begin
  Result := (B.Kind = atString) and (IDynString(Pointer(Self)).AsString = IDynString(Pointer(B)).AsString);
end;

function TCompareHelper.EqualList(B: TDynDatum): Boolean;
begin
  Result := (B.Kind = atPair) and list_equal(IDynPair(Pointer(Self)), IDynPair(Pointer(B)));
end;

function TCompareHelper.EqualVector(B: TDynDatum): Boolean;
begin
  Result := (B.Kind = atVector) and vector_equal(IDynArray(Pointer(Self)), IDynArray(Pointer(B)));
end;

function TCompareHelper.EqualPtr(B: TDynDatum): Boolean;
begin
  Result := (Pointer(Self) = B)
end;

class function TCompareHelper.EqualFunc(A: TDynDatum): TDynDatumPredicate;
var
  KindA: TDatumType;
  H: TCompareHelper;
begin
  H := TCompareHelper(Pointer(A));
  KindA := A.Kind;
  case KindA of
    atInteger:
      Result := H.EqualInteger;
    atReal:
      Result := H.EqualReal;
    atString:
      Result := H.EqualString;
    atPair:
      Result := H.EqualList;
    atVector:
      Result := H.EqualVector;
    else
      Result := H.EqualPtr;
  end;
end;

end. ///////////////////////////////////////////////////////////////////////////

