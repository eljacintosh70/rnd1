
function cons(const Car, Cdr: dyn): IDynPair; stdcall;
var
  Helper: TDynPair;
  Last: Pointer;
begin
  Last := Pointer(Cdr);
  if not Assigned(Last) then
    Last := _null;
  Helper := TDynPair.Create(Pointer(Car), Last);
  Result := IDynPair(Pointer(Helper))
end;

function cons_(const elem: array of dyn): IDynPair; stdcall;
var
  i, n: Integer;
begin
  n := High(elem);
  Result := IDynPair(Pointer(elem[n].Ref));
  for i := n - 1 downto 0 do
    Result := cons(elem[i], Result);
end;

function list(const elem: array of dyn): IDynPair; stdcall;
var
  i, n: Integer;
begin
  n := High(elem);
  Result := nil;
  for i := n downto 0 do
    Result := cons(elem[i], Result);
end;

function make_list(const Arr: array of const): IDynPair;
var
  i: Integer;
  Helper: TDynPair;
begin
  Helper := nil;
  for i := High(Arr) downto 0 do
    Helper := TDynPair.Create(ConstToDatum(Arr[i]), Pointer(Helper));
  Result := IDynPair(Pointer(Helper));
end;

function list_length(list: dyn): Integer; stdcall;
begin
  Result := 0;
  while IsPair(list) do
  begin
    Inc(Result);
    list := cdr(list)
  end;
end;

function Reverse(const List: IDynPair): IDynPair;
var
  car, Rest: Pointer;
begin
  Result := nil;
  Rest := Pointer(List);
  while (NativeInt(Rest) and PointerMask <> 0) do
  begin
    car := IDynPair(Rest).car;
    Result := DynTypes.cons(IDynDatum(car), IDynDatum(Result));
    Rest := IDynPair(Rest).cdr;
  end;
end;

exports
  cons          name 'cons',
  cons_         name 'cons*',
  list          name 'list',
  list_length   name 'list_length',
  make_list     name 'make_list',

  Reverse name 'Reverse';
