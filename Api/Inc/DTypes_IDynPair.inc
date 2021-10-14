
function cons(const Car, Cdr: dyn): IDynPair; stdcall;
var
  Helper: TDynPair;
begin
  Helper := TDynPair.Create(Car, Cdr);
  Result := Helper.AsISchPair;
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
  car, Rest: dyn;
  p: IDynPair;
begin
  Result := nil;
  Rest := List;
  while IsPair(Rest, p) do
  begin
    car := p.car;
    Result := DynTypes.cons(car, Result);
    Rest := p.cdr;
  end;
end;

exports
  cons          name 'cons',
  cons_         name 'cons*',
  list          name 'list',
  list_length   name 'list_length',
  make_list     name 'make_list',

  Reverse name 'Reverse';
