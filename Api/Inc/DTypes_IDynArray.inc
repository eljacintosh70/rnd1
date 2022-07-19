function make_vector(n: Integer; Fill: TDynDatum = nil): IDynArray;
var
  Helper: TDynArray;
begin
  Helper := TDynArray.Create(n, Fill);
  Result := Helper.AsIDynArray;
end;

function make_vector(const Arr: array of const): IDynArray;
var
  Helper: TDynArray;
begin
  Helper := TDynArray.Create(Arr);
  Result := Helper.AsIDynArray;
end;

function ListToDynArray(List: dyn): IDynArray;
var
  i, n: Integer;
  v: dyn;
begin
  n := list_length(List);
  Result := make_vector(n);
  for i := 0 to n - 1 do
  begin
    v := car(List);
    Result[i] := v;
    List := cdr(List);
  end;
end;

exports
  {$IFNDEF FPC}
  make_vector(n: Integer; Fill: TDynDatum)   name 'make_vectorN',
  make_vector(const Arr: array of const)     name 'make_vector',
  {$ENDIF}
  ListToDynArray                             name 'list->array';
