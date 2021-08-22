
function CreateFloNumNR(Val: Extended): TDynDatum;
var
  Helper: TDynFloat;
begin
  Helper := TDynFloat.Create(Val);
  Result := Pointer(Helper);
end;

exports
  CreateFloNumNR  name 'float->';
