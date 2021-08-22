
function CreateInt32NR(Val: Integer): TDynDatum;
var
  Helper: TAbstractDynInt;
begin
  case Val of
    Low(FixNum)..High(FixNum):
      Integer(Result) := Val shl 2 or smInteger
    else
      begin
        Helper := TDynInt32.Create(Val);
        Result := Pointer(Helper);
      end;
  end
end;

function CreateInt64NR(Val: Int64): TDynDatum;
var
  Helper: TAbstractDynInt;
begin
  if (Val >= Low(Integer)) and (Val <= High(Integer)) then
    case Val of
      Low(FixNum)..High(FixNum):
        Integer(Result) := Val shl 2 or smInteger
      else
        begin
          Helper := TDynInt32.Create(Val);
          Result := Pointer(Helper);
        end;
    end
  else
  begin
    Helper := TDynInt64.Create(Val);
    Result := Pointer(Helper);
  end;
end;

exports
  CreateInt32NR   name 'int32->',
  CreateInt64NR   name 'int64->';

