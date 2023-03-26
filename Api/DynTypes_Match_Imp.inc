
function AsignToDyn(var Dest; const src: dyn): Boolean;
begin
  dyn(Dest) := src;
  Result := True;
end;

function AsignToIDyn(var Dest; const src: dyn): Boolean;
begin
  IDyn(Dest) := src.Ref;
  Result := True;
end;

function AsignToBoolean(var Dest; const src: dyn): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToBool, @Dest)
end;

function AsignToInt64(var Dest; const src: dyn): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToInt64, @Dest)
end;

function AsignToDouble(var Dest; const src: dyn): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToDouble, @Dest)
end;

function AsignToWideChar(var Dest; const src: dyn): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToChar, @Dest)
end;

function AsignToAnsiChar(var Dest; const src: dyn): Boolean;
var
  ch: WideChar;
begin
  Result := HandleMessageWithPointer(src, MsgCastToChar, @ch);
  if Result then
    case ch of
      #$0000..#$00FF: AnsiChar(Dest) := AnsiChar(ch);
    else
      Result := False;
    end;
end;

function AsignToString(var Dest; const src: dyn): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToString, @Dest)
end;

function AsignIgnore(var Dest; const src: dyn): Boolean;
begin
  Result := True;
end;

function AsignIsNil(var Dest; const src: dyn): Boolean;
begin
  Result := (src = nil);
end;

function MatchCount(const src: dyn; const Required: array of dynout): Integer;
begin
  Result := MatchCount(src, Required, _nil)
end;

function MatchCount(const src: dyn; const Required: array of dynout;
  Rest: dynout): Integer;
var
  i, n: Integer;
  L, V: dyn;
  P: IDynPair;
begin
  n := Length(Required);
  L := src;
  for i := 0 to n - 1 do
  begin
    NeedPair(L, P);
    V := P.car;
    if not Required[i].CanAssign(V) then
    begin
      Result := i;
      Exit;
    end;
    L := P.cdr;
  end;
  Result := n;
  if Rest.CanAssign(L) then
    Inc(Result);
end;

{ dynout }

procedure dynout.Assign(const src: dyn);
begin
  if not AsigFn(DestVar^, src) then
    raise Exception.Create('invalid type');
end;

function dynout.CanAssign(const src: dyn): Boolean;
begin
  Result := AsigFn(DestVar^, src)
end;

class operator dynout.Implicit(var A: dyn): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToDyn
end;

class operator dynout.Implicit(var A: IDyn): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToIDyn
end;

class operator dynout.Implicit(var A: Boolean): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToBoolean
end;

class operator dynout.Implicit(var A: Int64): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToInt64
end;

class operator dynout.Implicit(var A: Double): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToDouble
end;

class operator dynout.Implicit(var A: WideChar): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToWideChar
end;

class operator dynout.Implicit(var A: AnsiChar): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToAnsiChar
end;

class operator dynout.Implicit(var A: UnicodeString): dynout;
begin
  Result.DestVar := @A;
  Result.AsigFn := AsignToString
end;



