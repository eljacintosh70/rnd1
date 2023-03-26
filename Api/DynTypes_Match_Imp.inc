
function MatchDyn(const src: dyn; var Dest): Boolean;
begin
  dyn(Dest) := src;
  Result := True;
end;

function MatchIDyn(const src: dyn; var Dest): Boolean;
begin
  IDyn(Dest) := src.Ref;
  Result := True;
end;

function MatchBoolean(const src: dyn; var Dest): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToBool, @Dest)
end;

function MatchInt64(const src: dyn; var Dest): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToInt64, @Dest)
end;

function MatchDouble(const src: dyn; var Dest): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToDouble, @Dest)
end;

function MatchWideChar(const src: dyn; var Dest): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToChar, @Dest)
end;

function MatchAnsiChar(const src: dyn; var Dest): Boolean;
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

function MatchString(const src: dyn; var Dest): Boolean;
begin
  Result := HandleMessageWithPointer(src, MsgCastToString, @Dest)
end;

function IgnoreMatch(const src: dyn; var Dest): Boolean;
begin
  Result := True;
end;

function MatchNil(const src: dyn; var Dest): Boolean;
begin
  Result := (src = nil);
end;

function Match(const src: dyn; const Required: array of dynout): Boolean; overload;
var
  n: Integer;
begin
  n := MatchCount(src, Required, _nil);
  Result := (n = Length(Required) + 1)
end;

function Match(const src: dyn; const Required: array of dynout; Rest: dynout): Boolean; overload;
var
  n: Integer;
begin
  n := MatchCount(src, Required, Rest);
  Result := (n = Length(Required) + 1)
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
    if not Required[i].Match(V) then
    begin
      Result := i;
      Exit;
    end;
    L := P.cdr;
  end;
  Result := n;
  if Rest.Match(L) then
    Inc(Result);
end;

{ dynout }

procedure dynout.Assign(const src: dyn);
begin
  if not MatchFn(src, DestVar^) then
    raise Exception.Create('invalid type');
end;

function dynout.Match(const src: dyn): Boolean;
begin
  Result := MatchFn(src, DestVar^)
end;

class operator dynout.Implicit(var A: dyn): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchDyn
end;

class operator dynout.Implicit(var A: IDyn): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchIDyn
end;

class operator dynout.Implicit(var A: Boolean): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchBoolean
end;

class operator dynout.Implicit(var A: Int64): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchInt64
end;

class operator dynout.Implicit(var A: Double): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchDouble
end;

class operator dynout.Implicit(var A: WideChar): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchWideChar
end;

class operator dynout.Implicit(var A: AnsiChar): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchAnsiChar
end;

class operator dynout.Implicit(var A: UnicodeString): dynout;
begin
  Result.DestVar := @A;
  Result.MatchFn := MatchString
end;



