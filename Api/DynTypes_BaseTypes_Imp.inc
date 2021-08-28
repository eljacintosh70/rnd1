
{ dyn }

function dyn.Invoke(const Arg: array of dyn): dyn;
begin
  Result := Ref.Invoke(Arg)
end;

function dyn.GetEnumerator: IDynEnumerator;
begin
  Result := Ref.GetEnumerator
end;

function dyn.GetItem(const Key: dyn): dyn;
begin
  Result := Ref.GetItem(Key)
end;

procedure dyn.SetItem(const Key, Value: dyn);
begin
  Ref.SetItem(Key, Value)
end;

function dyn.Method(Id: dyn; Arg: array of dyn): dyn;
begin
  Result := Ref.Method(Id, Arg)
end;

function dyn.Ptr: Pointer;
begin
  Result := Pointer(Ref)
end;

procedure dyn.DispatchMsg(var Message);
begin
  Ref.DispatchMsg(Message);
end;

class operator dyn.Implicit(const A: IDyn): dyn;
begin
  Result.Ref := A;
end;

class operator dyn.Implicit(const A: dyn): IDyn;
begin
  Result := A.Ref;
end;

class operator dyn.Implicit(const A: weak_IDyn): dyn;
begin
  Result.Ref := EnsureInterface(Pointer(A));
end;

class operator dyn.Implicit(const A: dyn): weak_IDyn;
begin
  Result := Pointer(A.Ref);
end;

class operator dyn.Implicit(const A: TDynDatum): dyn;
begin
  Result.Ref := EnsureInterface(Pointer(A));
end;

class operator dyn.Implicit(const A: dyn): TDynDatum;
begin
  Result := Pointer(A.Ref);
end;

class operator dyn.Implicit(const A: Boolean): dyn;
begin
  Result.Ref := MakeBool(A)
end;

class operator dyn.Implicit(const A: dyn): Boolean;
begin
  HandleMessageWithPointer_Err(A, MsgCastToBool,  @Result, @ErrCastToInt)
end;

class operator dyn.Implicit(const A: Char): dyn;
begin
  Result.Ref := MakeChar(A)
end;

class operator dyn.Implicit(const A: dyn): Char;
begin
  HandleMessageWithPointer_Err(A, MsgCastToChar,  @Result, @ErrCastToInt)
end;

class operator dyn.Implicit(const A: Int64): dyn;
begin
  Result.Ref := MakeInt64(A)
end;

class operator dyn.Implicit(const A: dyn): Int64;
begin
  HandleMessageWithPointer_Err(A, MsgCastToInt64,  @Result, @ErrCastToInt)
end;

class operator dyn.Implicit(const A: Double): dyn;
begin
  Result.Ref := MakeDouble(A)
end;

class operator dyn.Implicit(const A: dyn): Double;
begin
  HandleMessageWithPointer_Err(A, MsgCastToDouble, @Result, @ErrCastToDouble)
end;

class operator dyn.Implicit(const A: String): dyn;
begin
  Result.Ref := MakeString(A)
end;

class operator dyn.Implicit(const A: dyn): String;
begin
  HandleMessageWithPointer_Err(A, MsgCastToString, @Result, @ErrCastToString)
end;

class operator dyn.Equal(const A, B: dyn): Boolean;
begin
  Result := (Pointer(A.Ref) = Pointer(B.Ref))
end;

class operator dyn.Equal(const A: dyn; const B: IDyn): Boolean;
begin
  Result := (A.Ref = B)
end;

class operator dyn.Equal(const A: dyn; B: weak_IDyn): Boolean;
begin
  Result := (Pointer(A.Ref) = B)
end;

class operator dyn.Equal(const A: dyn; B: Pointer): Boolean;
begin
  Result := (Pointer(A.Ref) = B)
end;

class operator dyn.Equal(const A: IDyn; const B: dyn): Boolean;
begin
  Result := (A = B.Ref)
end;

class operator dyn.Equal(A: weak_IDyn; const B: dyn): Boolean;
begin
  Result := (A = Pointer(B.Ref))
end;

class operator dyn.Equal(A: Pointer; const B: dyn): Boolean;
begin
  Result := (A = Pointer(B.Ref))
end;

class operator dyn.NotEqual(const A, B: dyn): Boolean;
begin
  Result := (Pointer(A.Ref) <> Pointer(B.Ref))
end;

class operator dyn.NotEqual(const A: dyn; const B: IDyn): Boolean;
begin
  Result := (A.Ref <> B)
end;

class operator dyn.NotEqual(const A: dyn; B: weak_IDyn): Boolean;
begin
  Result := (Pointer(A.Ref) <> B)
end;

class operator dyn.NotEqual(const A: dyn; B: Pointer): Boolean;
begin
  Result := (Pointer(A.Ref) <> B)
end;

class operator dyn.NotEqual(const A: IDyn; const B: dyn): Boolean;
begin
  Result := (A <> B.Ref)
end;

class operator dyn.NotEqual(A: weak_IDyn; const B: dyn): Boolean;
begin
  Result := (A <> Pointer(B.Ref))
end;

class operator dyn.NotEqual(A: Pointer; const B: dyn): Boolean;
begin
  Result := (A <> Pointer(B.Ref))
end;
