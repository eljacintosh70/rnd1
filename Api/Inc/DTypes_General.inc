function Deb(Obj: TDynDatum; Max: Integer = 256): String;
begin
  Result := Deb(IDynDatum(Pointer(Obj)), max)
end;

function Deb(const Obj: IDynDatum; Max: Integer = 256): String; overload;
var
  Msg: TWriteMsg;
  TextOut: TStrTextOutW;
  Port: TDynOutPortLisp;
  Res: Boolean;
begin
  TextOut := TStrTextOutW.Create(200);
  Port := TDynOutPortLisp.Create(TextOut.WriteProc);

  if Obj <> nil then
  begin
    Msg.Msg := MsgDebugWrite;
    Msg.Res := 0;
    Msg.Port := Port;
    Obj.DispatchMsg(Msg);
    Res := Msg.Res <> 0;
  end
  else
    Res := Port.WriteSpecial(svNil);
  Result := TextOut.GetText;
end;

{$IFNDEF LINUX}
exports
  Deb(X: TDynDatum; Max: Integer = 256)      name 'DebDatum';
{$ENDIF}

