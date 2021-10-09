function Deb(Obj: TDynDatum; Max: Integer = 256): String;
{var
  Syntax: TDynSyntax;
  Ext: TDynFuncRTTI;
  Info: TDynLambda;
  s: String;
begin
  try
    case X.Kind of
      atSyntax:
        begin
          Syntax := TDynSyntax(X);
          Result := Format('Syntax:%s %p(%p)', [Syntax.Name,
            TMethod(Syntax.Method).Code, TMethod(Syntax.Method).Data]);
        end;
      atExtFunc:
        begin
          Ext := TDynFuncRTTI(X);
          Result := Format('ExtFunc:%s %p(%p)', [PTypeInfo(Ext.TypInfo).Name,
            TMethod(Ext.Method).Code, TMethod(Ext.Method).Data]);
        end;
      atAutoFunc:
        begin
          Ext := TDynFuncRTTI(X);
          Result := Format('AutoFunc:%s %p(%p)', [PTypeInfo(Ext.TypInfo).Name,
            TMethod(Ext.Method).Code, TMethod(Ext.Method).Data]);
        end;
      atLambda:
        begin
          Info := TDynLambda(X);
          Result := 'lambda (';
          Dec(Max, Length(Result));

          s := Deb(Info.Arg.Value, Max);  // Arg
          Dec(Max, Length(s));
          Result := Result + s;

          s := ')'#13#10 +
               'Body:';
          Dec(Max, Length(s));
          Result := Result + s;

          if Max > 0 then
          begin                     // Body
            s := Deb(Info.Body.Value, Max);
            Dec(Max, Length(s));
            Result := Result + s;

            if Max > 0 then        // Scope
            begin
              Result := Result + #13#10'Scope:' + Deb(Info.Scope, Max);
            end;
          end;
        end;
      else
        Result := X.DisplayStr(Max);
    end;
  except
    on E: Exception do
      Result := Format('%s:%s', [E.ClassName, E.Message]);
    else
      Result := 'ERROR';
  end
}
begin
  Result := Deb(Obj as IDynDatum, max)
end;

function Deb(const Obj: IDynDatum; Max: Integer = 256): String; overload;
var
  Msg: TWriteMsg;
  TextOut: TStrTextOutW;
  Port: TDynOutPort;
  Res: Boolean;
begin
  TextOut := TStrTextOutW.Create(200);
  Port := TDynOutPort.Create(TextOut.WriteProc);

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
