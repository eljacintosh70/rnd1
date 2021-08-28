function Deb(X: TDynDatum; Max: Integer = 256): String;
var
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
end;

function Deb(const X: IDynDatum; Max: Integer = 256): String; overload;
begin
  Result := Deb(TDynDatum(Pointer(X)), Max);
end;

function ManageRefs(const Refs: array of PDatumRef): TAutoDestroyRecord;
begin
end;

exports
  ManageRefs    name 'ManageRefs',

  Deb(X: TDynDatum; Max: Integer = 256)      name 'DebDatum';

