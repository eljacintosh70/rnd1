
function CreateObject(AParent: IDynScope; const PropNames: array of TDynDatum;
      const PropValues: array of const): IDynScope;
begin
  Result := TScriptObject.Create(AParent, PropNames, PropValues);
end;

function CreateMethod(AMethod: TDynMethodG): IDynMethod;
begin
  Result := TDynMethodObjG.Create(AMethod);
end;

function CreateMethod(AMethod: TDynMethodO): IDynMethod;
begin
  Result := TDynMethodObjO.Create(AMethod);
end;

procedure CallMember(out Result: TDatumRef; Scope: IDynScope; MemberId: TDynDatum;
  Params: TDynDatum = nil);
var
  Member: TDynDatum;
  Fn: IDynFunc;
  Method: IDynMethod;
begin
  Member := Scope.Value[MemberId];
  case Member.Kind of
    atExtFunc, atAutoFunc, atLambda:
      begin
        Fn := IDynFunc(Pointer(Member));
        Fn.Call(Result, Params);
      end;
    atMethod:
      begin
        Method := IDynMethod(Pointer(Member));
        Method.Call(Result, Scope, Params);
      end;
    else
      Result := (Member);
  end;
end;

procedure RegisterFunctions(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncO);
var
  i, n: Integer;
  Datum: IDynFunc;
  Method: TDynFuncO;
  Name: TDynDatum;
begin
  n := Length(Names);
  if Length(Func) <> n then
    raise Exception.Create('Count of Names and Functions doesn''t match');
  for i := 0 to n - 1 do
  begin
    Name := InitSymbol(PAnsiChar(Names[i]), Length(Names[i]));
    Method := Func[i];
    Datum := TNamedDynFunc.Create(Name, Method).AsIDynFunc;
    Scope.Value[Pointer(Name)] := Pointer(Datum);
  end;
end;

procedure RegisterFunctionsG(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncG);
var
  i, n: Integer;
  Datum: IDynFunc;
  Method: TDynFuncG;
  Name: TDynDatum;
begin
  n := Length(Names);
  if Length(Func) <> n then
    raise Exception.Create('Count of Names and Functions doesn''t match');
  for i := 0 to n - 1 do
  begin
    Name := InitSymbol(PAnsiChar(Names[i]), Length(Names[i]));
    Method := Func[i];
    Datum := TNamedDynFuncG.Create(Name, Method).AsIDynFunc;
    Scope.Value[Pointer(Name)] := Pointer(Datum);
  end;
end;

exports
  CreateObject name 'CreateObject',
  CreateMethod(AMethod: TDynMethodG) name 'CreateMethodG',
  CreateMethod(AMethod: TDynMethodO) name 'CreateMethodO',
  CallMember name 'CallMember',

  RegisterFunctions name 'RegisterFunctions',
  RegisterFunctionsG name 'RegisterFunctionsG';
