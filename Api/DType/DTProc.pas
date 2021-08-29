unit DTProc;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DUtils, DTDatum;

type
  ISchForm = IDynSyntax;
  TSintaxMethod = TDynSyntaxO;
  TProcedureMethod = TDynFuncO;
  //TSintaxMethod = procedure (out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope) of object;
  //TProcedureMethod = procedure (out Result: TDatumRef; Datum: TDynDatum) of object;

type
  TDynFunc = class(TDyn, IDynFunc)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynFunc
  public
    procedure Call(out Result: TDatumRef; Params: TDynDatum); virtual; abstract;
  private
    function GetAsIDynFunc: IDynFunc;
  public
    property AsIDynFunc: IDynFunc read GetAsIDynFunc {$if Declared(InlineVMT)} implements IDynFunc {$ifend};
  public
    function DisplayStr(NeededChars: Integer): String; override;
    procedure DoMsgEvalCall(var Msg: TEvalCallMessage); message MsgEvalCall;
  end;

  TDynMethod = class(TDyn, IDynMethod)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynMethod
  public
    procedure Call(out Result: TDatumRef; const this: IDynScope; Params: TDynDatum); virtual; abstract;
  private
    function GetAsIDynMethod: IDynMethod;
  public
    property AsIDynMethod: IDynMethod read GetAsIDynMethod {$if Declared(InlineVMT)} implements IDynMethod {$ifend};
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
  end;

  TDynMethodObjG = class(TDynMethod)
  public
    Method: TDynMethodG;
    constructor Create(AMethod: TDynMethodG);
    procedure Call(out Result: TDatumRef; const this: IDynScope; Params: TDynDatum); override;
  end;

  TDynMethodObjO = class(TDynMethod)
  public
    Method: TDynMethodO;
    procedure Call(out Result: TDatumRef; const this: IDynScope; Params: TDynDatum); override;
    constructor Create(AMethod: TDynMethodO);
  end;

  TNamedDynFunc = class(TDynFunc)
  public
    Method: TDynFuncO;
    Name: TDynDatum;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
    constructor Create(AName: TDynDatum; AMethod: TDynFuncO);
  end;

  TNamedDynFuncG = class(TDynFunc)
  public
    Method: TDynFuncG;
    Name: TDynDatum;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
    constructor Create(AName: TDynDatum; AMethod: TDynFuncG);
  end;

  TDynLambda = class(TDynFunc)
  public
    Arg: TDatumRef;
    Body: TDatumRef;
    Scope: IDynScope;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
  public
    constructor Create(AArg, ABody: TDynDatum; AScope: IDynScope);
  end;

  TCustomDynSyntax = class(TDyn, IDynSyntax)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynSyntax
  public
    procedure Eval(out Result: TDatumRef; Params: TDynDatum; Scope: IDynScope); virtual; abstract;
  private
    function GetAsIDynSyntax: IDynSyntax;
  public
    property AsIDynSyntax: IDynSyntax read GetAsIDynSyntax {$if Declared(InlineVMT)} implements IDynSyntax {$ifend};
  public
    function DisplayStr(NeededChars: Integer): String; override;
    procedure DoMsgEvalCall(var Msg: TEvalCallMessage); message MsgEvalCall;
  end;

  TDynSyntax = class(TCustomDynSyntax)
  public
    Method: TSintaxMethod;
    Name: String;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    procedure Eval(out Result: TDatumRef; Params: TDynDatum; Scope: IDynScope);
        override;
    constructor Create(const AName: String; AMethod: TSintaxMethod);
  end;

function AsSintax(Self: TDynDatum): TDynSyntax; overload;  {$ifdef INLINE} inline; {$endif}
function SyntaxDatum(const AName: String; AMethod: TSintaxMethod): TDynDatum; {$ifdef INLINE} inline; {$endif}

implementation /////////////////////////////////////////////////////////////////

uses
  SysUtils, DTScope;

function AsSintax(Self: TDynDatum): TDynSyntax;
begin
  Result := Pointer(Integer(Self) and PointerMask);
end;

function SyntaxDatum(const AName: String; AMethod: TSintaxMethod): TDynDatum;
begin
  Result := Pointer(TDynSyntax.Create(AName, AMethod));
end;

procedure EvalSeq(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
begin
  while IsPair(Datum) do
  begin
    Result := Eval(car(Datum), Scope);
    Datum := cdr(Datum);
  end;
end;

{ TDynFunc }

procedure TDynFunc.DoMsgEvalCall(var Msg: TEvalCallMessage);
var
  Msg2: TEvalMessage;
begin
  Msg2.Msg := MsgEvalItems;
  Msg2.Scope := Msg.Scope;
  Msg.Params.DispatchMsg(Msg2);
  Call(Msg.Res, Msg2.Res);
end;

function TDynFunc.GetAsIDynFunc: IDynFunc;
begin
  {$if Declared(InlineVMT)} Result := IDynFunc(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynFunc.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format(':Proc:%p', [Pointer(Self)]);
end;

{ TDynMethod }

function TDynMethod.GetAsIDynMethod: IDynMethod;
begin
  {$if Declared(InlineVMT)} Result := IDynMethod(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynMethod.DatumType: TDatumType;
begin
  Result := atMethod;
end;

function TDynMethod.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('method:%p', [Pointer(Self)]);
end;

{ TDynMethodObjG }

procedure TDynMethodObjG.Call(out Result: TDatumRef;
  const this: IDynScope; Params: TDynDatum);
begin
  Method(Result, this, Params);
end;

constructor TDynMethodObjG.Create(AMethod: TDynMethodG);
begin
  Method := AMethod;
end;

{ TDynMethodObjO }

procedure TDynMethodObjO.Call(out Result: TDatumRef;
  const this: IDynScope; Params: TDynDatum);
begin
  Method(Result, this, Params);
end;

constructor TDynMethodObjO.Create(AMethod: TDynMethodO);
begin
  Method := AMethod;
end;

{ TNamedDynFunc }

function TNamedDynFunc.DatumType: TDatumType;
begin
  Result := atExtFunc
end;

function TNamedDynFunc.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('?:%p:%s', [Pointer(Self), Deb(Name)]);
end;

procedure TNamedDynFunc.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  Method(Result, Par);
end;

constructor TNamedDynFunc.Create(AName: TDynDatum; AMethod: TDynFuncO);
begin
  Name := AName;
  Method := AMethod;
end;

{ TNamedDynFuncG }

function TNamedDynFuncG.DatumType: TDatumType;
begin
  Result := atExtFunc
end;

function TNamedDynFuncG.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('?:%p:%s', [Pointer(Self), Deb(Name)]);
end;

procedure TNamedDynFuncG.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  Method(Result, Par);
end;

constructor TNamedDynFuncG.Create(AName: TDynDatum; AMethod: TDynFuncG);
begin
  Name := AName;
  Method := AMethod;
end;

{ TDynLambda }

procedure TDynLambda.Call(out Result: TDatumRef; Par: TDynDatum);
var
  Arg: TDynDatum;
  Scope: IDynScope;
begin
  Arg := Self.Arg.Value;
  Scope := TBigScope.Create(Self.Scope);
  while IsPair(Arg) do
  begin
    Scope.Value[car(Arg)] := car(Par);
    Arg := cdr(Arg);
    Par := cdr(Par);
  end;
  //Deb(Scope);
  if IsSymbol(Arg) then
  begin
    Self.Scope.Value[Arg] := Par;
  end;
  EvalSeq(Result, Self.Body.Value, Scope);
end;

constructor TDynLambda.Create(AArg, ABody: TDynDatum; AScope: IDynScope);
begin
  inherited Create;
  Arg := (AArg);
  Body := (ABody);
  Scope := AScope;
end;

function TDynLambda.DatumType: TDatumType;
begin
  Result := atLambda
end;

function TDynLambda.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('?:%p', [Pointer(Self)]);
end;

{ TCustomDynSyntax }

procedure TCustomDynSyntax.DoMsgEvalCall(var Msg: TEvalCallMessage);
begin
  Eval(Msg.Res, msg.Params, Msg.Scope)
end;

function TCustomDynSyntax.GetAsIDynSyntax: IDynSyntax;
begin
  {$if Declared(InlineVMT)} Result := IDynSyntax(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TCustomDynSyntax.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format(':Syntax:%p', [Pointer(Self)]);
end;

{ TDynSyntax }

function TDynSyntax.DatumType: TDatumType;
begin
  Result := atSyntax
end;

function TDynSyntax.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('?:%p', [Pointer(Self)]);
end;

procedure TDynSyntax.Eval(out Result: TDatumRef;
  Params: TDynDatum; Scope: IDynScope);
begin
  Method(Result, Params, Scope);
end;

constructor TDynSyntax.Create(const AName: String; AMethod: TSintaxMethod);
begin
  inherited Create;
  //p.RefCount := 0;
  //p.DatumType := atSyntax;
  Pointer(Name) := nil;
  Name := AName;
  Method := AMethod;
end;

end. ///////////////////////////////////////////////////////////////////////////

