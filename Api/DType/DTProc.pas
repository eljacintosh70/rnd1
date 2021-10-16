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
  public                                                                     public
    function DatumType: TDatumType; override;
    procedure Call(out Result: TDatumRef; Params: TDynDatum); virtual; abstract;
  private
    function GetAsIDynFunc: IDynFunc;
  public
    property AsIDynFunc: IDynFunc read GetAsIDynFunc {$if Declared(InlineVMT)} implements IDynFunc {$ifend};
  public
    Name: String;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
    procedure DoMsgEvalCall(var Msg: TEvalCallMessage); message MsgEvalCall;
  end;

  TDynFuncNat = class(TDynFunc)
  public
    Fn: TLispProc;
    procedure Call(out Result: TDatumRef; Params: TDynDatum); override;
    constructor Create(const Def: TLispProcRec);
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
    Name: IDynSymbol;
    function DatumType: TDatumType; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
    constructor Create(AName: IDynSymbol; AMethod: TDynFuncO);
  end;

  TNamedDynFuncG = class(TDynFunc)
  public
    Method: TDynFuncG;
    Name: IDynSymbol;
    function DatumType: TDatumType; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
    constructor Create(AName: IDynSymbol; AMethod: TDynFuncG);
  end;

  TDynLambda = class(TDynFunc)
  public
    Arg: TDatumRef;
    Body: TDatumRef;
    Scope: IDynScope;
    function DatumType: TDatumType; override;
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
    Name: String;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
    procedure DoMsgEvalCall(var Msg: TEvalCallMessage); message MsgEvalCall;
  end;

  TDynSyntax = class(TCustomDynSyntax)
  public
    Method: TSintaxMethod;
    function DatumType: TDatumType; override;
    procedure Eval(out Result: TDatumRef; Params: TDynDatum; Scope: IDynScope);
        override;
    constructor Create(const AName: String; AMethod: TSintaxMethod);
  end;

  TDynSyntaxNat = class(TCustomDynSyntax)
  public
    Fn: TLispSyntax;
    function DatumType: TDatumType; override;
    procedure Eval(out Result: TDatumRef; Params: TDynDatum; Scope: IDynScope);
        override;
    constructor Create(const Def: TLispSyntaxRec);
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

function TDynFunc.DatumType: TDatumType;
begin
  Result := atExtFunc
end;

procedure TDynFunc.DoMsgDisplay(var Msg: TWriteMsg);
var
  s: string;
begin
  s := Format('(f:%s: %s %p)', [Name, ClassName(), Pointer(Self)]);
  Msg.Res := Ord(WriteToPort(Msg.Port, s));
end;

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

procedure TNamedDynFunc.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  Method(Result, Par);
end;

constructor TNamedDynFunc.Create(AName: IDynSymbol; AMethod: TDynFuncO);
begin
  Name := AName;
  Method := AMethod;
end;

{ TNamedDynFuncG }

function TNamedDynFuncG.DatumType: TDatumType;
begin
  Result := atExtFunc
end;

procedure TNamedDynFuncG.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  Method(Result, Par);
end;

constructor TNamedDynFuncG.Create(AName: IDynSymbol; AMethod: TDynFuncG);
begin
  Name := AName;
  Method := AMethod;
end;

{ TDynLambda }

procedure TDynLambda.Call(out Result: TDatumRef; Par: TDynDatum);
var
  Arg: TDynDatum;
  Scope: IDynScope;
  Obj: TBigScope;
begin
  Arg := Self.Arg.Value;
  Obj := TBigScope.Create(Self.Scope);
  Scope := Obj.AsIDynScope;
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

{ TCustomDynSyntax }

procedure TCustomDynSyntax.DoMsgDisplay(var Msg: TWriteMsg);
var
  s: string;
begin
  s := Format('(s:%s: %s %p)', [Name, ClassName(), Pointer(Self)]);
  Msg.Res := Ord(WriteToPort(Msg.Port, s));
end;

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

{ TDynSyntax }

function TDynSyntax.DatumType: TDatumType;
begin
  Result := atSyntax
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

{ TDynFuncNat }

procedure TDynFuncNat.Call(out Result: TDatumRef; Params: TDynDatum);
begin
  Fn(Result, Params);
end;

constructor TDynFuncNat.Create(const Def: TLispProcRec);
begin
  Name := Def.Name;
  Fn   := Def.Fn;
end;

{ TDynSyntaxNat }

constructor TDynSyntaxNat.Create(const Def: TLispSyntaxRec);
begin
  Name := Def.Name;
  Fn   := Def.Fn;
end;

function TDynSyntaxNat.DatumType: TDatumType;
begin
  Result := atSyntax
end;

procedure TDynSyntaxNat.Eval(out Result: TDatumRef; Params: TDynDatum;
  Scope: IDynScope);
begin
  Fn(Result, Params, Scope)
end;

end. ///////////////////////////////////////////////////////////////////////////

