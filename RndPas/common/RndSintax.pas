unit RndSintax;

interface

uses
  SysUtils,
  DynTypes;

    // (call FnName Params)
    procedure call(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (elem ArrName Indices)
    procedure elem(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (:= id valor)
    // (:= (call FnName Params) FnBody)
    // (:= (elem ArrName Indices) valor)
    procedure asign(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);

const
  TRndSintax: array[0..2] of TLispSyntaxRec = (
    //(Name: '.';       Fn: dot),
    (Name: 'call';    Fn: call),
    (Name: 'elem';    Fn: elem),
    (Name: ':=';      Fn: asign));

implementation

var
  SyElem, SyCall: dyn;

  DebStr: string;

// (call FnName Params)
procedure call(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  FnName, Params: TDynDatum;
  f: dyn;
begin
  NeedParams(Datum, [@FnName, @Params]);
  f := cons(FnName, Params);
  Result := Eval(f, Scope);
end;

// (elem ArrName Indices)
procedure elem(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  ArrName, Indices: TDynDatum;
  i: dyn;
begin
  NeedParams(Datum, [@ArrName, @Indices]);
  Result := Scope.Value[ArrName];
  for i in Indices do
    Result := Result.Item[i];
end;

// (:= id valor)
// (:= (call FnName Params) FnBody)
// (:= (elem Array Indices) valor)
procedure asign(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  Dest, Value: TDynDatum;
  FnName, Params: TDynDatum;
  ArrName, Indices: TDynDatum;
  i: TDynDatum;
  p: IDynPair;
  fn, Arr: dyn;
begin
  NeedParams(Datum, [@Dest, @Value]);

  // (:= id valor)
  if IsSymbol(Dest) then
  begin
    Scope.Value[Dest] := Value;
    Result := Value;
    Exit;
  end;

  if not IsPair(Dest, p) then
    raise Exception.Create(Format('cannot assign to: %s', [Deb(Dest)]));

  fn := p.car;
  Dest := p.cdr;

  // (:= (call FnName Params) FnBody)
  if fn = SyCall then
  begin                   //  f(x,y) := 1000*x+y
    //DebStr := Deb(Dest);  // ( f ( x y))
    NeedParams(Dest, [@FnName, @Params]);
    Result := CreateLambda(Params, list([Value]), Scope);
    Scope.Value[FnName] := Result;
    Exit;
  end;

  // (:= (elem Array Indices) valor)
  if fn = SyElem then
  begin                   // a[1,1] := 3
    //DebStr := Deb(Dest);  // ( a  ( 1 1))
    NeedParams(Dest, [@ArrName, @Indices]);

    Arr := Eval(ArrName, Scope);
    Indices := EvalItems(Indices, Scope);
    while IsPair(Indices, p) do
    begin
      i := p.car;
      Indices := p.cdr;
      if Indices <> nil then
        Arr := Arr[i]
      else
        Arr[i] := Value;
    end;

    Result := Value;
    Exit;
  end;

  raise Exception.Create(Format('cannot assign to: %s', [Deb(fn)]));
end;

initialization
  SyCall := InitSymbol('call');
  SyElem := InitSymbol('elem');
end.

