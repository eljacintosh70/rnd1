unit LispSintax;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTParser, DTProc;

type
  {$TYPEINFO ON}
  TSintax1 = class(TObject)
  private
    function AdToEnd(a, d: TDynDatum): IDynPair;
    function CopyUnquoting(p: TDynDatum; const Scope: IDynScope): IDynPair;
  published
    // (define Name Rest)
    procedure define(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (. obj member params...)
    procedure dot(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (.set! obj member value)
    procedure DotSet(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (set! Name Value)
    procedure SetVal(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (if Cond IfVal ElseVal)
    procedure _if(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (quote Value)
    procedure quote(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (quasiquote Value)
    procedure quasiquote(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (lambda params . value)
    procedure lambda(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (let ((x v) ...) e1 e2 ...)
    // (let f ((x v) ...) e1 e2 ...)
    procedure let(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (let* ((x v) ...) e1 e2 ...)
    procedure let_2A(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (letrec ((x v) ...) e1 e2 ...)
    procedure letrec(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    {
    // (import (libname) procname...)
    procedure import(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (import-on-call (libname) procname...)
    procedure _import_on_call(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    }
  end;
  // quasiquote unquote unquote-splicing


procedure SeparCarCdrList(List: TDynDatum; out CarL, CdrL: IDynPair);

implementation /////////////////////////////////////////////////////////////////

uses
  SysUtils,
  // SchProc, LazyLib,
  LispEval, LispParser;

const
  Undefined = TDynDatum(smInline or $40);

var
  DebStr: String;

procedure SeparCarCdrList(List: TDynDatum; out CarL, CdrL: IDynPair);
var
  Seq: IDynPair;
  Item: TDynDatum;
  CarL1, CdrL1: IDynPair;
begin
  Seq := IDynPair(Pointer(List));
  while GetNext(Seq, Item) do
  begin
    CarL1 := DynTypes.cons(car(Item), CarL1);
    CdrL1 := DynTypes.cons(cdr(Item), CdrL1);
  end;
  CarL := Reverse(CarL1);
  CdrL := Reverse(CdrL1);
end;

procedure SeparCarCadrList(List: TDynDatum; out CarL, CadrL: IDynPair);
var
  Seq: IDynPair;
  Item: TDynDatum;
  CarL1, CadrL1: IDynPair;
begin
  Seq := IDynPair(Pointer(List));
  while GetNext(Seq, Item) do
  begin
    CarL1 := DynTypes.cons(car(Item), CarL1);
    CadrL1 := DynTypes.cons(car(cdr(Item)), CadrL1);
  end;
  CarL := Reverse(CarL1);
  CadrL := Reverse(CadrL1);
end;


{ TSintax1 }

procedure TSintax1._if(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
begin                   // (Cond IfVal ElseVal)
  Result := Eval(car(Datum), Scope);
  Datum := cdr(Datum);    // (IfVal ElseVal)
  if Result.Value = _f then
    Datum := cdr(Datum);  // (ElseVal)
  if IsPair(Datum) then
    Result := Eval(car(Datum), Scope)
  else
    Result := (Undefined);
end;

{
procedure TSintax1.import(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  LibName, ProcNames: TDynDatum;
  Lib: IInterface;
begin
  // (import (libname) procname...)
  NeedParams(Datum, [@LibName], @ProcNames);
  Lib := TSchLib.Create(LibName, ProcNames, Scope);
  Result := (Undefined);
end;

procedure TSintax1._import_on_call(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  LibName, ProcNames: TDynDatum;
  Lib: IInterface;
begin
  // (import-on-call (libname) procname...)
  NeedParams(Datum, [@LibName], @ProcNames);
  Lib := TLazyLib.Create(LibName, ProcNames, Scope);
  Result := (Undefined);
end;
}

procedure TSintax1.define(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  VarDatum, Rest: TDynDatum;
  Formal: TDynDatum;
  Name: string;
begin
  // http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.2.1
  // (define <variable> <expression>)
  // (define <variable>)
  // (define (<variable> <formals>) <body>)
  // (define (<variable> . <formal>) <body>)

  NeedParams(Datum, [@VarDatum], @Rest);
  if IsSymbol(VarDatum) then
  begin
    NeedSymbol(VarDatum);
    if Rest <> _null then
      if IsPair(Rest) and (Cdr(Rest) = _Null) then
        Result := Eval(Car(Rest), Scope)    // (define <variable> <expression>)
      else
      begin
        Name := IDynSymbol(Pointer(VarDatum)).Name;
        raise Exception.Create(Format('invalid define %s . %s', [Name, Deb(Rest)]))
      end
    else
      Result := (Unbound);             // (define <variable>)
  end
  else if IsPair(VarDatum) then
  begin
    Formal := cdr(VarDatum);
    VarDatum := car(VarDatum);
    NeedSymbol(VarDatum);  // (define (<variable> . <formal>) <body>)
    Result := (TDynLambda.Create(Formal, Rest, Scope)).AsIDynFunc;
  end
  else
    raise Exception.Create(Format('invalid define %s . %s', [Deb(Datum), Deb(Rest)]));
  Scope.Value[VarDatum] := Result.Value;
end;

procedure TSintax1.dot(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
{
https://clojure.org/reference/java_interop

(.instanceMember instance args*) ==> (. instance instanceMember args*)
(.instanceMember Classname args*) ==>
    (. (identity Classname) instanceMember args*)
(.-instanceField instance) ==> (. instance -instanceField)
(Classname/staticMethod args*) ==> (. Classname staticMethod args*)
Classname/staticField ==> (. Classname staticField)
}
var
  Instance, MemberId, Params, Member: TDynDatum;
  InstanceR, ParamsR: TDatumRef;
  SubScope: IDynScope;
  Fn: IDynFunc;
  Method: IDynMethod;
begin
  // (. instance member params)
  NeedParams(Datum, [@Instance, @MemberId], @Params);
  DebStr := Deb(Datum);

  InstanceR := Eval(Instance, Scope);
  if Assigned(Params) then
  begin
    EvalParams(ParamsR, Params, Scope);
    Params := ParamsR.Value;
  end;
  case InstanceR.Value.Kind of
    atScope:
      begin
        CallMember(Result, IDynScope(Pointer(InstanceR.Value)), MemberId, Params);
        Exit;
        //SubScope := IDynScope(Pointer(InstanceR.Value));
        //Member := SubScope.Value[MemberId];
      end;
    else
      begin
        Member := nil;
        DynError('record required: %s', [Deb(InstanceR.Value, 200)]);
      end;
  end;
  case Member.Kind of
    atExtFunc, atAutoFunc, atLambda:
      begin
        Fn := IDynFunc(Pointer(Member));
        Fn.Call(Result, Params);
      end;
    atMethod:
      begin
        Method := IDynMethod(Pointer(Member));
        Method.Call(Result, SubScope, Params);
      end;
    else
      Result := (Member);
  end;
end;

procedure TSintax1.DotSet(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  Obj, Member, Value: TDynDatum;
  ObjR,       ValueR: TDatumRef;
  SubScope: IDynScope;
begin
  // (.set! obj member value)
  NeedParams(Datum, [@Obj, @Member, @Value]);
  DebStr := Deb(Datum);

  ObjR := Eval(Obj, Scope);
  ValueR := Eval(Value, Scope);

  case ObjR.Value.Kind of
    atScope:
      begin
        SubScope := IDynScope(Pointer(ObjR.Value));
        SubScope.Value[Member] := ValueR.Value;
      end;
    else
      begin
        DynError('record required: %s', [Deb(ObjR.Value, 200)]);
      end;
  end;
  Result := (Undefined);
end;

procedure TSintax1.quote(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  Value: TDynDatum;
begin
  // (quote Value)
  NeedParams(Datum, [@Value]);
  Result := (Value);
end;

procedure TSintax1.quasiquote(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  Value: TDynDatum;
begin
  // (quasiquote Value)
  NeedParams(Datum, [@Value]);
  Result := (Value);
  if IsPair(Value) then
  begin
    InitQuoteFn;
    Result := (CopyUnquoting(Value, Scope));
  end;
end;

procedure TSintax1.lambda(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  Params: TDynDatum;
  FnDef: TDynDatum;
begin
  // (lambda Params . FnDef)
  NeedParams(Datum, [@Params], @FnDef);
  NeedPair(Params);
  Result := (TDynLambda.Create(Params, FnDef, Scope)).AsIDynFunc;
end;

var
  tmp: String;

procedure TSintax1.let(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  Bindings, Body: TDynDatum;
  x, v: IDynPair;
  FnRef: IDynFunc;
  Par: TDatumRef;
begin
  // http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.4.6
  // (let <bindings> <body>)
  // ( [lambda (<bindings-vars>) <body>] . <bindings-values>)

  // http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.16
  // (let <variable> <bindings> <body>)

  {
    (let
     ((x v) ...)
     e1 e2 ...)
  =>
    ( (lambda
        (x ...)
        e1 e2 ...)
      v ...)
  }
  NeedParams(Datum, [@Bindings], @Body);
  if IsPair(Bindings) then
  begin
    tmp := Deb(Bindings);
    SeparCarCadrList(Bindings, x, v);
    tmp := Deb(x);
    tmp := Deb(v);
    FnRef := TDynLambda.Create(Pointer(x), Body, Scope).AsIDynFunc;
    EvalParams(Par, Pointer(v), Scope);
    FnRef.Call(Result, Par.Value);
  end

  {
    (let f
     ((x v) ...)
     e1 e2 ...)
  =>
    ( (letrec
        ((f (lambda
          (x ...)
          e1 e2 ...)

        f)
      v ...)
  }

  else
    raise Exception.Create(Format('invalid syntax: (let . %s)', [Deb(Datum)]));
end;

procedure TSintax1.letrec(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
begin
  {
    (letrec
      ((x v) ...)
      e1 e2 ...)
   =>
    (let
      ((x #f) ...)
      (set! x v) ...
      e1 e2 ...)))
  }
    raise Exception.Create(Format('invalid syntax: (letrec . %s)', [Deb(Datum)]));
end;

procedure TSintax1.let_2A(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
begin
  {
    (let*
      ((x1 v1) (x2 v2) ...)
      e1 e2 ...)
  =>
    (let
      ((x1 v1) )
      (let* ((x2 v2) ...)
        e1 e2 ...))
  }
    raise Exception.Create(Format('invalid syntax: (let* . %s)', [Deb(Datum)]));
end;

procedure TSintax1.SetVal(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  NameDatum: TDynDatum;
begin
  Result := (Datum);
  NameDatum := car(Datum);
  NeedSymbol(NameDatum);
  Datum := cdr(Datum);
  Result := (car(Datum));
  Result := Eval(Result.Value, Scope);
  Scope.Value[NameDatum] := Result.Value;
end;

function TSintax1.CopyUnquoting(p: TDynDatum; const Scope: IDynScope): IDynPair;
var
  a, d, b: TDynDatum;
  f: TDynDatum;
  dRef: IDynPair;
  Res: TDatumRef;
begin
  a := car(p);                 // p = (a . d)
  d := cdr(p);
  if IsPair(d) then
  begin
    dRef := CopyUnquoting(d, Scope);
    d := Pointer(dRef);
  end;
  if IsPair(a) then
  begin
    f := car(a);
    if IsSymbol(f) then
      if f = QuoteFn[tkUnQuote].Value then  // a = (unquote b) => p = ((unquote b) . d)
      begin
        b := car(cdr(a));
        Res := Eval(b, Scope);
        Result := cons(Res.Value, d);      // -> ((eval b) . d)
        Exit;
      end
      else if f = QuoteFn[tkUnQuoteSplicing].Value then
      begin
        b := car(cdr(a));
        Res := Eval(b, Scope);
        Result := AdToEnd(Res.Value, d);
        p.Free;
        Exit;
      end
  end;
  Result := cons(a, d);
end;

function TSintax1.AdToEnd(a, d: TDynDatum): IDynPair;
begin
  if IsPair(a) then
    Result := cons(car(a), AdToEnd(cdr(a), d))
  else
    Result := IDynPair(Pointer(d))
end;

end. ///////////////////////////////////////////////////////////////////////////

