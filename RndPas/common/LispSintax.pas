unit LispSintax;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTParser, DTProc;

type
  {$TYPEINFO ON}
  TSintax1 = class(TObject)
  private
    function AdToEnd(a, d: TDynDatum): ISchPair;
    function CopyUnquoting(p: TDynDatum; const Scope: IScope): ISchPair;
  published
    // (define Name Rest)
    procedure define(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (. obj member params...)
    procedure dot(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (.set! obj member value)
    procedure DotSet(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (set! Name Value)
    procedure SetVal(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (if Cond IfVal ElseVal)
    procedure _if(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (quote Value)
    procedure quote(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (quasiquote Value)
    procedure quasiquote(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (lambda params . value)
    procedure lambda(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (let ((x v) ...) e1 e2 ...)
    // (let f ((x v) ...) e1 e2 ...)
    procedure let(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (let* ((x v) ...) e1 e2 ...)
    procedure let_2A(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (letrec ((x v) ...) e1 e2 ...)
    procedure letrec(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    {
    // (import (libname) procname...)
    procedure import(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    // (import-on-call (libname) procname...)
    procedure _import_on_call(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
    }
  end;
  // quasiquote unquote unquote-splicing


procedure SeparCarCdrList(List: TDynDatum; out CarL, CdrL: ISchPair);

implementation /////////////////////////////////////////////////////////////////

uses
  SysUtils,
  // SchProc, LazyLib,
  LispEval, LispParser;

const
  Undefined = TDynDatum(smInline or $40);

var
  DebStr: String;

procedure SeparCarCdrList(List: TDynDatum; out CarL, CdrL: ISchPair);
var
  Seq: ISchPair;
  Item: TDynDatum;
  CarL1, CdrL1: ISchPair;
begin
  Seq := ISchPair(Pointer(List));
  while GetNext(Seq, Item) do
  begin
    CarL1 := DynTypes.cons(car(Item), CarL1);
    CdrL1 := DynTypes.cons(cdr(Item), CdrL1);
  end;
  CarL := Reverse(CarL1);
  CdrL := Reverse(CdrL1);
end;

procedure SeparCarCadrList(List: TDynDatum; out CarL, CadrL: ISchPair);
var
  Seq: ISchPair;
  Item: TDynDatum;
  CarL1, CadrL1: ISchPair;
begin
  Seq := ISchPair(Pointer(List));
  while GetNext(Seq, Item) do
  begin
    CarL1 := DynTypes.cons(car(Item), CarL1);
    CadrL1 := DynTypes.cons(car(cdr(Item)), CadrL1);
  end;
  CarL := Reverse(CarL1);
  CadrL := Reverse(CadrL1);
end;


{ TSintax1 }

procedure TSintax1._if(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
begin                   // (Cond IfVal ElseVal)
  Eval(Result, car(Datum), Scope);
  Datum := cdr(Datum);    // (IfVal ElseVal)
  if Result.Value = _f then
    Datum := cdr(Datum);  // (ElseVal)
  if IsPair(Datum) then
    Eval(Result, car(Datum), Scope)
  else
    Result.Assign(Undefined);
end;

{
procedure TSintax1.import(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  LibName, ProcNames: TDynDatum;
  Lib: IInterface;
begin
  // (import (libname) procname...)
  NeedParams(Datum, [@LibName], @ProcNames);
  Lib := TSchLib.Create(LibName, ProcNames, Scope);
  Result.Assign(Undefined);
end;

procedure TSintax1._import_on_call(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  LibName, ProcNames: TDynDatum;
  Lib: IInterface;
begin
  // (import-on-call (libname) procname...)
  NeedParams(Datum, [@LibName], @ProcNames);
  Lib := TLazyLib.Create(LibName, ProcNames, Scope);
  Result.Assign(Undefined);
end;
}

procedure TSintax1.define(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
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
        Eval(Result, Car(Rest), Scope)    // (define <variable> <expression>)
      else
      begin
        Name := IDynSymbol(Pointer(VarDatum)).Name;
        raise Exception.Create(Format('invalid define %s . %s', [Name, Deb(Rest)]))
      end
    else
      Result.Assign(Unbound);             // (define <variable>)
  end
  else if IsPair(VarDatum) then
  begin
    Formal := cdr(VarDatum);
    VarDatum := car(VarDatum);
    NeedSymbol(VarDatum);  // (define (<variable> . <formal>) <body>)
    Result.Assign(TDynLambda.Create(Formal, Rest, Scope));
  end
  else
    raise Exception.Create(Format('invalid define %s . %s', [Deb(Datum), Deb(Rest)]));
  Scope.Value[VarDatum] := Result.Value;
end;

procedure TSintax1.dot(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
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
  SubScope: IScope;
  Assoc: IDynAssoc;
  Fn: IDynFunc;
  Method: IDynMethod;
begin
  // (. instance member params)
  ManageRefs([@InstanceR, @ParamsR]);
  NeedParams(Datum, [@Instance, @MemberId], @Params);
  DebStr := Deb(Datum);

  Eval(InstanceR, Instance, Scope);
  if Assigned(Params) then
  begin
    EvalParams(ParamsR, Params, Scope);
    Params := ParamsR.Value;
  end;
  case InstanceR.Value.Kind of
    atScope:
      begin
        CallMember(Result, IScope(Pointer(InstanceR.Value)), MemberId, Params);
        Exit;
        //SubScope := IScope(Pointer(InstanceR.Value));
        //Member := SubScope.Value[MemberId];
      end;
    atAssoc:
      begin
        Assoc := IDynAssoc(Pointer(InstanceR.Value));
        Member := Assoc.Assoc[MemberId];
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
      Result.Assign(Member);
  end;
end;

procedure TSintax1.DotSet(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  Obj, Member, Value: TDynDatum;
  ObjR,       ValueR: TDatumRef;
  SubScope: IScope;
  Assoc: IDynAssoc;
begin
  // (.set! obj member value)
  ManageRefs([@ObjR, @ValueR]);
  NeedParams(Datum, [@Obj, @Member, @Value]);
  DebStr := Deb(Datum);

  Eval(ObjR, Obj, Scope);
  Eval(ValueR, Value, Scope);

  case ObjR.Value.Kind of
    atScope:
      begin
        SubScope := IScope(Pointer(ObjR.Value));
        SubScope.Value[Member] := ValueR.Value;
      end;
    atAssoc:
      begin
        Assoc := IDynAssoc(Pointer(ObjR.Value));
        Assoc.Assoc[Member] := ValueR.Value;
      end;
    else
      begin
        DynError('record required: %s', [Deb(ObjR.Value, 200)]);
      end;
  end;
  Result.Assign(Undefined);
end;

procedure TSintax1.quote(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  Value: TDynDatum;
begin
  // (quote Value)
  NeedParams(Datum, [@Value]);
  Result.Assign(Value);
end;

procedure TSintax1.quasiquote(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  Value: TDynDatum;
begin
  // (quasiquote Value)
  NeedParams(Datum, [@Value]);
  Result.Assign(Value);
  if IsPair(Value) then
  begin
    InitQuoteFn;
    Result.Assign(CopyUnquoting(Value, Scope));
  end;
end;

procedure TSintax1.lambda(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  Params: TDynDatum;
  FnDef: TDynDatum;
begin
  // (lambda Params . FnDef)
  NeedParams(Datum, [@Params], @FnDef);
  NeedPair(Params);
  Result.Assign(TDynLambda.Create(Params, FnDef, Scope));
end;

var
  tmp: String;

procedure TSintax1.let(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
var
  Bindings, Body: TDynDatum;
  x, v: ISchPair;
  FnRef: IDynFunc;
  Par: TDatumRef;
begin
  ManageRefs([@Par]);
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
    FnRef := TDynLambda.Create(Pointer(x), Body, Scope);
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

procedure TSintax1.letrec(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
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

procedure TSintax1.let_2A(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
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
  Scope: IScope);
var
  NameDatum: TDynDatum;
begin
  Result.Assign(Datum);
  NameDatum := car(Datum);
  NeedSymbol(NameDatum);
  Datum := cdr(Datum);
  Result.Assign(car(Datum));
  Eval(Result, Result.Value, Scope);
  Scope.Value[NameDatum] := Result.Value;
end;

function TSintax1.CopyUnquoting(p: TDynDatum; const Scope: IScope): ISchPair;
var
  a, d, b: TDynDatum;
  f: TDynDatum;
  dRef: ISchPair;
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
        ManageRefs([@Res]);
        Eval(Res, b, Scope);
        Result := cons(Res.Value, d);      // -> ((eval b) . d)
        Exit;
      end
      else if f = QuoteFn[tkUnQuoteSplicing].Value then
      begin
        b := car(cdr(a));
        ManageRefs([@Res]);
        Eval(Res, b, Scope);
        Result := AdToEnd(Res.Value, d);
        p.Free;
        Exit;
      end
  end;
  Result := cons(a, d);
end;

function TSintax1.AdToEnd(a, d: TDynDatum): ISchPair;
begin
  if IsPair(a) then
    Result := cons(car(a), AdToEnd(cdr(a), d))
  else
    Result := ISchPair(Pointer(d))
end;

end. ///////////////////////////////////////////////////////////////////////////

