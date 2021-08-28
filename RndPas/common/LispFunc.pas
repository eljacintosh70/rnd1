unit LispFunc;
interface //////////////////////////////////////////////////////////////////////

{$WARN SYMBOL_DEPRECATED OFF} // en Delphi200x no recomiendan usar automated

uses
  SysUtils,
  DUtils, DynTypes, SchFunc, rnrs_lists;

type
  {$TYPEINFO ON}
  {--$METHODINFO ON}  // funciona en Delphi200x

  TBasicFunctions = class(TObject)
  published
    // (list a b c ...)
    procedure list(out Result: TDatumRef; Datum: TDynDatum);
    // (apply fn params)
    procedure apply(out Result: TDatumRef; Datum: TDynDatum);
    procedure _car(out Result: TDatumRef; Datum: TDynDatum);
    procedure _cdr(out Result: TDatumRef; Datum: TDynDatum);
    procedure _cons(out Result: TDatumRef; Datum: TDynDatum);
    // https://srfi.schemers.org/srfi-1/srfi-1.html#FoldUnfoldMap
    // (map proc clist1 clist2 ...) -> list
    procedure map(out Result: TDatumRef; Datum: TDynDatum);

    // (vector a b c ...)
    procedure vector(out Result: TDatumRef; Datum: TDynDatum);
    // (make-vector size [fill])
    procedure _make_vector(out Result: TDatumRef; Datum: TDynDatum);
    // (vector-ref vec i)
    procedure _vector_ref(out Result: TDatumRef; Datum: TDynDatum);
    // (vector-set! vec i val)
    procedure _vector_set_X(out Result: TDatumRef; Datum: TDynDatum);
    // https://srfi.schemers.org/srfi-133/srfi-133.html#Conversion
    // (vector->list vec [start [end]]) -> proper-list
    procedure vector_list(out Result: TDatumRef; Datum: TDynDatum);
    // (list->vector proper-list) -> vector
    procedure list_vector(out Result: TDatumRef; Datum: TDynDatum);
    // (string->vector string [start [end]]) -> vector
    // (vector->string vec [start [end]]) -> string
    // (vector-map f vec1 vec2 ...) -> vector

    {
    // (make-record size )
    procedure _make_record(out Result: TDatumRef; Datum: TDynDatum);
    // (_record_ref rec i)
    procedure _record_ref(out Result: TDatumRef; Datum: TDynDatum);
    // (record-set! rec i val)
    procedure _record_set_X(out Result: TDatumRef; Datum: TDynDatum);
    }
    procedure _null_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _boolean_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _char_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _num_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _symbol_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _string_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _pair_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _vector_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _byte_vector_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _record_P(out Result: TDatumRef; Datum: TDynDatum);
    procedure _procedure_P(out Result: TDatumRef; Datum: TDynDatum);

    // http://srfi.schemers.org/srfi-9/srfi-9.html
    {
      (define-syntax define-record-type
        (syntax-rules ()
          ((define-record-type type
             (constructor constructor-tag ...)
             predicate
             (field-tag accessor . more) ...)
           (begin
             (define type
               (make-record-type 'type '(field-tag ...)))
             (define constructor
               (record-constructor type '(constructor-tag ...)))
             (define predicate
               (record-predicate type))
             (define-record-field type field-tag accessor . more)
             ...))))
    // (make-record-type name field-tags)
    }


    // métodos de comparación, referencia
    // http://sicp.ai.mit.edu/Fall-2003/manuals/scheme-7.5.5/doc/scheme_4.html
    // (eq? A B)
    procedure _eq_P(out Result: TDatumRef; Datum: TDynDatum);
    // (eqv? A B)
    procedure _eqv_P(out Result: TDatumRef; Datum: TDynDatum);
    // (equal? A B)
    procedure _equal_P(out Result: TDatumRef; Datum: TDynDatum);

    // (string-append str ...)
    procedure _string_append(out Result: TDatumRef; Datum: TDynDatum);
  automated
  end;

{
	(define (load filename)
	  (eval (read-all-as-a-begin-expr filename)
      (interaction-environment)))

(read)
(read port)
  Read converts external representations of Scheme objects
  into the objects themselves.

,open cargar librería
}

  TMathOpers = class(TObject)
  published
    // (= x y ...)
    procedure nEQ(out Result: TDatumRef; Datum: TDynDatum);
    // (>= x y ...)
    procedure nGE(out Result: TDatumRef; Datum: TDynDatum);
    // (> x y ...)
    procedure nGT(out Result: TDatumRef; Datum: TDynDatum);
    // (<= x y ...)
    procedure nLE(out Result: TDatumRef; Datum: TDynDatum);
    // (< x y ...)
    procedure nLT(out Result: TDatumRef; Datum: TDynDatum);
  published
    //--- operadores que podrían generar enteros o punto flotante
    // (+ x y ...)
    procedure Add(out Result: TDatumRef; Datum: TDynDatum);
    // (* x y ...)
    procedure Mult(out Result: TDatumRef; Datum: TDynDatum);
    // (/ x y)
    procedure Divide(out Result: TDatumRef; Datum: TDynDatum);
    // (- x y)
    procedure Subst(out Result: TDatumRef; Datum: TDynDatum);
  automated
    function flabs(x: Real): Real;
    // (fllog x)
    function fllog(x: Real): Real;
    // (flexp x)
    function flexp(x: Real): Real;
    // (flsqrt x)
    function flsqrt(x: Real): Real;
    // (^ x y)
    function Pow(x, y: Real): Real;
    // (hex val nDig)
    function hex(val, nDig: Integer): String;
  end;

implementation /////////////////////////////////////////////////////////////////

var
  TempDebug: Variant;

procedure Debug(a: TDynDatum);
begin
  try
    TempDebug := a.AsVariant;
  except
    TempDebug := 'Error';
  end;
end;

procedure AssignInt64(out Result: TDatumRef; val: Int64);
begin
  case val of
    Low(FixNum)..High(FixNum):
      Result.Assign(CreateFixNum(val));
    else
      Result.Assign(CreateInt64NR(val));
  end;
end;

{ TBasicFunctions }

procedure TBasicFunctions.apply(out Result: TDatumRef; Datum: TDynDatum);
var
  Fn, Params: TDynDatum;
// (apply fn params)
begin
  NeedParams(Datum, [@Fn, @Params]);
  case Fn.Kind of
    atExtFunc, atAutoFunc, atLambda:
      IDynFunc(Pointer(Fn)).Call(Result, Params);
    else DynError('Non Callable object', [Fn]);
  end;
end;

procedure TBasicFunctions.List(out Result: TDatumRef; Datum: TDynDatum);
begin
  Result.Assign(Datum);
end;

procedure TBasicFunctions._car(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(car(A));
end;

procedure TBasicFunctions._cdr(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(cdr(A));
end;

procedure TBasicFunctions._cons(out Result: TDatumRef; Datum: TDynDatum);
var
  A, D: TDynDatum;
begin
  NeedParams(Datum, [@A, @D]);
  Result.cons(A, D);
end;

procedure TBasicFunctions.list_vector(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
  Res: IDynArray;
begin
  // (list->vector proper-list) -> vector
  NeedParams(Datum, [@A]);
  Res := ListToDynArray(A);
  Result.Assign(Res);
end;

procedure TBasicFunctions.map(out Result: TDatumRef; Datum: TDynDatum);
var
  A, D: TDynDatum;
  Fn: IDynFunc;
  Res: IDynPair;
begin
  // (map proc clist1 clist2 ...) -> list
  NeedParams(Datum, [@A, @D]);
  NeedInterface(A, IDynFunc,Fn);
  Res := Map2(Fn, IDynPair(Pointer(D)));
  Result.Assign(Res);
end;

procedure TBasicFunctions.vector(out Result: TDatumRef; Datum: TDynDatum);
begin
  Result.Assign(ListToDynArray(Datum));
end;

procedure TBasicFunctions.vector_list(out Result: TDatumRef; Datum: TDynDatum);
var
  A, D: TDynDatum;
  ArgStart, ArgEnd: TDynDatum;
  NStart, NEnd: Integer;
  Vec: IDynArray;
begin
  // (vector->list vec [start [end]]) -> proper-list
  NeedParams(Datum, [@A], @D);
  NeedVector(A, Vec);
  if Assigned(D) then
  begin
    ArgStart := car(D);
    NeedInteger(ArgStart, NStart);

    D := cdr(D);
    if Assigned(D) then
    begin
      ArgEnd := car(D);
      NeedInteger(ArgEnd, NEnd);
    end
    else
      NEnd := Vec.Length - 1;
  end
  else
  begin
    NStart := 0;
    NEnd := Vec.Length - 1;
  end;
  Result.Assign(VectorToList(Vec, NStart, NEnd));
end;

procedure TBasicFunctions._null_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsNull(A)));
end;

procedure TBasicFunctions._boolean_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsBoolean(A)));
end;

procedure TBasicFunctions._char_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsChar(A)));
end;

procedure TBasicFunctions._num_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsNum(A)));
end;

procedure TBasicFunctions._symbol_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsSymbol(A)));
end;

procedure TBasicFunctions._string_append(out Result: TDatumRef; Datum: TDynDatum);
// (string-append str ...)
var
  Args: TDynDatum;
  s, sRes: String;
begin
  Args := Datum;
  sRes := '';
  while Args <> nil do
  begin
    NeedString(car(Args), s);
    Args := cdr(Args);
    sRes := sRes + s;
  end;
  Result.Assign(make_string(sRes));
end;

procedure TBasicFunctions._string_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsString(A)));
end;

procedure TBasicFunctions._pair_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsPair(A)));
end;

procedure TBasicFunctions._vector_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsVector(A)));
end;

procedure TBasicFunctions._byte_vector_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsByteVector(A)));
end;

procedure TBasicFunctions._record_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsRecord(A)));
end;

procedure TBasicFunctions._procedure_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A: TDynDatum;
begin
  NeedParams(Datum, [@A]);
  Result.Assign(BoolDatum(IsProcedure(A)));
end;

procedure TBasicFunctions._eq_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A, B: TDynDatum;
  Res: Boolean;
begin
  NeedParams(Datum, [@A, @B]);
  // si ambos apuntan al mismo objeto,
  // o son símbolos del mismo nombre
  Res := ((Pointer(A) = Pointer(B)));{ or
    (IsSymbol(A) and IsSymbol(B)
      and (TSymbolAtom(A).Name = TSymbolAtom(B).Name)));}
  Result.Assign(BoolDatum(Res));
end;

procedure TBasicFunctions._eqv_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A, B: TDynDatum;
  Res: Boolean;
begin
  NeedParams(Datum, [@A, @B]);
  // si ambos apuntan al mismo objeto,
  // o son símbolos, caracteres o números iguales
  Res := eqv(A,B);
  Result.Assign(BoolDatum(Res));
end;

procedure TBasicFunctions._equal_P(out Result: TDatumRef; Datum: TDynDatum);
var
  A, B: TDynDatum;
  Res: Boolean;
begin
  NeedParams(Datum, [@A, @B]);
  // si ambos apuntan al mismo objeto,
  // o son iguales los valores
  Res := Equal(A, B);
  { TODO : agregar comparaciones para strings, listas y vectores }
  Result.Assign(BoolDatum(Res));
end;

procedure TBasicFunctions._make_vector(out Result: TDatumRef; Datum: TDynDatum);
var
  size: TDynDatum;
  fill: TDynDatum;
  Vec: IDynArray;
  n: Integer;
begin
  // (make-vector size [fill])
  NeedParams(Datum, [@size], @fill);
  if IsPair(fill) then
    fill := car(fill)
  else
    fill := Unbound;
  NeedInteger(size, n);
  Vec := make_vector(n, fill);
  Result.Assign(Vec);
end;
(*
procedure TBasicFunctions._make_record(out Result: TDatumRef; Datum: TDynDatum);
var
  size: TDynDatum;
  Rec: TRecordDatum;
  n: Integer;
begin
  // (make-record size)
  NeedParams(Datum, [@size]);
  NeedInteger(size, n);
  { TODO : reemplazar función }
  //Rec := TRecordDatum.Create(n);
  Result.Assign(Rec);
end;

procedure TBasicFunctions._record_ref(out Result: TDatumRef; Datum: TDynDatum);
var
  Rec, Index: TDynDatum;
  i: Integer;
begin
  // (_record_ref rec i)
  NeedParams(Datum, [@Rec, @Index]);
  NeedType(Rec, atRecord);
  NeedInteger(Index, i);
  Result.Assign(TRecordDatum(Rec)[i]);
end;

procedure TBasicFunctions._record_set_X(out Result: TDatumRef; Datum: TDynDatum);
var
  Rec, Index, Val: TDynDatum;
  i: Integer;
begin
  // (record-set! rec i val)
  NeedParams(Datum, [@Rec, @Index, @Val]);
  NeedType(Rec, atRecord);
  NeedInteger(Index, i);
  TRecordDatum(Rec)[i] := Val;
  Result.Assign(Undefined);
end;
*)
procedure TBasicFunctions._vector_ref(out Result: TDatumRef; Datum: TDynDatum);
var
  Vec, Index: TDynDatum;
  i: Integer;
begin
  // (vector-ref vec i)
  NeedParams(Datum, [@Vec, @Index]);
  NeedType(Vec, atVector);
  NeedInteger(Index, i);
  Result.Assign(IDynArray(Pointer(Vec))[i]);
end;

procedure TBasicFunctions._vector_set_X(out Result: TDatumRef; Datum: TDynDatum);
var
  Vec, Index, Val: TDynDatum;
  i: Integer;
begin
  // (vector-set! vec i val)
  NeedParams(Datum, [@Vec, @Index, @Val]);
  NeedType(Vec, atVector);
  NeedInteger(Index, i);
  IDynArray(Pointer(Vec))[i] := Val;
  Result.Assign(Undefined);
end;

{ TMathOpers }

procedure TMathOpers.nEQ(out Result: TDatumRef; Datum: TDynDatum);
var
  a: TDynDatum;
  n1, n2: Real;
begin
  Result.Assign(_f);
  if not IsPair(Datum) then Exit;
  a := car(Datum);

  if not IsNum(a) then Exit;
  n1 := dyn(a);

  Datum := cdr(Datum);
  while IsPair(Datum) do
  begin
    a := car(Datum);
    Debug(a);
    if not IsNum(a) then Exit;
    n2 := dyn(a);
    if not (n1 = n2) then Exit; // linea diferente
    n1 := n2;
    Datum := cdr(Datum);
  end;
  Result.Assign(_t);
end;

procedure TMathOpers.nGE(out Result: TDatumRef; Datum: TDynDatum);
var
  a: TDynDatum;
  n1, n2: Real;
begin
  Result.Assign(_f);
  if not IsPair(Datum) then Exit;
  a := car(Datum);

  if not IsNum(a) then Exit;
  n1 := dyn(a);

  Datum := cdr(Datum);
  while IsPair(Datum) do
  begin
    a := car(Datum);
    if not IsNum(a) then Exit;
    n2 := dyn(a);
    if not (n1 >= n2) then Exit; // linea diferente
    n1 := n2;
    Datum := cdr(Datum);
  end;
  Result.Assign(_t);
end;

procedure TMathOpers.nGT(out Result: TDatumRef; Datum: TDynDatum);
var
  a: TDynDatum;
  n1, n2: Real;
begin
  Result.Assign(_f);
  if not IsPair(Datum) then Exit;
  a := car(Datum);

  if not IsNum(a) then Exit;
  n1 := dyn(a);

  Datum := cdr(Datum);
  while IsPair(Datum) do
  begin
    a := car(Datum);
    if not IsNum(a) then Exit;
    n2 := dyn(a);
    if not (n1 > n2) then Exit; // linea diferente
    n1 := n2;
    Datum := cdr(Datum);
  end;
  Result.Assign(_t);
end;

procedure TMathOpers.nLE(out Result: TDatumRef; Datum: TDynDatum);
var
  a: TDynDatum;
  n1, n2: Real;
begin
  Result.Assign(_f);
  if not IsPair(Datum) then Exit;
  a := car(Datum);

  if not IsNum(a) then Exit;
  n1 := dyn(a);

  Datum := cdr(Datum);
  while IsPair(Datum) do
  begin
    a := car(Datum);
    if not IsNum(a) then Exit;
    n2 := dyn(a);
    if not (n1 <= n2) then Exit; // linea diferente
    n1 := n2;
    Datum := cdr(Datum);
  end;
  Result.Assign(_t);
end;

procedure TMathOpers.nLT(out Result: TDatumRef; Datum: TDynDatum);
var
  a: TDynDatum;
  n1, n2: Real;
begin
  Result.Assign(_f);
  if not IsPair(Datum) then Exit;
  a := car(Datum);

  if not IsNum(a) then Exit;
  n1 := dyn(a);

  Datum := cdr(Datum);
  while IsPair(Datum) do
  begin
    a := car(Datum);
    if not IsNum(a) then Exit;
    n2 := dyn(a);
    if not (n1 < n2) then Exit; // linea diferente
    n1 := n2;
    Datum := cdr(Datum);
  end;
  Result.Assign(_t);
end;

procedure TMathOpers.Add(out Result: TDatumRef; Datum: TDynDatum);
var
  ResI, Val: Int64;
  Res: Real;
  a: TDynDatum;
begin
  Res := 0.0;
  if IsPair(Datum) then
  begin
    a := car(Datum);
    if IsInteger(a) then
    begin
      ResI := a.AsInteger;
      // operación con enteros
      repeat
        Datum := cdr(Datum);
        if not IsPair(Datum) then
        begin
          AssignInt64(Result, ResI);
          Exit;
        end;
        a := car(Datum);
        if not IsInteger(a) then
          Break;                  // continuar con punto flotante
        Val := a.AsInteger;
        ResI := ResI + Val;
      until False;
      Res := ResI;
    end
    else if IsNum(a) then
      Res := 0.0   // continuar con punto flotante Datum señala al primer item a sumar
    else
      DynError('Number expected but %s found', [Deb(a)]);
  end;

  while IsPair(Datum) do
  begin
    a := car(Datum);
    if IsNum(a) then
      Res := Res + Real(dyn(a))
    else
      DynError('Number expected but %s found', [Deb(a)]);
    Datum := cdr(Datum);
  end;
  Result.Assign(CreateFloNumNR(Res));
end;

procedure TMathOpers.Mult(out Result: TDatumRef; Datum: TDynDatum);
var
  ResI, Val: Int64;
  Res: Real;
  a: TDynDatum;
begin
  Res := 1.0;
  if IsPair(Datum) then
  begin
    a := car(Datum);
    if IsInteger(a) then
    begin
      ResI := a.AsInteger;
      // operación con enteros
      repeat
        Datum := cdr(Datum);
        if not IsPair(Datum) then
        begin
          AssignInt64(Result, ResI);
          Exit;
        end;
        a := car(Datum);
        if not IsInteger(a) then
          Break;                  // continuar con punto flotante
        Val := a.AsInteger;
        ResI := ResI * Val;
      until False;
      Res := ResI;
    end
    else if IsNum(a) then
      Res := 1.0   // continuar con punto flotante Datum señala al primer item a sumar
    else
    begin
      DynError('Number expected but %s found', [Deb(a)]);
      Exit;
    end;
  end;

  while IsPair(Datum) do
  begin
    a := car(Datum);
    if IsNum(a) then
      Res := Res * Real(dyn(a));
    //else ERROR
    Datum := cdr(Datum);
  end;
  Result.Assign(CreateFloNumNR(Res));
end;

procedure TMathOpers.Divide(out Result: TDatumRef; Datum: TDynDatum);
var
  ResI, Val: Int64;
  Res: Real;
  a: TDynDatum;
begin
  Res := 1.0;
  if IsPair(Datum) then
  begin
    a := car(Datum);
    if IsInteger(a) then
    begin
      ResI := a.AsInteger;
      // operación con enteros
      repeat
        Datum := cdr(Datum);
        if not IsPair(Datum) then
        begin
          AssignInt64(Result, ResI);
          Exit;
        end;
        a := car(Datum);
        if not IsInteger(a) then
          Break;                  // continuar con punto flotante
        Val := a.AsInteger;
        if ResI mod Val <> 0 then
          Break;                  // continuar con punto flotante (no definí racional)
        ResI := ResI div Val;
      until False;
      Res := ResI;
    end
    else if IsNum(a) then
    begin
      Res := dyn(a); // continuar con punto flotante
      Datum := cdr(Datum);   // datum señala al segundo elemento
    end
    else
    begin
      DynError('Number expected but %s found', [Deb(a)]);
      Exit;
    end;
  end;

  while IsPair(Datum) do
  begin
    a := car(Datum);
    if IsNum(a) then
      Res := Res / Real(dyn(a));
    //else ERROR
    Datum := cdr(Datum);
  end;
  Result.Assign(CreateFloNumNR(Res));
end;

procedure TMathOpers.Subst(out Result: TDatumRef; Datum: TDynDatum);
var
  ResI, Val: Int64;
  Res: Real;
  a: TDynDatum;
begin
  //ResI := 0;
  if IsPair(Datum) then
  begin
    a := car(Datum);
    if IsInteger(a) then
    begin
      // operación con enteros
      ResI := a.AsInteger;
      Datum := cdr(Datum);
      if not IsPair(Datum) then
      begin
        AssignInt64(Result, -ResI);  // (- x) -> -x
        Exit;
      end;

      a := car(Datum);
      if IsInteger(a) then
      begin
        Val := a.AsInteger;
        if IsPair(cdr(Datum)) then
          DynError('Too many parameters in -', []);
        AssignInt64(Result, ResI - Val);    // (- x y) -> x - y
      end
      else if IsNum(a) then
      begin
        Res := ResI - Real(dyn(a));
        if IsPair(cdr(Datum)) then
          DynError('Too many parameters in -', []);
        Result.Assign(CreateFloNumNR(Res));  // (- x y) -> x - y
      end;
    end
    else if IsNum(a) then
    begin
      // operación con punto flotante
      Res := dyn(a);
      Datum := cdr(Datum);
      if not IsPair(Datum) then
      begin
        Result.Assign(CreateFloNumNR(-Res));  // (- x) -> -x
        Exit;
      end;

      a := car(Datum);
      if IsNum(a) then
      begin
        Res := Res - Real(dyn(a));
        if IsPair(cdr(Datum)) then
          DynError('Too many parameters in -', []);
        Result.Assign(CreateFloNumNR(Res));  // (- x y) -> x - y
      end
      else
        DynError('Number expected but %s found', [Deb(a)]);
    end
    else
    begin
      DynError('Number expected but %s found', [Deb(a)]);
      Exit;
    end;
  end
  else
    DynError('Not enought parameters in (-)', []);
end;

function TMathOpers.flabs(x: Real): Real;
begin
  Result := Abs(x)
end;

function TMathOpers.fllog(x: Real): Real;
begin
  Result := Ln(x)
end;

function TMathOpers.flexp(x: Real): Real;
begin
  Result := Exp(x)
end;

function TMathOpers.flsqrt(x: Real): Real;
begin
  Result := sqrt(x)
end;

function TMathOpers.Pow(x, y: Real): Real;
begin
  Result := Exp(Ln(x) * y);
end;

function TMathOpers.hex(val, nDig: Integer): String;
begin
  Result := IntToHex(val, nDig);
end;

end. ///////////////////////////////////////////////////////////////////////////
