unit RndSintax;

interface

uses
  DynTypes;

type
  {$TYPEINFO ON}
  TRndSintax = class(TObject)
  published
    // (call FnName Params)
    procedure call(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
  end;

implementation

{ TRndSintax }

procedure TRndSintax.call(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IDynScope);
var
  FnName, Params: TDynDatum;
  f: dyn;
begin
  NeedParams(Datum, [@FnName, @Params]);
  f := cons(FnName, Params);
  Result := Eval(f, Scope);
end;

end.

