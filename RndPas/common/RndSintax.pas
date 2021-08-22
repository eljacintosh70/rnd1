unit RndSintax;

interface

uses
  DynTypes;

type
  {$TYPEINFO ON}
  TRndSintax = class(TObject)
  published
    // (call FnName Params)
    procedure call(out Result: TDatumRef; Datum: TDynDatum; Scope: IScope);
  end;

implementation

uses
  LispEval;

{ TRndSintax }

procedure TRndSintax.call(out Result: TDatumRef; Datum: TDynDatum;
  Scope: IScope);
var
  FnName, Params: TDynDatum;
begin
  NeedParams(Datum, [@FnName, @Params]);
  EvalCall(Result, FnName, Params, Scope);
end;

end.

