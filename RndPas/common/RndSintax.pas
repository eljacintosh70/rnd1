unit RndSintax;

interface

uses
  DynTypes;

    // (call FnName Params)
    procedure call(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);

const
  TRndSintax: array[0..0] of TLispSyntaxRec = (
    (Name: 'call';    Fn: call));

implementation

{ TRndSintax }

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

end.

