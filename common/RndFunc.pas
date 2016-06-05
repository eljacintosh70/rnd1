unit RndFunc;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RndBase, RndClass;

type
  TLambdaClass = class of TSyntax;
  TLambdaEntry = record
    Name: String;
    ClassId: TLambdaClass;
  end;

  TNumFunction = function (const Val: array of Double): Double;
  TNumFunctionEntry = record
    Name: String;
    Fn: TNumFunction;
  end;

  TDefine = class(TSyntax, ISyntax)
  public
    function Apply(Arg: INode): IDatum; override;
  end;

  TListFn = class(TSyntax)
  public
    function Apply(Arg: INode): IDatum; override;
  end;

  TNumFn = class(TSyntax)
  public
    Apply2: TNumFunction;
    function Apply(Arg: INode): IDatum; override;
  end;

function FnAdd(const Val: array of Double): Double;
function FnMul(const Val: array of Double): Double;
function FnSub(const Val: array of Double): Double;
function FnDiv(const Val: array of Double): Double;

procedure DefineFunctions(const Defs: array of TLambdaEntry);
procedure DefineNumFunctions(const Defs: array of TNumFunctionEntry);

const
  Functions: array[0..1] of TLambdaEntry = (
   (Name: 'define'; ClassId:TDefine),
   (Name: 'list'; ClassId:TListFn));
  NumFunctions: array[0..3] of TNumFunctionEntry = (
   (Name: '+'; Fn: FnAdd),
   (Name: '*'; Fn: FnMul),
   (Name: '-'; Fn: FnSub),
   (Name: '/'; Fn: FnDiv));

implementation

procedure DefineFunctions(const Defs: array of TLambdaEntry);
var
  Sym: ISymbol;
  Value: ISyntax;
  i: Integer;
begin
  for i := Low(Defs) to High(Defs) do
  begin
    Sym := MakeSymbol(Defs[i].Name);
    Value := Defs[i].ClassId.Create;
    Sym.Value := Value;
  end;
end;

procedure DefineNumFunctions(const Defs: array of TNumFunctionEntry);
var
  Sym: ISymbol;
  Value: TNumFn;
  i: Integer;
begin
  for i := Low(Defs) to High(Defs) do
  begin
    Sym := MakeSymbol(Defs[i].Name);
    Value := TNumFn.Create;
    Value.Apply2 := Defs[i].Fn;
    Sym.Value := ISyntax(Value);
  end;
end;

{ TDefine }

function TDefine.Apply(Arg: INode): IDatum;
var
  Name, Value: IDatum;
  Sym: ISymbol;
begin
  Name := Arg.Value;
  NeedSymbol(Name, Sym);

  Arg := Arg.Next;
  Value := Arg.Value.Eval;
  if (Arg.Next <> nil) then
    Error('Too many parameters');

  Sym.Value := Value;
  Result := Value;  // mostrar el nombre o el valor?
end;

{ TListFn }

function TListFn.Apply(Arg: INode): IDatum;
begin
  Result := Arg.EvalItems
end;

{ TNumFn }

function TNumFn.Apply(Arg: INode): IDatum;
var
  Values: array of Double;
  i, n: Integer;
  Res: Double;
begin
  Arg := Arg.EvalItems;
  n := ListCount(Arg);
  SetLength(Values, n);
  for i := 0 to n - 1 do
  begin
    NeedNumber(Arg.Value, Values[i]);
    Arg := Arg.Next;
  end;
  Res := Apply2(Values);
  Result := MakeNumber(Res);
end;

function FnAdd(const Val: array of Double): Double;
var
  i: Integer;
begin
  Result := 0;                          // (+) = 0
  for i := Low(Val) to High(Val) do
    Result := Result + Val[i];
end;

function FnMul(const Val: array of Double): Double;
var
  i: Integer;
begin
  Result := 1;                          // (*) = 1
  for i := Low(Val) to High(Val) do
    Result := Result * Val[i];
end;

function FnSub(const Val: array of Double): Double;
var
  i: Integer;
begin
  if Length(Val) <= 0 then
    Error('"-" requires at least one parameter');
  if Length(Val) = 1 then
  begin
    Result := - Val[0];                // (- a) = -a
    Exit;
  end;

  Result := Val[0];
  for i := 1 to High(Val) do         //  (- a b c ...) = a - b - c - ...
    Result := Result - Val[i];
end;

function FnDiv(const Val: array of Double): Double;
var
  i: Integer;
begin
  if Length(Val) <= 0 then
    Error('"/" requires at least one parameter');
  if Length(Val) = 1 then
  begin
    Result := 1 / Val[0];           // (/ a) = 1 / a
    Exit;
  end;

  Result := Val[0];
  for i := 1 to High(Val) do       //  (/ a b c ...) = a / b / c / ...
    Result := Result / Val[i];
end;

initialization
  DefineFunctions(Functions);
  DefineNumFunctions(NumFunctions);
end.
