unit RndBase;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  Generics.Collections;

type
  IDatum = interface
    function Eval: IDatum;
    procedure Write(Stream: TStream);
  end;
function Display(Val: IDatum): String;
procedure Error(Msg: String); overload;
procedure Error(MsgFm: String; Val: IDatum); overload;

type
  IValue<T> = interface(IDatum)
    function GetValue: T;
    procedure SetValue(const AValue: T);
    property Value: T read GetValue write SetValue;
  end;

  ISymbol = interface(IValue<IDatum>)
    ['{6033F2D0-2486-42E0-B7BD-AB5391206152}']
    function Name: String;
  end;
function MakeSymbol(Name: String): ISymbol;
procedure NeedSymbol(Name: IDatum; var Sym: ISymbol);

type
  INode = interface(IValue<IDatum>)
    ['{8E8AF832-E561-4687-A1EC-3F50059FC3F4}']
    function Next: INode;
    procedure WriteValues(Stream: TStream);
    function EvalItems: INode;
  end;
function cons(Value: IDatum; Next: INode): INode;
function List(const Value: array of IDatum): INode;
function ListCount(List: INode): Integer;
function Reverse(List: INode): INode;

type
  ISyntax = interface(IDatum)
    ['{A3B86FD0-C9FD-45F4-8666-283A5676B337}']
    function Apply(Arg: INode): IDatum;
  end;

type
  INumber = interface(IValue<Double>)
    ['{AE186958-8ECF-486E-BE62-4BB53AD7C96E}']
  end;
function MakeNumber(Num: Double): INumber;
procedure NeedNumber(Datum: IDatum; var Val: Double);

var
  Symbols: TDictionary<String,ISymbol>;

implementation

uses
  RndClass;

function Display(Val: IDatum): String;
var
  Strm: TStringStream;
begin
  if Val = nil then
    Result := '()'
  else
  begin
    Strm := TStringStream.Create('' {$IFNDEF FPC}, TEncoding.Unicode {$ENDIF});
    Val.Write(Strm);
    Result := Strm.DataString;
    Strm.Free;
  end;
end;

procedure Error(Msg: String);
begin
  raise Exception.Create(Msg) {$IFNDEF FPC} at ReturnAddress {$ENDIF};
end;

procedure Error(MsgFm: String; Val: IDatum);
var
  sVal: String;
begin
  sVal := Display(Val);
  raise Exception.CreateFmt(MsgFm, [sVal]) {$IFNDEF FPC} at ReturnAddress {$ENDIF};
end;

function MakeSymbol(Name: String): ISymbol;
begin
  if not Symbols.TryGetValue(Name, Result) then
  begin
    Result := TSymbol.Create(Name);
    Symbols.Add(Name, Result);
  end;
end;

procedure NeedSymbol(Name: IDatum; var Sym: ISymbol);
begin
  if not Supports(Name, ISymbol, Sym) then
    Error('%s is not a symbol', Name);
end;

function cons(Value: IDatum; Next: INode): INode;
var
  Node: TNode;
begin
  Node := TNode.Create(Value, Next);
  Result := Node;
end;

function List(const Value: array of IDatum): INode;
var
  i: Integer;
begin
  Result := nil;
  for i := High(Value) downto Low(Value) do
    Result := cons(Value[i], Result);
end;

function ListCount(List: INode): Integer;
begin
  Result := 0;
  while List <> nil do
  begin
    List := List.Next;
    Inc(Result);
  end;
end;

procedure Reverse2(List: INode; var Res: INode);
begin
  if List <> nil then
  begin
    Res := cons(List.Value, Res);
    Reverse2(List.Next, Res);
  end;
end;

function Reverse(List: INode): INode;
begin
  Result := nil;
  Reverse2(List, Result);
end;

function MakeNumber(Num: Double): INumber;
begin
  Result := TNumber.Create(Num);
end;

procedure NeedNumber(Datum: IDatum; var Val: Double);
var
  Num: INumber;
begin
  if not Supports(Datum, INumber, Num) then
    Error('%s is not a number', Num);
  Val := Num.Value;
end;

initialization
  Symbols := TDictionary<String,ISymbol>.Create;
end.
