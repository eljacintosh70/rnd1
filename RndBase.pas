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

  ISymbol = interface(IDatum)
    function Name: String;
    function GetValue: IDatum;
    procedure SetValue(AValue: IDatum);
    property Value: IDatum read GetValue write SetValue;
  end;

  INode = interface(IDatum)
    function GetValue: IDatum;
    procedure SetValue(AValue: IDatum);
    property Value: IDatum read GetValue write SetValue;
    function Next: INode;
    procedure WriteValues(Stream: TStream);
    function EvalItems: INode;
  end;

  ILambda = interface(IDatum)
    ['{18A1ACFD-A52C-4AE9-929B-23DCB62CB104}']
    function Apply(Arg: INode): IDatum;
  end;

  INumber = interface(IDatum)
    function GetValue: Double;
    procedure SetValue(AValue: Double);
    property Value: Double read GetValue write SetValue;
  end;

function MakeSymbol(Name: String): ISymbol;
function cons(Value: IDatum; Next: INode): INode;
function Reverse(List: INode): INode;
function MakeNumber(Num: Double): INumber;

var
  Symbols: TDictionary<String,ISymbol>;

implementation

uses
  RndClass;

function MakeSymbol(Name: String): ISymbol;
begin
  if not Symbols.TryGetValue(Name, Result) then
  begin
    Result := TSymbol.Create(Name);
    Symbols.Add(Name, Result);
  end;
end;

function cons(Value: IDatum; Next: INode): INode;
var
  Node: TNode;
begin
  Node := TNode.Create(Value, Next);
  Result := Node;
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

initialization
  Symbols := TDictionary<String,ISymbol>.Create;
end.
