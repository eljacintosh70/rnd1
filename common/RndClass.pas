unit RndClass;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RndBase;

type
  TDatum = class(TInterfacedObject)
  protected
    procedure WriteString(Stream: TStream; s: String);
  public
    function Eval: IDatum; virtual; abstract;
    procedure Write(Stream: TStream); virtual; abstract;
  end;

  TBaseValue<T> = class(TDatum)
  protected
    FValue: T;
    function GetValue: T;
    procedure SetValue(const AValue: T);
  public
    constructor Create(const AValue: T);
    property Value: T read GetValue write SetValue;
  end;

  TSymbol = class(TBaseValue<IDatum>, ISymbol)
  protected
    FName: String;
  public
    constructor Create(AName: String);
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
    function Name: String;
  end;

  TNode = class(TBaseValue<IDatum>, INode)
  protected
    FNext: INode;
  public
    constructor Create(AValue: IDatum; ANext: INode);
    function Eval: IDatum; override;
    function EvalItems: INode;
    function Next: INode;
    procedure Write(Stream: TStream); override;
    procedure WriteValues(Stream: TStream);
  end;

  TSyntax = class(TDatum, ISyntax)
  public
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
    function Apply(Arg: INode): IDatum; virtual; abstract;
  end;

  TNumber = class(TBaseValue<Double>, INumber)
  public
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
  end;

implementation


{ TDatum }

procedure TDatum.WriteString(Stream: TStream; s: String);
begin
  Stream.Write(Pointer(s)^, Length(s) * SizeOf(Char));
end;

{ TBaseValue<T> }

function TBaseValue<T>.GetValue: T;
begin
  Result := FValue
end;

procedure TBaseValue<T>.SetValue(const AValue: T);
begin
  FValue := AValue
end;

constructor TBaseValue<T>.Create(const AValue: T);
begin
  FValue := AValue
end;

{ TSymbol }

constructor TSymbol.Create(AName: String);
begin
  FName := AName
end;

function TSymbol.Eval: IDatum;
begin
  Result := FValue
end;

function TSymbol.Name: String;
begin
  Result := FName
end;

procedure TSymbol.Write(Stream: TStream);
begin
  WriteString(Stream, FName);
end;

{ TNode }

constructor TNode.Create(AValue: IDatum; ANext: INode);
begin
  FValue := AValue;
  FNext := ANext;
end;

function TNode.Eval: IDatum;
var
  Value2: IDatum;
  Lambda: ISyntax;
begin
  Value2 := Value.Eval;
  if Supports(Value2, ISyntax, Lambda) then
    Result := Lambda.Apply(Next)
  else
    Error('function expected');
end;

function TNode.EvalItems: INode;
var
  Value2: IDatum;
  Next2: INode;
begin
  Value2 := Value.Eval;
  if Assigned(FNext) then
    Next2 := FNext.EvalItems;
  Result := cons(Value2, Next2);
end;

function TNode.Next: INode;
begin
  Result := FNext
end;

procedure TNode.Write(Stream: TStream);
begin
  WriteString(Stream, '(');
  WriteValues(Stream);
  WriteString(Stream, ')');
end;

procedure TNode.WriteValues(Stream: TStream);
begin
  FValue.Write(Stream);
  if Assigned(FNext) then
  begin
    WriteString(Stream, ' ');
    FNext.WriteValues(Stream);
  end;
end;

{ TLambda }

function TSyntax.Eval: IDatum;
begin
  Result := ISyntax(Self)
end;

procedure TSyntax.Write(Stream: TStream);
begin
  WriteString(Stream, Format('%p', [Self]));
end;

{ TNumber }

function TNumber.Eval: IDatum;
begin
  Result := INumber(Self)
end;

procedure TNumber.Write(Stream: TStream);
begin
  WriteString(Stream, Format('%g', [FValue]));
end;

end.
