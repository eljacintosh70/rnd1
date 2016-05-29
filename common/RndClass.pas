unit RndClass;

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

  TSymbol = class(TDatum, ISymbol)
  protected
    FName: String;
    FValue: IDatum;
    function GetValue: IDatum;
    procedure SetValue(AValue: IDatum);
  public
    constructor Create(AName: String);
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
    function Name: String;
    property Value: IDatum read GetValue write SetValue;
  end;

  TNode = class(TDatum, INode)
  protected
    FValue: IDatum;
    FNext: INode;
    function GetValue: IDatum;
    procedure SetValue(AValue: IDatum);
  public
    constructor Create(AValue: IDatum; ANext: INode);
    function Eval: IDatum; override;
    function EvalItems: INode;
    function Next: INode;
    procedure Write(Stream: TStream); override;
    procedure WriteValues(Stream: TStream);
    property Value: IDatum read GetValue write SetValue;
  end;

  TLambda = class(TDatum, ILambda)
  public
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
    function Apply(Arg: INode): IDatum; virtual; abstract;
  end;

  TNumber = class(TDatum, INumber)
  protected
    FValue: Double;
    function GetValue: Double;
    procedure SetValue(AValue: Double);
  public
    constructor Create(AValue: Double);
    function Eval: IDatum; override;
    procedure Write(Stream: TStream); override;
    property Value: Double read GetValue write SetValue;
  end;

implementation


{ TDatum }

procedure TDatum.WriteString(Stream: TStream; s: String);
begin
  Stream.Write(Pointer(s)^, Length(s) * SizeOf(Char));
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

function TSymbol.GetValue: IDatum;
begin
  Result := FValue
end;

function TSymbol.Name: String;
begin
  Result := FName
end;

procedure TSymbol.SetValue(AValue: IDatum);
begin
  FValue := AValue
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
  Lst: INode;
  Value2: IDatum;
  Lambda: ILambda;
begin
  Lst := EvalItems;
  Value2 := Lst.Value;
  if Supports(Value2, ILambda, Lambda) then
    Result := Lambda.Apply(Lst.Next);
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

function TNode.GetValue: IDatum;
begin
  Result := FValue
end;

function TNode.Next: INode;
begin
  Result := FNext
end;

procedure TNode.SetValue(AValue: IDatum);
begin
  FValue := AValue
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

function TLambda.Eval: IDatum;
begin
  Result := ILambda(Self)
end;

procedure TLambda.Write(Stream: TStream);
begin
  WriteString(Stream, Format('%p', [Self]));
end;

{ TNumber }

constructor TNumber.Create(AValue: Double);
begin
  FValue := AValue
end;

function TNumber.Eval: IDatum;
begin
  Result := INumber(Self)
end;

function TNumber.GetValue: Double;
begin
  Result := FValue
end;

procedure TNumber.SetValue(AValue: Double);
begin
  FValue := AValue
end;

procedure TNumber.Write(Stream: TStream);
begin
  WriteString(Stream, Format('%g', [FValue]));
end;

end.
