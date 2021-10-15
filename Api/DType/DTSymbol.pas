unit DTSymbol;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DynTypes, DUtils, DTDatum;

type
  TDynSymbol = class(TDyn, IDynSymbol)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynSymbol
  public
    function FoldedCase: IDynSymbol; virtual;
    function Name: Utf8String; virtual;
  private
    function GetAsIDynSymbol: IDynSymbol;
  public
    property AsIDynSymbol: IDynSymbol read GetAsIDynSymbol {$if Declared(InlineVMT)} implements IDynSymbol {$ifend};
  protected//private
    Next: TDynSymbol;
    Key: Utf8String;
    FFoldedCase: IDynSymbol;
    function CreateFoldedCase: IDynSymbol;
  public
    constructor Create(pName: PAnsiChar; cbName: Integer);
    destructor Destroy; override;
    function DatumType: TDatumType; override;
  protected
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
    procedure DoMsgEval(var Msg: TEvalMessage); message MsgEval;

    procedure DoMsgIsSymbol(var Msg: TVarMessage); message MsgIsSymbol;
    procedure DoMsgIsSymbolR(var Msg: TVarMessage); message MsgIsSymbolR;
  end;

type
  ISymbolSource = interface
    function CreateSymbol(pName: PAnsiChar; cbName: Integer): IDynSymbol;
  end;
  PPHashItem = ^TDynSymbol;

  TSymbolHash = class(TInterfacedObject, ISymbolSource)
  private
    Buckets: array of TDynSymbol;
    Mask: Integer;
    function Add(pKey: PAnsiChar; cbKey: Integer): IDynSymbol;
    function HashOfP(pKey: PAnsiChar; cbKey: Integer): Cardinal; virtual;
    function FindP(pKey: PAnsiChar; cbKey: Integer): PPHashItem;
    procedure Clear;
    function CreateSymbol(pName: PAnsiChar; cbName: Integer): IDynSymbol;
  public
    constructor Create(Size: Integer = 256);
    destructor Destroy; override;
  end;

var
  SymbolSource: ISymbolSource;

function SymbolName(A: TDynDatum): Utf8String;

implementation /////////////////////////////////////////////////////////////////

function SymbolName(A: TDynDatum): Utf8String;
var
  Ref: IDynSymbol;
begin
  NeedSymbol(A, Ref);
  Result := Ref.Name
end;

function SameSymbolsName(A: RawByteString; pB: PAnsiChar; cbB: Integer): Boolean; overload;
var
  pA: PAnsiChar;
begin
  Result := False;
  if Length(A) = cbB then
  begin
    pA := Pointer(A);
    if CompareMem(pA, pB, cbB) then
      Result := True;
  end;
end;

procedure CreateSymbolSource(HashSize: Integer = 256);
begin
  SymbolSource := TSymbolHash.Create(HashSize);
end;

{ TDynSymbol }

function TDynSymbol.FoldedCase: IDynSymbol;
begin
  if Assigned(FFoldedCase) then
    Result := FFoldedCase
  else
    Result := CreateFoldedCase
end;

function TDynSymbol.Name: Utf8String;
begin
  Result := Key;
end;

function TDynSymbol.GetAsIDynSymbol: IDynSymbol;
begin
  {$if Declared(InlineVMT)} Result := IDynSymbol(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynSymbol.CreateFoldedCase: IDynSymbol;
var
  s, sKey: String;
begin
  sKey := string(Key);
  s := LowerCase(sKey);
  if s = sKey then
    Result := AsIDynSymbol
  else
    Result := TDynSymbol.Create(Pointer(s), Length(s)).AsIDynSymbol;
  FFoldedCase := Result;
end;

constructor TDynSymbol.Create(pName: PAnsiChar; cbName: Integer);
begin
  SetString(Key, pName, cbName);
end;

destructor TDynSymbol.Destroy;
begin
  inherited Destroy;
end;

function TDynSymbol.DatumType: TDatumType;
begin
  Result := atSymbol
end;

procedure TDynSymbol.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
  s: UnicodeString;
begin
  s := UnicodeString(Key);
  r := Msg.Port.WriteStrW(Pointer(s), Length(s), skSymbol);
  Msg.Res := Ord(r);
end;

procedure TDynSymbol.DoMsgEval(var Msg: TEvalMessage);
begin
  Msg.Res := Msg.Scope.Item[IDynSymbol(Self)]
end;

procedure TDynSymbol.DoMsgIsSymbol(var Msg: TVarMessage);
begin
  Msg.Res := 1; //True;
end;

procedure TDynSymbol.DoMsgIsSymbolR(var Msg: TVarMessage);
type
  PIDynSymbol = ^IDynSymbol;
begin
  Msg.Res := 1; //True;
  PIDynSymbol(Msg.VarPtr)^ := IDynSymbol(Self);
end;

{ TSymbolHash }

function TSymbolHash.Add(pKey: PAnsiChar; cbKey: Integer): IDynSymbol;
var
  Hash: Integer;
  Obj: TDynSymbol;
begin
  Hash := HashOfP(pKey, cbKey) mod Cardinal(Length(Buckets));
  Obj := TDynSymbol.Create(pKey, cbKey);
  Result := Obj.AsIDynSymbol;
  Result._AddRef; // mantener una referencia interna
  Obj.Next := Buckets[Hash and Mask];
  Buckets[Hash and Mask] := Obj;
end;

function TSymbolHash.HashOfP(pKey: PAnsiChar; cbKey: Integer): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to cbKey - 1 do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(pKey[I]);
end;

function TSymbolHash.FindP(pKey: PAnsiChar; cbKey: Integer): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOfP(pKey, cbKey) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash and Mask];
  while Result^ <> nil do
  begin
    if SameSymbolsName(Result^.Key, pKey, cbKey) then
      Exit;
    Result := @Result^.Next;
  end;
end;

procedure TSymbolHash.Clear;
var
  I: Integer;
  P, N: TDynSymbol;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P.Next;
      P._Release;
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

function TSymbolHash.CreateSymbol(pName: PAnsiChar; cbName: Integer): IDynSymbol;
var
  Item: PPHashItem;
begin
  Item := FindP(pName, cbName);
  if Assigned(Item) then
    if Assigned(Item^) then
    begin
      Result := Item^;
      Exit;
    end;
  Result := Add(pName, cbName);
end;

constructor TSymbolHash.Create(Size: Integer = 256);
begin
  inherited Create;
  SetLength(Buckets, Size);
  Mask := Size - 1;
end;

destructor TSymbolHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

initialization /////////////////////////////////////////////////////////////////
{$ifdef DTYPES}
  CreateSymbolSource($10000);
{$endif}
end. ///////////////////////////////////////////////////////////////////////////

