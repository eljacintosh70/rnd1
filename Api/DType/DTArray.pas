unit DTArray;
interface

uses
  Windows, SysUtils,
  DynTypes, DTDatum, DUtils;

////////////////////////////////////////////////////////////////////////////////
//                                                          manejo de interfaces
////////////////////////////////////////////////////////////////////////////////

type
  TAbstractDynArray = class(TDyn, IDynArray)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynArray
  public
    //~ function HasData: Boolean; override;
    function Length: TArraySize; virtual;
    function GetItemA(i: Integer): TDynDatum; virtual; abstract;
    procedure SetItemA(i: Integer; const Value: TDynDatum); virtual; abstract;
    property ItemA[i: Integer]: TDynDatum read GetItemA write SetItemA;
    procedure Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Size: TLockSize =
        UpToEnd; Writeable: Boolean = False); virtual;
    function DataPtr: Pointer; virtual;
  private
    function GetAsIDynArray: IDynArray;
  public
    property AsIDynArray: IDynArray read GetAsIDynArray {$if Declared(InlineVMT)} implements IDynArray {$ifend};
  protected
    FLength: Integer;
    DataLabel: record end;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TCustomDynArray = class(TAbstractDynArray)
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
  end;

  TDynArray = class(TCustomDynArray)
  protected
    FItems: array[0..0] of TDatumRef;
    procedure DisposeInstance;
  public
    class function Create(n: Integer): TDynArray; overload;
    class function Create(const Arr: array of const): TDynArray; overload;
    class function Create(n: Integer; Fill: TDynDatum): TDynArray; overload;
    function _Release: Integer; override; stdcall;
    function GetItemA(i: Integer): TDynDatum; override;
    procedure SetItemA(i: Integer; const First: TDynDatum); override;
    // deben seguir el mismo orden que en ISchVector
    procedure Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Size: TLockSize =
        UpToEnd; Writeable: Boolean = False); override;
    procedure DoMsgEval(var Msg: TEvalMessage); message MsgEval;
  end;

  TAbstractDynMemory = class(TAbstractDynArray, IDynMemory)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynMemory
  public
    function SubBlock(Ofs: TArrayPos; Size: TArraySize): IDynMemory; virtual;
    function GetBytes(i: Integer): Byte; virtual;
    procedure SetBytes(i: Integer; const First: Byte); virtual;
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean; virtual;
    property Bytes[i: Integer]: Byte read GetBytes write SetBytes;
  private
    function GetAsIDynMemory: IDynMemory;
  public
    property AsIDynMemory: IDynMemory read GetAsIDynMemory {$if Declared(InlineVMT)} implements IDynMemory {$ifend};
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function GetItemA(i: Integer): TDynDatum; override;
    procedure SetItemA(i: Integer; const First: TDynDatum); override;
  end;

  TDynMemory = class(TAbstractDynMemory)
  protected
    FItems: array[0..0] of Byte;
  public
    class function Create(n: Integer): TDynMemory; overload;
    class function Create(pData: Pointer;
      cbData: Integer): TDynMemory; overload;
    function _Release: Integer; override; stdcall;
    function GetBytes(i: Integer): Byte; override;
    procedure SetBytes(i: Integer; const Value: Byte); override;
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean; override;
  protected
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TRefDynMemory = class(TAbstractDynMemory)
  protected
    FDataPtr: Pointer;
    Ref: IDynDatum;
  public
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean; override;
    procedure Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Size: TLockSize =
        UpToEnd; Writeable: Boolean = False); override;
    function DataPtr: Pointer; override;
    function GetBytes(i: Integer): Byte; override;
    procedure SetBytes(i: Integer; const Value: Byte); override;
  public
    constructor Create(ARef: IDynDatum; Ptr: Pointer; n: Integer);
  protected
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

implementation

{ TODO : Revisar todos los accesos a FItems[i],
       los índices deben compararse con FLength, no con lo declarado }
{$R-}

var
  DebugStr: String;

{ TAbstractDynArray }

function TAbstractDynArray.Length: TArraySize;
begin
  Result := FLength
end;

procedure TAbstractDynArray.Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Size:
    TLockSize = UpToEnd; Writeable: Boolean = False);
var
  MaxSize: TArraySize;
  p: PRaw;
begin
  if Ofs < 0 then
    Ofs := 0;
  MaxSize := FLength - Ofs;
  if Size > MaxSize then
    Size := MaxSize;
  if Size > 0 then
  begin
    p := Pointer(@DataLabel);
    Inc(p, Ofs);
    Block.Size := Size;
    Block.Ptr := p;
    Block.Lock := AsIDynArray;
  end
  else
  begin
    Block.Size := 0;
    Block.Ptr := nil;
    Block.Lock := nil;
  end
end;

function TAbstractDynArray.DataPtr: Pointer;
begin
  Result := @DataLabel;
end;

function TAbstractDynArray.GetAsIDynArray: IDynArray;
begin
  {$if Declared(InlineVMT)} Result := IDynArray(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

procedure TAbstractDynArray.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
  List: IDynOutPort;
  i, n: Integer;
begin
  List := Msg.Port.BeginList(lkVector);
  n := Length;
  r := True;
  if n > 0 then
  begin
    r := List.Write(ItemA[0]);
    if r then
      for i := 1 to n - 1 do
      begin
        r := List.WriteSepar;
        r := r and List.Write(ItemA[i]);
        if not r then
          Break;
      end;
  end;
  Msg.Res := Ord(r);
end;

{ TCustomDynArray }

function TCustomDynArray.DatumType: TDatumType;
begin
  Result := atVector
end;

function TCustomDynArray.DisplayStr(NeededChars: Integer): String;
var
  s: string;
  Datum: TDynDatum;
  i: Integer;
begin
  Result := '#(';
  Dec(NeededChars, System.Length(Result));
  if NeededChars <= 0 then
    Exit;
  for i := 0 to length - 1 do
  begin
    Datum := ItemA[i];
    s := Datum.DisplayStr(NeededChars);
    Result := Result + s + ' ';
    Dec(NeededChars, System.Length(s) + 1);
    if NeededChars <= 0 then
      Exit;
  end;
  Result := Result + ')';
end;

{ TDynArray }

procedure TDynArray.DisposeInstance;
var
  i, n: Integer;
begin
  DebugStr := Format('%s(%p)', [ClassName, Pointer(Self)]);
  n := FLength;
  for i := 0 to n - 1 do
    FItems[i] := nil;
  //Destroy;
  Dispose(Pointer(Self));
end;

procedure TDynArray.DoMsgEval(var Msg: TEvalMessage);
var
  i, n: Integer;
  v: IDynArray;
begin
  n := Length;
  v := make_vector(n);
  Msg.Res := v;
  for i := 0 to n - 1 do
    v[i] := Eval(ItemA[i], Msg.Scope)
end;

class function TDynArray.Create(n: Integer): TDynArray;
begin
  GetMem(Pointer(Result), InstanceSize + (n - 1) * SizeOf(TDatumRef));
  InitInstance(Result);
  Result.FLength := n;
  FillChar(Result.FItems, n * SizeOf(TDatumRef), #0);
end;

class function TDynArray.Create(const Arr: array of const): TDynArray;
var
  i, n: Integer;
begin
  n := System.Length(Arr);
  Result := TDynArray.Create(n);
  for i := 0 to n - 1 do
    Result.FItems[i] := ConstToDatum(Arr[i]);
end;

class function TDynArray.Create(n: Integer; Fill: TDynDatum): TDynArray;
var
  i: Integer;
begin
  Result := TDynArray.Create(n);
  for i := 0 to n - 1 do
    Result.FItems[i] := Fill;
end;

function TDynArray._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    DisposeInstance;
end;

function TDynArray.GetItemA(i: Integer): TDynDatum;
begin
  Result := FItems[i]
end;

procedure TDynArray.SetItemA(i: Integer; const First: TDynDatum);
begin
  FItems[i] := First
end;

procedure TDynArray.Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Size:
    TLockSize = UpToEnd; Writeable: Boolean = False);
var
  MaxSize: TArraySize;
  p: ^IDynDatum;
begin
  if Ofs < 0 then
    Ofs := 0;
  MaxSize := FLength - Ofs;
  if Size > MaxSize then
    Size := MaxSize;
  if Size > 0 then
  begin
    p := Pointer(@DataLabel);
    Inc(p, Ofs);
    Block.Size := Size;
    Block.Ptr := p;
    Block.Lock := IDynArray(Pointer(Self));
  end
  else
  begin
    Block.Size := 0;
    Block.Ptr := nil;
    Block.Lock := nil;
  end
end;

{ TAbstractDynMemory }

function TAbstractDynMemory.SubBlock(Ofs: TArrayPos;
  Size: TArraySize): IDynMemory;
var
  Block: TArrayBlock;
  Obj: TRefDynMemory;
begin
  Lock(Block, Ofs, Size, False);
  Obj := TRefDynMemory.Create(Block.Lock, Block.Ptr, Size);
  Result := Obj;
end;

function TAbstractDynMemory.GetBytes(i: Integer): Byte;
var
  Block: TArrayBlock;
begin
  Lock(Block, i, 1, False);
  if Block.Size > 0 then
    Result := PByte(Block.Ptr)^
  else
    Result := 0;
end;

procedure TAbstractDynMemory.SetBytes(i: Integer; const First: Byte);
var
  Block: TArrayBlock;
begin
  Lock(Block, i, 1, True);
  if Block.Size > 0 then
    PByte(Block.Ptr)^ := First
end;

function TAbstractDynMemory.FindNext(const Data: RawData; var Pos:
    TArrayPos; MaxPos: TArrayPos = FullBlock): Boolean;
begin
  Result := False;
end;

function TAbstractDynMemory.GetAsIDynMemory: IDynMemory;
begin
  {$if Declared(InlineVMT)} Result := IDynMemory(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TAbstractDynMemory.DatumType: TDatumType;
begin
  Result := atByteVector
end;

function TAbstractDynMemory.DisplayStr(NeededChars: Integer): String;
var
  s: string;
  b: Byte;
  i: Integer;
  n, MaxN: Integer;
begin
  Result := '#m(';
  Dec(NeededChars, System.Length(Result));
  n := length;
  MaxN := (NeededChars + 2) div 3;
  if n > MaxN then
    n := MaxN;
  for i := 0 to n - 1 do
  begin
    b := Bytes[i];
    s := IntToHex(b, 2);
    Result := Result + s + ' ';
  end;
  Result := Result + ')';
end;

function TAbstractDynMemory.GetItemA(i: Integer): TDynDatum;
var
  b: dyn;
begin
  b := MakeInt64(Bytes[i]);
  Result := b;
end;

procedure TAbstractDynMemory.SetItemA(i: Integer;
  const First: TDynDatum);
begin
  Bytes[i] := First.AsInteger;
end;

{ TDynMemory }

class function TDynMemory.Create(n: Integer): TDynMemory;
begin
  GetMem(Pointer(Result), InstanceSize + (n - 1) * SizeOf(Byte));
  InitInstance(Result);
  Result.FLength := n;
end;

class function TDynMemory.Create(pData: Pointer; cbData: Integer): TDynMemory;
begin
  Result := Create(cbData);
  Move(pData^, Result.FItems[0], cbData);
end;

function TDynMemory._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    DebugStr := Format('%s(%p)', [ClassName, Pointer(Self)]);
    //Destroy;
    Dispose(Pointer(Self));
  end;
end;

function TDynMemory.GetBytes(i: Integer): Byte;
begin
  Result := FItems[i]
end;

procedure TDynMemory.SetBytes(i: Integer; const Value: Byte);
begin
  FItems[i] := Value
end;

function TDynMemory.FindNext(const Data: RawData; var Pos: TArrayPos;
  MaxPos: TArrayPos): Boolean;
var
  p, pe: PRaw;
  pData: PRaw;
  i: Integer;
  ch0: Raw;
  CurPos: PRaw;
  cbData: Integer;
  Size: Integer;
begin
  cbData := System.Length(Data);
  Result := False;
  if (cbData = 0) then Exit;

  Size := Length;
  if (MaxPos > Size - cbData) then
    MaxPos := Size - cbData;
  pData := Pointer(Data);

  CurPos := @FItems[0];

  p := CurPos + Pos;
  pe := CurPos + MaxPos;
  ch0 := pData^;
  while (p < pe) do
  begin
    if (p^ <> ch0) then
      Inc(p)
    else
    begin
      Inc(p);
      Result := True;
      for i := 0 to cbData - 2 do
      begin
        if pData[i + 1] <> p[i] then
        begin
          Result := False;
          Break {for i};
        end;
      end;
      if Result then
      begin
        Pos := p - 1 - CurPos;
        exit;
      end;
    end
  end;
end;

procedure TDynMemory.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteMem(@FItems, FLength);
  Msg.Res := Ord(r);
end;

{ TRefDynMemory }

function TRefDynMemory.FindNext(const Data: RawData;
  var Pos: TArrayPos; MaxPos: TArrayPos): Boolean;
var
  p, pe: PRaw;
  pData: PRaw;
  cbData: Integer;
  i: Integer;
  ch0: Raw;
begin
  cbData := System.Length(Data);
  Result := False;
  if (cbData = 0) then Exit;
  if (MaxPos > FLength - cbData) then
    MaxPos := FLength - cbData;
  pData := Pointer(Data);
  p := PRaw(FDataPtr) + Pos;
  pe := PRaw(FDataPtr) + MaxPos;
  ch0 := pData^;
  while (p < pe) do
  begin
    if (p^ <> ch0) then
      Inc(p)
    else
    begin
      Inc(p);
      Result := True;
      for i := 0 to cbData - 2 do
      begin
        if pData[i + 1] <> p[i] then
        begin
          Result := False;
          Break {for i};
        end;
      end;
      if Result then
      begin
        Pos := p - 1 - PRaw(FDataPtr);
        exit;
      end;
    end
  end;
end;

procedure TRefDynMemory.Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0;
    Size: TLockSize = UpToEnd; Writeable: Boolean = False);
var
  MaxSize: TArraySize;
  p: PRaw;
begin
  if Ofs < 0 then
    Ofs := 0;
  MaxSize := FLength - Ofs;
  if Size > MaxSize then
    Size := MaxSize;
  if Size > 0 then
  begin
    p := FDataPtr;
    Inc(p, Ofs);
    Block.Size := Size;
    Block.Ptr := p;
    Block.Lock := IDynArray(Pointer(Self));
  end
  else
  begin
    Block.Size := 0;
    Block.Ptr := nil;
    Block.Lock := nil;
  end
end;

function TRefDynMemory.DataPtr: Pointer;
begin
  Result := FDataPtr;
end;

function TRefDynMemory.GetBytes(i: Integer): Byte;
begin
  Result := Byte(PRaw(FDataPtr)[i])
end;

procedure TRefDynMemory.SetBytes(i: Integer; const Value: Byte);
begin
  Byte(PRaw(FDataPtr)[i]) := Value
end;

constructor TRefDynMemory.Create(ARef: IDynDatum; Ptr: Pointer;
  n: Integer);
begin
  FLength := n;
  FDataPtr := Ptr;
  Ref := ARef;
end;

procedure TRefDynMemory.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteMem(FDataPtr, FLength);
  Msg.Res := Ord(r);
end;

end.
