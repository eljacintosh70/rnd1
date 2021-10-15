unit DTString;

interface

uses
  SysUtils,
  {$IFNDEF LINUX} Windows, {$ENDIF}
  DynTypes, DUtils, DTDatum, DTArray;

type
  TDynChar = class(TDyn)
  public
    FValue: Char;
    constructor Create(AValue: Char);
    function DatumType: TDatumType; override;
    function AsVariant: Variant; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function WriteStr(NeededChars: Integer): String; override;
  protected
    procedure CastToChar(var Msg: TVarMessage); message MsgCastToChar;
    procedure CastToString(var Msg: TVarMessage); message MsgCastToString;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TAbstractDynString = class(TAbstractDynArray, IDynString)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynString
  public
    function GetChars(i: Integer): Char; virtual; abstract;
    procedure SetChars(i: Integer; const First: Char); virtual; abstract;
    property Chars[i: Integer]: Char read GetChars write SetChars;
    function AsString: String; virtual; abstract;
  protected
    function GetAsIDynString: IDynString;
  public
    property AsIDynString: IDynString read GetAsIDynString {$if Declared(InlineVMT)} implements IDynString {$ifend};
  end;

  TDynString = class(TAbstractDynString)
  public
    function GetChars(i: Integer): Char; override;
    procedure SetChars(i: Integer; const First: Char); override;
    function AsString: String; override;
  public
    property AsISchString: IDynString read GetAsIDynString;
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
  protected
    FItems: array[0..0] of WideChar;
  public
    class function Create(pData: PWideChar; cbData: Integer): TDynString;
    class function CreateI(pData: PWideChar; cbData: Integer): IDynString;
    function GetItemA(i: Integer): dyn; override;
    procedure SetItemA(i: Integer; const Value: dyn); override;
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
  protected
    procedure CastToString(var Msg: TVarMessage); message MsgCastToString;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TDynStringA = class(TAbstractDynString)
  public
    function GetChars(i: Integer): Char; override;
    procedure SetChars(i: Integer; const First: Char); override;
    function AsString: String; override;
  public
    property AsISchString: IDynString read GetAsIDynString;
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
  protected
    FItems: array[0..0] of AnsiChar;
  public
    class function Create(pData: PAnsiChar; cbData: Integer): TDynStringA;
    class function CreateI(pData: PAnsiChar; cbData: Integer): IDynString;
    function GetItemA(i: Integer): dyn; override;
    procedure SetItemA(i: Integer; const Value: dyn); override;
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
  protected
    procedure CastToString(var Msg: TVarMessage); message MsgCastToString;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

implementation

{ TODO : Revisar todos los accesos a FItems[i],
       los índices deben compararse con FLength, no con lo declarado }
{$R-}

var
  DebugStr: String;

{ TDynChar }

constructor TDynChar.Create(AValue: Char);
begin
  FValue := AValue;
end;

function TDynChar.DatumType: TDatumType;
begin
  Result := atChar
end;

function TDynChar.AsVariant: Variant;
begin
  Result := FValue
end;

function TDynChar.DisplayStr(NeededChars: Integer): String;
begin
  Result := FValue
end;

function TDynChar.WriteStr(NeededChars: Integer): String;
begin
  Result := FValue
end;

procedure TDynChar.CastToChar(var Msg: TVarMessage);
begin
  PChar(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynChar.CastToString(var Msg: TVarMessage);
begin
  PString(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynChar.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteStrW(@FValue, 1, skChar);
  Msg.Res := Ord(r);
end;

{ TAbstractDynString }

function TAbstractDynString.GetAsIDynString: IDynString;
begin
  {$if Declared(InlineVMT)} Result := IDynString(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

{ TDynString }

function TDynString.GetChars(i: Integer): Char;
begin
  Result := FItems[i]
end;

procedure TDynString.SetChars(i: Integer; const First: Char);
begin
  FItems[i] := First
end;

function TDynString.AsString: String;
var
  n: Integer;
begin
  n := FLength;
  SetLength(Result, n);
  Move(FItems[0], Pointer(Result)^, n * SizeOf(FItems[0]))
end;

function TDynString.DatumType: TDatumType;
begin
  Result := atString
end;

function TDynString.DisplayStr(NeededChars: Integer): String;
var
  n: Integer;
begin
  n := FLength;
  if n <= NeededChars - 2 then
    Result := '"' + AsString + '"'
  else
  begin
    Result := '"' + Copy(AsString, 1, NeededChars - 1);
    if n < NeededChars - 1 then
      Result := Result + '"';
  end;
end;

class function TDynString.Create(pData: PWideChar; cbData: Integer): TDynString;
begin
  GetMem(Pointer(Result), TDynString.InstanceSize
    + cbData * SizeOf(Result.FItems[0])); // permitir #0
  InitInstance(Result);
  Move(pData^, Result.FItems[0], cbData * SizeOf(Result.FItems[0]));
  Result.FItems[cbData] := #0;
  Result.FLength := cbData;
end;

class function TDynString.CreateI(pData: PWideChar; cbData: Integer): IDynString;
var
  Obj: TDynString;
begin
  Obj := Create(pData, cbData);
  Result := IDynString(Obj)
end;

function TDynString.GetItemA(i: Integer): dyn;
var
  ch: WideChar;
  r: dyn;
begin
  ch := WideChar(Chars[i]);
  r := MakeChar(ch);
  Result := r;
end;

procedure TDynString.SetItemA(i: Integer; const Value: dyn);
begin
  { TODO : implementar }
end;

function TDynString._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    DebugStr := Format('%s(%p)', [ClassName, Pointer(Self)]);
    //Destroy;
    Dispose(Pointer(Self));
  end;
end;

procedure TDynString.CastToString(var Msg: TVarMessage);
begin
  PString(Msg.VarPtr)^ := AsString;
  Msg.Res := ResOK;
end;

procedure TDynString.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteStrW(FItems, FLength);
  Msg.Res := Ord(r);
end;


{ TDynStringA }

function TDynStringA.GetChars(i: Integer): Char;
begin
  Result := Char(FItems[i])
end;

procedure TDynStringA.SetChars(i: Integer; const First: Char);
begin
  FItems[i] := AnsiChar(First)
end;

function TDynStringA.AsString: String;
var
  n: Integer;
  Str: AnsiString;
begin
  n := FLength;
  SetString(Str, PAnsiChar(@FItems[0]), n);
  Result := string(Str);
end;

function TDynStringA.DatumType: TDatumType;
begin
  Result := atString
end;

function TDynStringA.DisplayStr(NeededChars: Integer): String;
var
  n: Integer;
begin
  n := FLength;
  if n <= NeededChars - 2 then
    Result := '"' + AsString + '"'
  else
  begin
    Result := '"' + Copy(AsString, 1, NeededChars - 1);
    if n < NeededChars - 1 then
      Result := Result + '"';
  end;
end;

class function TDynStringA.Create(pData: PAnsiChar; cbData: Integer): TDynStringA;
begin
  GetMem(Pointer(Result), TDynStringA.InstanceSize
    + cbData * SizeOf(Result.FItems[0])); // permitir #0
  InitInstance(Result);
  Move(pData^, Result.FItems[0], cbData * SizeOf(Result.FItems[0]));
  Result.FItems[cbData] := #0;
  Result.FLength := cbData;
end;

class function TDynStringA.CreateI(pData: PAnsiChar; cbData: Integer): IDynString;
var
  Obj: TDynStringA;
begin
  Obj := Create(pData, cbData);
  Result := IDynString(Obj)
end;

function TDynStringA.GetItemA(i: Integer): dyn;
var
  ch: WideChar;
  r: dyn;
begin
  ch := WideChar(Chars[i]);
  r := MakeChar(ch);
  Result := r;
end;

procedure TDynStringA.SetItemA(i: Integer; const Value: dyn);
begin
  { TODO : implementar }
end;

function TDynStringA._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    DebugStr := Format('%s(%p)', [ClassName, Pointer(Self)]);
    //Destroy;
    Dispose(Pointer(Self));
  end;
end;

procedure TDynStringA.CastToString(var Msg: TVarMessage);
begin
  PString(Msg.VarPtr)^ := AsString;
  Msg.Res := ResOK;
end;

procedure TDynStringA.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteStrA(FItems, FLength);
  Msg.Res := Ord(r);
end;

end.
