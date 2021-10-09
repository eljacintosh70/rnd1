unit DTPortW;

interface

uses
  SysUtils,
  DynTypes, DTDatum, DUtils, DTPort;

type
  TStrTextOutW = class(TObject)
  private
    Buf: string;
    Ofs, Len: Integer;
    function GrowTo(NewLen: Integer): Boolean; virtual;
  public
    function WriteText(p: PWideChar; cc: Integer): Boolean; inline;
  public
    constructor Create(ALen: Integer);
    function WriteProc(p: Pointer; cb: Integer): Boolean;
    procedure Reset;
    function GetText: string;
  end;

  TCustomDynOutPortW = class(TCustomDynOutPort)
  public
    function WriteText(p: PWideChar; cc: Integer): Boolean; overload; inline;
    function WriteText(const s: String): Boolean; overload; inline;
  end;

  TDynOutPort = class(TCustomDynOutPortW)
  public
    function Write(Obj: dyn): Boolean; override; stdcall;
    function WriteSepar: Boolean; override; stdcall;
    function WriteLn: Boolean; override; stdcall;
    function WriteSpecial(v: TSpecialValue): Boolean; override; stdcall;
    function WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean; override; stdcall;
    function WriteFloat(v: Single): Boolean; overload; override; stdcall;
    function WriteFloat(v: Double): Boolean; overload; override; stdcall;
    function WriteStrA(p: PAnsiChar; cc: Integer; Kind: TStrKind = skFull; CodePage: Word = 0): Boolean; override; stdcall;
    function WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; override; stdcall;
    function WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; override; stdcall;
    function WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; override; stdcall;
    function BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
  end;

  TDynOutPortWr = class(TDynOutPort)
  public
    function Write(Obj: dyn): Boolean; override; stdcall;
    function WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; override; stdcall;
    function WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; override; stdcall;
    function WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; override; stdcall;
    function BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
  protected
    function WriteChar(ch: WideChar): Boolean; virtual;
    function EscapeStr(p: PWideChar; cc: Integer): Boolean; virtual;
  end;

  TDynOutPortIndent = class(TInterfacedObject, IDynOutPort)
    function IDynOutPort._AddRef = IDynOutPort__AddRef;
    function IDynOutPort._Release = IDynOutPort__Release;
    function IDynOutPort.Write = Write;
    function IDynOutPort.WriteSepar = IDynOutPort_WriteSepar;
    function IDynOutPort.BeginList = IDynOutPort_BeginList;
  protected
    FPort: TCustomDynOutPortW;
    Open: string;
    Separ: string;
    Close: string;
  protected
    // sobreescribir los que no deban llamarse desde el objeto Port
    function IDynOutPort__AddRef: Integer; stdcall;
    function IDynOutPort__Release: Integer; stdcall;
    function Write(Obj: dyn): Boolean; stdcall;
    function IDynOutPort_WriteSepar: Boolean; stdcall;
    function IDynOutPort_BeginList(Kind: TListKind): IDynOutPort; virtual; stdcall;
  public
    constructor Create(APort: TCustomDynOutPortW; AOpen, ASepar, AClose: string);
    property Port: TCustomDynOutPortW read FPort implements IDynOutPort;
  end;

implementation


{ TStrTextOutW }

function TStrTextOutW.GrowTo(NewLen: Integer): Boolean;
begin
  NewLen := (NewLen + 15) and not 15;
  SetLength(Buf, NewLen);
  Len := NewLen;
  Result := True;
end;

function TStrTextOutW.WriteProc(p: Pointer; cb: Integer): Boolean;
begin
  Result := WriteText(p, cb div SizeOf(WideChar));
end;

function TStrTextOutW.WriteText(p: PWideChar; cc: Integer): Boolean;
var
  NewOfs, i0: Integer;
begin
  NewOfs := Ofs + cc;
  if NewOfs > Len then
  begin
    Result := GrowTo(NewOfs);
    if not Result then
      Exit;
  end;
  i0 := Ofs + 1;
  Move(p^, Buf[i0], cc * SizeOf(WideChar));
  Ofs := NewOfs;
  Result := True;
end;

constructor TStrTextOutW.Create(ALen: Integer);
begin
  GrowTo(ALen);
end;

procedure TStrTextOutW.Reset;
begin
  Ofs := 0;
end;

function TStrTextOutW.GetText: string;
begin
  Result := Copy(Buf, 1, Ofs);
end;


{ TCustomDynOutPortW }

function TCustomDynOutPortW.WriteText(p: PWideChar; cc: Integer): Boolean;
begin
  Result := WriteProc(p, cc * SizeOf(WideChar))
end;

function TCustomDynOutPortW.WriteText(const s: String): Boolean;
begin
  Result := WriteProc(Pointer(s), Length(s) * SizeOf(WideChar))
end;

{ TDynOutPort }

function TDynOutPort.Write(Obj: dyn): Boolean;
var
  Msg: TWriteMsg;
begin
  if Obj <> nil then
  begin
    Msg.Msg := MsgWrite;
    Msg.Res := 0;
    Msg.Port := Self;
    Obj.DispatchMsg(Msg);
    Result := Msg.Res <> 0;
  end
  else
    Result := WriteSpecial(svNil);
end;

function TDynOutPort.WriteSepar: Boolean;
begin
  Result := True;
end;

function TDynOutPort.WriteLn: Boolean;
begin
  Result := WriteText(#13#10)
end;

function TDynOutPort.WriteSpecial(v: TSpecialValue): Boolean;
var
  s: String;
begin
  case v of
    svFalse: s := '#f';
    svTrue:  s := '#t';
    svNil:   s := '()';
    else  Result := False;
          Exit;
  end;
  Result := WriteText(s)
end;

function TDynOutPort.WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean;
var
  v: Int64;
  s: String;
begin
  case Sign of
    isTwoComp:
      case cb of
        1: v := PShortInt(p)^;
        2: v := PSmallInt(p)^;
        4: v := PInteger(p)^;
        8: v := PInt64(p)^;
      else DynError('Unsupported int size', [cb]);  v := 0;
      end;
    else // isPositive, isNegative:
      case cb of
        1: v := PByte(p)^;
        2: v := PWord(p)^;
        4: v := PCardinal(p)^;
        8: v := PUInt64(p)^;
      else DynError('Unsupported int size', [cb]);  v := 0;
      end;
      if Sign = isNegative then
        v := - v;
  end;
  s := IntToStr(v);
  Result := WriteText(s)
end;

function TDynOutPort.WriteFloat(v: Single): Boolean;
var
  s: String;
begin
  s := FloatToStr(v);
  Result := WriteText(s)
end;

function TDynOutPort.WriteFloat(v: Double): Boolean;
var
  s: String;
begin
  s := FloatToStr(v);
  Result := WriteText(s)
end;

function TDynOutPort.WriteStrA(p: PAnsiChar; cc: Integer;
  Kind: TStrKind = skFull; CodePage: Word = 0): Boolean;
var
  a: RawByteString;
  w: UnicodeString;
begin
  SetString(a, p, cc);
  SetCodePage(a, CodePage, False);
  w := UnicodeString(a);
  Result := WriteStrW(Pointer(w), Length(w), Kind)
end;

function TDynOutPort.WriteStrW(p: PWideChar; cc: Integer;
  Kind: TStrKind): Boolean;
begin
  Result := WriteText(p, cc)
end;

function TDynOutPort.WriteMem(p: PAnsiChar; cb: Integer;
  Kind: TMemKind): Boolean;
begin
  Result := WriteStrA(p, cb, skRaw)
end;

function TDynOutPort.WriteList(const val: array of dyn;
  Kind: TListKind): Boolean;
var
  obj: dyn;
begin
  Result := True;
  for obj in val do
  begin
    Result := Write(Obj);
    if not Result then
      Exit;
  end;
end;

function TDynOutPort.BeginList(Kind: TListKind): IDynOutPort;
begin
  Result := Self; // se escribira todo lo que está en la lista sin separadores
end;

{ TDynOutPortWr }

function TDynOutPortWr.Write(Obj: dyn): Boolean;
var
  Msg: TWriteMsg;
begin
  if Obj <> nil then
  begin
    Msg.Msg := MsgDisplay;
    Msg.Res := 0;
    Msg.Port := Self;
    Obj.DispatchMsg(Msg);
    Result := Msg.Res <> 0;
  end
  else
    Result := WriteSpecial(svNil);
end;

function TDynOutPortWr.WriteStrW(p: PWideChar; cc: Integer;
  Kind: TStrKind): Boolean;
begin
  case Kind of
    skRaw, skSymbol:
      begin
        Result := WriteText(p, cc);
        Exit;
      end;
    skChar:
      begin
        Result := WriteChar(p^);
        Exit;
      end;
    skFull, skStart:
      WriteText('"');
  end;
  Result := EscapeStr(p, cc);
  case Kind of
    skFull, skEnd:
      Result := WriteText('"');
  end;
end;

function TDynOutPortWr.WriteMem(p: PAnsiChar; cb: Integer;
  Kind: TMemKind): Boolean;
const
  HexDigit: array[0..$F] of WideChar = '0123456789ABCDEF';
var
  HexStr: array[0..2] of WideChar;
  b: Byte;
  i: Integer;
begin
  Result := True;

  case Kind of
    mkFull, mkStart:
      Result := WriteText('#m(');
  end;

  if cb > 0 then
  begin
    b := Byte(p[0]);
    HexStr[0] := HexDigit[b shr 4];
    HexStr[1] := HexDigit[b and $F];
    Result := WriteText(HexStr, 2);
    if not Result then
      Exit;

    HexStr[0] := ' ';
    for i := 1 to cb - 1 do
    begin
      b := Byte(p[i]);
      HexStr[1] := HexDigit[b shr 4];
      HexStr[2] := HexDigit[b and $F];
      Result := WriteText(HexStr, 3);
      if not Result then
        Exit;
    end;
  end;

  case Kind of
    mkFull, mkEnd:
      Result := WriteText(')');
  end;
end;

function TDynOutPortWr.WriteList(const val: array of dyn;
  Kind: TListKind): Boolean;
var
  Port: IDynOutPort;
  i, n: integer;
begin
  Result := True;
  Port := BeginList(Kind);
  n := Length(val);
  if n > 0 then
  begin
    Port.Write(val[0]);
    for i := 1 to n - 1 do
    begin
      Port.WriteSepar;
      Result := Port.Write(val[i]);
      if not Result then
        exit;
    end;
  end;
end;

function TDynOutPortWr.BeginList(Kind: TListKind): IDynOutPort;
begin
  Result := TDynOutPortIndent.Create(Self, '', ' ', '')
end;

function TDynOutPortWr.WriteChar(ch: WideChar): Boolean;
begin
  Result := WriteText(@ch, 1)
end;

function TDynOutPortWr.EscapeStr(p: PWideChar; cc: Integer): Boolean;
begin
  Result := WriteText(p, cc);
end;

{ TDynOutPortIndent }

function TDynOutPortIndent.IDynOutPort__AddRef: Integer;
begin
  Result := inherited _AddRef
end;

function TDynOutPortIndent.IDynOutPort__Release: Integer;
begin
  if RefCount = 1 then
    Port.WriteText(Close);
  Result := inherited _Release
end;

function TDynOutPortIndent.Write(Obj: dyn): Boolean;
var
  Msg: TWriteMsg;
begin
  if Obj <> nil then
  begin
    Msg.Msg := MsgWrite;
    Msg.Res := 0;
    Msg.Port := Self;       // es importante que este self sea el TDynOutPortIndent
    Obj.DispatchMsg(Msg);
    Result := Msg.Res <> 0;
  end
  else
    Result := Port.WriteSpecial(svNil);
end;

function TDynOutPortIndent.IDynOutPort_WriteSepar: Boolean;
begin
  Result := Port.WriteText(Separ);
end;

function TDynOutPortIndent.IDynOutPort_BeginList(Kind: TListKind): IDynOutPort;
begin
  Result := TDynOutPortIndent.Create(Port, Open, Separ, Close)
end;

constructor TDynOutPortIndent.Create(APort: TCustomDynOutPortW; AOpen, ASepar, AClose: string);
begin
  FPort := APort;
  Open := AOpen;
  Separ := ASepar;
  Close := AClose;
  Port.WriteText(Open);
end;

end.

