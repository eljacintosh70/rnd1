unit DTPortW;

interface

uses
  SysUtils,
  DynTypes, DTDatum, DUtils, DTPort;

type
  TStrTextOutW = class(TObject)
  private
    Buf: UnicodeString;
    Ofs, Len: Integer;
    function GrowTo(NewLen: Integer): Boolean; virtual;
  public
    function WriteText(p: PWideChar; cc: Integer): Boolean; inline;
  public
    constructor Create(ALen: Integer);
    function WriteProc(p: Pointer; cb: Integer): Boolean;
    procedure Reset;
    function GetText: UnicodeString;
  end;

  TCustomDynOutPortW = class(TCustomDynOutPort)
  public
    function WriteText(p: PWideChar; cc: Integer): Boolean; overload; inline;
    function WriteText(const s: UnicodeString): Boolean; overload; inline;
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
  protected
    FPort: TCustomDynOutPortW;
    Open: UnicodeString;
    Separ: UnicodeString;
    Close: UnicodeString;
  protected
    // sobreescribir los que no deban llamarse desde el objeto Port
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function Write(Obj: dyn): Boolean; stdcall;
    function WriteSepar: Boolean; stdcall;
    function BeginList(Kind: TListKind): IDynOutPort; virtual; stdcall;
  public
    constructor Create(APort: TCustomDynOutPortW; AOpen, ASepar, AClose: UnicodeString);
    property Port: TCustomDynOutPortW read FPort {$IFNDEF LINUX} implements IDynOutPort{$ENDIF};
  {$IFDEF LINUX}
  protected
    //IDyn = interface(IInterface)
    function Invoke(const Arg: array of dyn): dyn; stdcall;
    function GetEnumerator: IDynEnumerator; stdcall;
    function GetItem(const Key: dyn): dyn; stdcall;
    procedure SetItem(const Key: dyn; const Value: dyn); stdcall;
    function Method(Id: dyn; Arg: array of dyn): dyn; stdcall;
    procedure DispatchMsg(var Message); stdcall;
    //IDynDatum = interface(IDyn)
    function CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer;
    function DatumType: TDatumType;
    function AsVariant: Variant;
    function DisplayStr(NeededChars: Integer): string;
    function WriteStr(NeededChars: Integer): string;
    //IDynOutPort = interface(IDynDatum)
    //function Write(Obj: dyn): Boolean; stdcall;
    //function WriteSepar: Boolean; stdcall;
    function WriteLn: Boolean; stdcall;
    function WriteSpecial(v: TSpecialValue): Boolean; stdcall;
    function WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean; stdcall;
    function WriteFloat(v: Single): Boolean; overload; stdcall;
    function WriteFloat(v: Double): Boolean; overload; stdcall;
    function WriteStrA(p: PAnsiChar; cc: Integer; Kind: TStrKind = skFull; CodePage: Word = 0): Boolean; stdcall;
    function WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; stdcall;
    function WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; stdcall;
    function WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; stdcall;
    //function BeginList(Kind: TListKind): IDynOutPort; stdcall;
  {$ENDIF}
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

function TStrTextOutW.GetText: UnicodeString;
begin
  Result := Copy(Buf, 1, Ofs);
end;


{ TCustomDynOutPortW }

function TCustomDynOutPortW.WriteText(p: PWideChar; cc: Integer): Boolean;
begin
  Result := WriteProc(p, cc * SizeOf(WideChar))
end;

function TCustomDynOutPortW.WriteText(const s: UnicodeString): Boolean;
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
  s: UnicodeString;
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
  s: UnicodeString;
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
  s: UnicodeString;
begin
  s := FloatToStr(v);
  Result := WriteText(s)
end;

function TDynOutPort.WriteFloat(v: Double): Boolean;
var
  s: UnicodeString;
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

function TDynOutPortIndent._AddRef: longint;
begin
  Result := inherited _AddRef
end;

function TDynOutPortIndent._Release: longint;
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

function TDynOutPortIndent.WriteSepar: Boolean;
begin
  Result := Port.WriteText(Separ);
end;

function TDynOutPortIndent.BeginList(Kind: TListKind): IDynOutPort;
begin
  Result := TDynOutPortIndent.Create(Port, Open, Separ, Close)
end;

constructor TDynOutPortIndent.Create(APort: TCustomDynOutPortW; AOpen, ASepar, AClose: UnicodeString);
begin
  FPort := APort;
  Open := AOpen;
  Separ := ASepar;
  Close := AClose;
  Port.WriteText(Open);
end;

{$IFDEF LINUX}
//IDyn = interface(IInterface)
function TDynOutPortIndent.Invoke(const Arg: array of dyn): dyn; stdcall;                                                        begin Result := Port.Invoke(Arg); end;
function TDynOutPortIndent.GetEnumerator: IDynEnumerator; stdcall;                                                               begin Result := Port.GetEnumerator; end;
function TDynOutPortIndent.GetItem(const Key: dyn): dyn; stdcall;                                                                begin Result := Port.GetItem(Key); end;
procedure TDynOutPortIndent.SetItem(const Key: dyn; const Value: dyn); stdcall;                                                  begin           Port.SetItem(Key, Value); end;
function TDynOutPortIndent.Method(Id: dyn; Arg: array of dyn): dyn; stdcall;                                                     begin Result := Port.Method(Id, Arg); end;
procedure TDynOutPortIndent.DispatchMsg(var Message); stdcall;                                                                   begin           Port.DispatchMsg(Message); end;
//IDynDatum = interface(IDyn)
function TDynOutPortIndent.CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer;                            begin Result := Port.CommandExec(Command, Res, Data); end;
function TDynOutPortIndent.DatumType: TDatumType;                                                                                begin Result := Port.DatumType; end;
function TDynOutPortIndent.AsVariant: Variant;                                                                                   begin Result := Port.AsVariant; end;
function TDynOutPortIndent.DisplayStr(NeededChars: Integer): string;                                                             begin Result := Port.DisplayStr(NeededChars); end;
function TDynOutPortIndent.WriteStr(NeededChars: Integer): string;                                                               begin Result := Port.WriteStr(NeededChars); end;
//IDynOutPort = interface(IDynDatum)
//function Write(Obj: dyn): Boolean; stdcall;
//function WriteSepar: Boolean; stdcall;
function TDynOutPortIndent.WriteLn: Boolean; stdcall;                                                                            begin Result := Port.WriteLn; end;
function TDynOutPortIndent.WriteSpecial(v: TSpecialValue): Boolean; stdcall;                                                     begin Result := Port.WriteSpecial(v); end;
function TDynOutPortIndent.WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean; stdcall;                                  begin Result := Port.WriteInt(p, cb, Sign); end;
function TDynOutPortIndent.WriteFloat(v: Single): Boolean; overload; stdcall;                                                    begin Result := Port.WriteFloat(v); end;
function TDynOutPortIndent.WriteFloat(v: Double): Boolean; overload; stdcall;                                                    begin Result := Port.WriteFloat(v); end;
function TDynOutPortIndent.WriteStrA(p: PAnsiChar; cc: Integer; Kind: TStrKind = skFull; CodePage: Word = 0): Boolean; stdcall;  begin Result := Port.WriteStrA(p, cc, Kind, CodePage); end;
function TDynOutPortIndent.WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; stdcall;                      begin Result := Port.WriteStrW(p, cc, Kind); end;
function TDynOutPortIndent.WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; stdcall;                      begin Result := Port.WriteMem (p, cb, Kind); end;
function TDynOutPortIndent.WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; stdcall;                       begin Result := Port.WriteList(val, Kind); end;
//function BeginList(Kind: TListKind): IDynOutPort; stdcall;
{$ENDIF}

end.

