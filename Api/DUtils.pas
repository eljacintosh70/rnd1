unit DUtils;

interface

uses
  SysUtils,
  DynTypes;

type
  EDynError = class(Exception)
    constructor Create(Msg: String; const Arg: array of dyn); virtual;
  end;
  TDynErrorClass = class of EDynError;

  PDynErrorInfo = ^TDynErrorInfo;
  TDynErrorInfo = record
    EClass: TDynErrorClass;
    EMsg: string;
  end;

// Mensajes que pueden ser enviados a Obj.Dispatch(Cmd)
const
   // reciben un TVarMessage
   MsgCastToBool   = $001;
   MsgCastToChar   = $002;
   MsgCastToInt64  = $003;   // Cmd.VarPtr^ := Int64 (Obj)
   MsgCastToDouble = $004;   // Cmd.VarPtr^ := Double(Obj)
   MsgCastToString = $005;   // Cmd.VarPtr^ := String(Obj)

   MsgIsSymbol     = $006;   // Cmd.Res     := Ord(Obj is IDynSymbol)
   MsgIsSymbolR    = $007;   // Cmd.VarPtr^ := IDynSymbol(Obj)
   MsgIsPair       = $008;   // Cmd.Res     := Ord(Obj is IDynPair)
   MsgIsPairR      = $009;   // Cmd.VarPtr^ := IDynPair(Obj)

   // reciben un TWriteMsg
   MsgWrite        = $010;  // escribelos objetos en una forma en que debería poder leerse
   MsgDisplay      = $011;  // forma no reversible, por ejemplo, texto sin comillas
   MsgDebugWrite   = $012;  // MsgWrite con información extra, de ser necesario

   // reciben un TEvalMessage
   MsgEval         = $020;  //  evalúa un objeto usando los valores indicados en Scope
   MsgEvalItems    = $021;  //  evalúa cada item de una lista usando los valores indicados en Scope
   // reciben un TEvalCallMessage
   MsgEvalCall     = $022;

type
   TVarMessage = record
     Msg, Res: Integer;
     VarPtr: Pointer;
   end;

   TEvalMessage = record
     Msg: Integer;
     Res: TDatumRef;
     Scope: IDynScope;
   end;

   TEvalCallMessage = record
     Msg: Integer;
     Res: TDatumRef;
     Scope: IDynScope;
     Params: dyn;
   end;

const
  ErrCastToBool:   TDynErrorInfo = (EClass: EDynError; EMsg: 'cannot cast %t to bool');
  ErrCastToInt:    TDynErrorInfo = (EClass: EDynError; EMsg: 'cannot cast %t to int');
  ErrCastToDouble: TDynErrorInfo = (EClass: EDynError; EMsg: 'cannot cast %t to double');
  ErrCastToChar:   TDynErrorInfo = (EClass: EDynError; EMsg: 'cannot cast %t to char');
  ErrCastToString: TDynErrorInfo = (EClass: EDynError; EMsg: 'cannot cast %t to string');

const
  ResOK = 0;
  ResNotImpl = -1;

function  HandleMessageWithPointer(Obj: dyn; Msg: Integer; Ptr: Pointer): Boolean;
procedure HandleMessageWithPointer_Err(Obj: dyn; Msg: Integer; Ptr: Pointer; Err: PDynErrorInfo);

type
  TIntSign = (isPositive, isTwoComp, isNegative);
  TStrKind = (skFull, skStart, skMid, skEnd, skSymbol, skChar, skRaw);
  TMemKind = (mkFull, mkStart, mkMid, mkEnd);
  TListKind = (lkList, lkVector, lkDict, lkKeyValue);
  TSpecialValue = (svFalse, svTrue, svNil);

  IDynOutPort = interface(IDynDatum)
    ['{D728B19F-5FE4-4229-A4CB-005897EE073D}']
    function Write(Obj: dyn): Boolean; stdcall;
    function WriteSepar: Boolean; stdcall;
    function WriteLn: Boolean; stdcall;
    function WriteSpecial(v: TSpecialValue): Boolean; stdcall;
    function WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean; stdcall;
    function WriteFloat(v: Single): Boolean; overload; stdcall;
    function WriteFloat(v: Double): Boolean; overload; stdcall;
    function WriteStrA(p: PAnsiChar; cc: Integer; Kind: TStrKind = skFull; CodePage: Word = 0): Boolean; stdcall;
    function WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; stdcall;
    function WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; stdcall;
    function WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; stdcall;
    function BeginList(Kind: TListKind): IDynOutPort; stdcall;
  end;

  TWriteMsg  = record
     Msg, Res: Integer;
     Port: IDynOutPort;
   end;

function WriteToPort(const Port: IDynOutPort; s: UnicodeString): Boolean;

implementation

function Str(Obj: dyn): String;
begin
  Result := '';
end;

function  HandleMessageWithPointer(Obj: dyn; Msg: Integer; Ptr: Pointer): Boolean;
var
  Cmd: TVarMessage;
begin
  Cmd.Msg := Msg;
  Cmd.Res := ResNotImpl;
  Cmd.VarPtr := Ptr;
  Obj.Ref.DispatchMsg(Cmd);
  Result := (Cmd.Res = ResOK);
end;

procedure HandleMessageWithPointer_Err(Obj: dyn; Msg: Integer; Ptr: Pointer; Err: PDynErrorInfo);
var
  Cmd: TVarMessage;
begin
  Cmd.Msg := Msg;
  Cmd.Res := ResNotImpl;
  Cmd.VarPtr := Ptr;
  Obj.Ref.DispatchMsg(Cmd);
  case Cmd.Res of
    ResOK: Exit;
    ResNotImpl:
      raise Err.EClass.Create('Error: Not Implemented. ' + Err.EMsg, Obj) {$IFNDEF FPC} at ReturnAddress {$ENDIF};
    else
      raise Err.EClass.Create(Format('Error: %d. ', [Cmd.Res]) + Err.EMsg, Obj) {$IFNDEF FPC} at ReturnAddress {$ENDIF};
  end;
end;

function WriteToPort(const Port: IDynOutPort; s: UnicodeString): Boolean;
begin
  Result := Port.WriteStrW(Pointer(s), Length(s), skRaw);
end;

function DynTypeName(Val: dyn): string;
begin
  Result := TObject(Pointer(Val)).ClassName
end;

{ EDynError }

constructor EDynError.Create(Msg: String; const Arg: array of dyn);
var
  Str: {$IFDEF FPC} String {$ELSE} TStringBuilder {$ENDIF};
  p, pe: PChar;
  cb, iParam: Integer;
  ch: Char;
begin
{$IFDEF FPC}
  cb := Length(Msg);
  p := Pointer(Msg);
  pe := p + cb;
  Str := '';
  iParam := 0;
  while p < pe do
  begin
    ch := p^;
    Inc(p);
    case ch of
      '%':
        begin
          ch := p^;
          Inc(p);
          case ch of
            't', 'T':
              begin
                Str := Str + DynTypeName(Arg[iParam]);
                Inc(iParam);
              end;
            'd', 'D':
              begin
                Str := Str + FloatToStr(Double(Arg[iParam]));
                Inc(iParam);
              end;
            's', 'S':
              begin
                Str := Str + String(Arg[iParam]);
                Inc(iParam);
              end;
          else
            Str := Str + ch;
          end;
        end;
      else Str := Str + ch;
    end;
  end;
  Msg := Str;
  inherited Create(Msg);
{$ELSE}
  cb := Length(Msg);
  p := Pointer(Msg);
  pe := p + cb;
  Str := TStringBuilder.Create(cb);
  iParam := 0;
  while p < pe do
  begin
    ch := p^;
    Inc(p);
    case ch of
      '%':
        begin
          ch := p^;
          Inc(p);
          case ch of
            't', 'T':
              begin
                Str.Append(DynTypeName(Arg[iParam]));
                Inc(iParam);
              end;
            'd', 'D':
              begin
                Str.Append(Double(Arg[iParam]));
                Inc(iParam);
              end;
            's', 'S':
              begin
                Str.Append(String(Arg[iParam]));
                Inc(iParam);
              end;
          else
            Str.Append(ch);
          end;
        end;
      else Str.Append(ch);
    end;
  end;
  Msg := Str.ToString;
  inherited Create(Msg);
{$ENDIF}
end;

end.
