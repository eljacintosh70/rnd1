unit DTPort;

interface

uses
  SysUtils,
  DynTypes, DTDatum, DUtils;

type
  TAbstractDynOutPort = class(TDyn, IDynOutPort)
  public
    function Write(Obj: dyn): Boolean; virtual; stdcall; abstract;
    function WriteSepar: Boolean; virtual; stdcall; abstract;
    function WriteLn: Boolean; virtual; stdcall; abstract;
    function WriteSpecial(v: TSpecialValue): Boolean; virtual; stdcall; abstract;
    function WriteInt(p: Pointer; cb: Integer; Sign: TIntSign): Boolean; virtual; stdcall; abstract;
    function WriteFloat(v: Single): Boolean; overload; virtual; stdcall; abstract;
    function WriteFloat(v: Double): Boolean; overload; virtual; stdcall; abstract;
    function WriteStrA(p: PAnsiChar; cc: Integer; Kind: TStrKind = skFull; CodePage: Word = 0): Boolean; virtual; stdcall; abstract;
    function WriteStrW(p: PWideChar; cc: Integer; Kind: TStrKind = skFull): Boolean; virtual; stdcall; abstract;
    function WriteMem (p: PAnsiChar; cb: Integer; Kind: TMemKind = mkFull): Boolean; virtual; stdcall; abstract;
    function WriteList(const val: array of dyn; Kind: TListKind = lkList): Boolean; virtual; stdcall; abstract;
    function BeginList(Kind: TListKind): IDynOutPort; virtual; stdcall; abstract;
  public
    function DatumType: TDatumType; override;
  end;

  TWriteProc = function (p: Pointer; cb: Integer): Boolean of object;

  TCustomDynOutPort = class(TAbstractDynOutPort)
  public
    WriteProc: TWriteProc;
    constructor Create(AWriteProc: TWriteProc = nil);
  end;

function WriteStr(v: dyn): string; stdcall;
function DisplayStr(v: dyn): string; stdcall;

exports
  WriteStr name 'WriteStr',
  DisplayStr name 'DisplayStr';

implementation

uses
  DTPortW;

function WriteStr(v: dyn): string; stdcall;
var
  TextOut: TStrTextOutW;
  PortObj: TDynOutPort;
  Port: IDynOutPort;
begin
  TextOut := TStrTextOutW.Create(200);
  PortObj := TDynOutPort.Create(TextOut.WriteProc);
  Port := PortObj;
  Port.Write(v);
  Result := TextOut.GetText;
end;

function DisplayStr(v: dyn): string; stdcall;
var
  TextOut: TStrTextOutW;
  PortObj: TDynOutPortWr;
  Port: IDynOutPort;
begin
  TextOut := TStrTextOutW.Create(200);
  PortObj := TDynOutPortWr.Create(TextOut.WriteProc);
  Port := PortObj;
  Port.Write(v);
  Result := TextOut.GetText;
end;

{ TAbstractDynOutPort }

function TAbstractDynOutPort.DatumType: TDatumType;
begin
  Result := atOpaque
end;

{ TCustomDynOutPort }

constructor TCustomDynOutPort.Create(AWriteProc: TWriteProc);
begin
  WriteProc := AWriteProc;
end;

end.

