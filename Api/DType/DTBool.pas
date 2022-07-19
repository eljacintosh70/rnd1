unit DTBool;

interface

uses
  DynTypes, DTDatum, DUtils;

type
  TCustomBool = class(TDyn)
  public
    // objetos constantes, no destruir, no contar referencias
    function _AddRef: Integer; override; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; override; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    function DatumType: TDatumType; override;
  end;

  TFalseObj = class(TCustomBool)
  protected
    procedure CastToBool(var Msg: TVarMessage); message MsgCastToBool;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TTrueObj = class(TCustomBool)
  protected
    procedure CastToBool(var Msg: TVarMessage); message MsgCastToBool;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

implementation

{ TCustomBool }

function TCustomBool._AddRef: Integer;
begin
  Result := -1
end;

function TCustomBool._Release: Integer;
begin
  Result := -1
end;

function TCustomBool.DatumType: TDatumType;
begin
  Result := atBool
end;

{ TFalseObj }

procedure TFalseObj.CastToBool(var Msg: TVarMessage);
begin
  PBoolean(Msg.VarPtr)^ := False;
  Msg.Res := ResOK;
end;

procedure TFalseObj.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteSpecial(svFalse);
  Msg.Res := Ord(r);
end;

{ TTrueObj }

procedure TTrueObj.CastToBool(var Msg: TVarMessage);
begin
  PBoolean(Msg.VarPtr)^ := True;
  Msg.Res := ResOK;
end;

procedure TTrueObj.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteSpecial(svTrue);
  Msg.Res := Ord(r);
end;

end.
