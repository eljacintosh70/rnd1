unit DTBool;

interface

uses
  DynTypes, DTDatum, DUtils;

type
  TCustomBool = class(TDyn)
  public
    // objetos constantes, no destruir, no contar referencias
    function _AddRef: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
  public
    function DatumType: TDatumType; override;
  end;

  TFalseObj = class(TCustomBool)
  public
    function AsVariant: Variant; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function WriteStr(NeededChars: Integer): String; override;
  protected
    procedure CastToBool(var Msg: TVarMessage); message MsgCastToBool;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TTrueObj = class(TCustomBool)
  public
    function AsVariant: Variant; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function WriteStr(NeededChars: Integer): String; override;
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

function TFalseObj.AsVariant: Variant;
begin
  Result := False
end;

function TFalseObj.DisplayStr(NeededChars: Integer): String;
begin
  Result := '#f'
end;

function TFalseObj.WriteStr(NeededChars: Integer): String;
begin
  Result := '#f'
end;

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

function TTrueObj.AsVariant: Variant;
begin
  Result := True
end;

function TTrueObj.DisplayStr(NeededChars: Integer): String;
begin
  Result := '#t'
end;

function TTrueObj.WriteStr(NeededChars: Integer): String;
begin
  Result := '#t'
end;

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
