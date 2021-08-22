unit DTFloat;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTDatum, DUtils;

type
  Real = Double;

type
  TDynFloat = class(TDyn, IDynFloat)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynFloat
  public
    function Value: Real; virtual;
  private
    function GetAsIDynFloat: IDynFloat;
  public
    property AsIDynFloat: IDynFloat read GetAsIDynFloat {$if Declared(InlineVMT)} implements IDynFloat {$ifend};
  public
    function DatumType: TDatumType; override;
    function AsVariant: Variant; override;
    function DisplayStr(NeededChars: Integer): String; override;
  private
    FValue: Real;
  public
    constructor Create(AValue: Real);
    procedure CastToDouble(var Msg: TVarMessage); message MsgCastToDouble;
  protected
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TFloNumHelper = TDynFloat deprecated 'use: TDynFloat';

implementation /////////////////////////////////////////////////////////////////

uses
  SysUtils;

{ TDynFloat }

function TDynFloat.Value: Real;
begin
  Result := FValue;
end;

function TDynFloat.GetAsIDynFloat: IDynFloat;
begin
  {$if Declared(InlineVMT)} Result := IDynFloat(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynFloat.DatumType: TDatumType;
begin
  Result := atReal;
end;

function TDynFloat.AsVariant: Variant;
begin
  Result := FValue;
end;

function TDynFloat.DisplayStr(NeededChars: Integer): String;
begin
  Result := FloatToStr(FValue);
end;

constructor TDynFloat.Create(AValue: Real);
begin
  FValue := AValue;
end;

procedure TDynFloat.CastToDouble(var Msg: TVarMessage);
begin
  PDouble(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynFloat.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteFloat(FValue);
  Msg.Res := Ord(r);
end;

end. ///////////////////////////////////////////////////////////////////////////

