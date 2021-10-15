unit DTInt;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTDatum, DUtils;

type
  TAbstractDynInt = class(TDyn, IDynInt)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynInt
  public
    function ByteCount: Integer; virtual; abstract;
    function BytePtr: PByte; virtual; abstract;
  private
    function GetAsIBigInt: IDynInt;
  public
    property AsIBigInt: IDynInt read GetAsIBigInt {$if Declared(InlineVMT)}implements IDynInt {$ifend};
  public
    function DatumType: TDatumType; override;
  end;

  TDynInt32 = class(TAbstractDynInt)
  private
    FValue: Integer;
  public
    constructor Create(Value: Integer);
    function ByteCount: Integer; override;
    function BytePtr: PByte; override;
  protected
    procedure CastToInt64(var Msg: TVarMessage); message MsgCastToInt64;
    procedure CastToDouble(var Msg: TVarMessage); message MsgCastToDouble;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TDynInt64 = class(TAbstractDynInt)
  private
    FValue: Int64;
  public
    constructor Create(Value: Int64);
    function ByteCount: Integer; override;
    function BytePtr: PByte; override;
  protected
    procedure CastToInt64(var Msg: TVarMessage); message MsgCastToInt64;
    procedure CastToDouble(var Msg: TVarMessage); message MsgCastToDouble;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

implementation

uses
  SysUtils;

{ TAbstractDynInt }

function TAbstractDynInt.GetAsIBigInt: IDynInt;
begin
  {$if Declared(InlineVMT)} Result := IDynInt(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TAbstractDynInt.DatumType: TDatumType;
begin
  Result := atInteger;
end;

{ TDynInt32 }

constructor TDynInt32.Create(Value: Integer);
begin
  FValue := Value
end;

function TDynInt32.ByteCount: Integer;
begin
  Result := 4;
end;

function TDynInt32.BytePtr: PByte;
begin
  Result := @FValue;
end;

procedure TDynInt32.CastToInt64(var Msg: TVarMessage);
begin
  PInt64(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynInt32.CastToDouble(var Msg: TVarMessage);
begin
  PDouble(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynInt32.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteInt(@FValue, SizeOf(FValue), isTwoComp);
  Msg.Res := Ord(r);
end;

{ TDynInt64 }

constructor TDynInt64.Create(Value: Int64);
begin
  FValue := Value
end;

function TDynInt64.ByteCount: Integer;
begin
  Result := 8
end;

function TDynInt64.BytePtr: PByte;
begin
  Result := @FValue;
end;

procedure TDynInt64.CastToInt64(var Msg: TVarMessage);
begin
  PInt64(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynInt64.CastToDouble(var Msg: TVarMessage);
begin
  PDouble(Msg.VarPtr)^ := FValue;
  Msg.Res := ResOK;
end;

procedure TDynInt64.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
begin
  r := Msg.Port.WriteInt(@FValue, SizeOf(FValue), isTwoComp);
  Msg.Res := Ord(r);
end;

end.
