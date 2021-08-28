unit DTProcRTTI;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTDatum, DTProc;

type
  TDynFuncRTTI = class(TDynFunc)
  public
    Method: TMethod;
    TypInfo: Pointer;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
  public
    constructor Create(ATypInfo: Pointer; AMethod: TMethod);
  end;

  TDynFuncAuto = class(TDynFuncRTTI)
  public
    function DatumType: TDatumType; override;
    procedure Call(out Result: TDatumRef; Par: TDynDatum); override;
  public
    constructor Create(ATypInfo: Pointer; AMethod: TMethod);
  end;

function ExtFuncDatum(ATypInfo: Pointer; AMethod: TMethod): TDynDatum; {$ifdef INLINE} inline; {$endif}
function AutoFuncDatum(ATypInfo: Pointer; AMethod: TMethod): TDynDatum; {$ifdef INLINE} inline; {$endif}
procedure CallAutoFunc(out Result: TDatumRef; ExtFunc: TDynFuncRTTI; Par:
    TDynDatum);

type
  PAutoTable = ^TAutoTable;
  PAutoEntry = ^TAutoEntry;
  PAutoParamInfo = ^TAutoParamInfo;

  TAutoEntry = record
    Id: Integer;
    Name: PShortString;
    Unk1: Integer;
    ParamInfo: PAutoParamInfo;
    MethodAddr: Pointer;
  end;
  TAutoParamInfo = record
    ResultType: Byte;
    ParamCount: Byte;
    ParamType: array[0..$FF] of Byte;
  end;
  TAutoTable = record
    Len: Integer;
    Entry: array[0..$FF] of TAutoEntry;
  end;
  {
00458760: $03 $00 $00 $00
00458764: $03 $00 $00 $00 $B5 $87 $45 $00 $01 $00 $00 $00 $B9 $87 $45 $00 $9C $89 $45 $00
00458778: $02 $00 $00 $00 $AA $87 $45 $00 $01 $00 $00 $00 $B1 $87 $45 $00 $80 $89 $45 $00
00458780: $01 $00 $00 $00 $A0 $87 $45 $00 $01 $00 $00 $00 $A6 $87 $45 $00 $64 $89 $45 $00

004587A0: $05 'Subst'  $05 $02 $05 $05
004587AA: $06 'Divide' $05 $02 $05 $05
004587B5: $03 'Pow',   $05 $02 $05 $05
  }

implementation

uses
  SysUtils, ActiveX;

function ExtFuncDatum(ATypInfo: Pointer; AMethod: TMethod): TDynDatum;
begin
  Result := Pointer(TDynFuncRTTI.Create(ATypInfo, AMethod));
end;

function AutoFuncDatum(ATypInfo: Pointer; AMethod: TMethod): TDynDatum;
begin
  Result := Pointer(TDynFuncAuto.Create(ATypInfo, AMethod));
end;

procedure CallAutoFunc(out Result: TDatumRef; ExtFunc: TDynFuncRTTI; Par:
    TDynDatum);
var
  AutoEntry: PAutoEntry;
  Obj: Pointer;
  Fn: Pointer;
  ParamInfo: PAutoParamInfo;
  Res64: Int64;
  i: Integer;
  Datum: TDynDatum;
  r: Real;
  ValInt: Integer;
  StrRes: String;
  LastParam: PAnsiChar;
begin
  AutoEntry := ExtFunc.TypInfo;
  //AutoEntry.ParamInfo

  Obj := ExtFunc.Method.Data;
  Fn := AutoEntry.MethodAddr;
  ParamInfo := AutoEntry.ParamInfo;
  asm
    mov LastParam,esp
  end;
  for i := 0 to ParamInfo.ParamCount - 1 do
  begin
    Datum := car(Par);
    case ParamInfo.ParamType[i] of
      // VT_BOOL
      // VT_I8, VT_UI8
      VT_INT, VT_I1, VT_I2, VT_I4,
      VT_UINT, VT_UI1, VT_UI2, VT_UI4:
        begin
          NeedInteger(Datum, ValInt);
          asm
            push ValInt
          end;
        end;
      VT_R4, VT_R8,
      VT_CY, VT_DATE:
        begin
          r := Datum.AsVariant;
          asm
            push r.DWORD[4]
            push r.DWORD[0]
          end;
        end;
    end;
    Par := cdr(Par);
  end;

  case ParamInfo.ResultType of
    VT_BSTR, VT_CLSID:
      begin
        asm
          lea  edx,[StrRes]
          push edx

          mov  eax,Obj
          mov  edx,LastParam
          mov  ecx,[edx-8]
          mov  edx,[edx-4]
          call [Fn]
        end;
        Result := (make_string(StrRes));
      end;
    VT_R4, VT_R8,
    VT_CY, VT_DATE:
      begin
        asm
          mov  eax,Obj
          call [Fn]
          fstp Res64
        end;
        Result := (MakeDouble(PDouble(@Res64)^));
      end;
    else
      raise Exception.Create('unsuported automated result type');
  end;
  {
  asm
    mov  eax,Obj
    call [Fn]
    mov  ecx,ParamInfo
    mov  dl,TAutoParamInfo([ecx]).ResultType

    mov  ecx,ParamInfo
    mov  dl,TAutoParamInfo([ecx]).ResultType
    cmp  dl,VT_R4       // VT_R4 = 4;
    jl   @NoFloat       // VT_R8 = 5;
    cmp  dl,VT_DATE     // VT_CY = 6;
    jg   @NoFloat       // VT_DATE = 7;


    @NoFloat:

    @Ready:
  end;
  }
end;

{ TDynFuncRTTI }

function TDynFuncRTTI.DatumType: TDatumType;
begin
  Result := atExtFunc
end;

function TDynFuncRTTI.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('?:%p', [Pointer(Self)]);
end;

procedure TDynFuncRTTI.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  TProcedureMethod(Method)(Result, Par);
end;

constructor TDynFuncRTTI.Create(ATypInfo: Pointer; AMethod: TMethod);
begin
  inherited Create;
  //p.RefCount := 0;
  //p.DatumType := atExtFunc;
  TypInfo := ATypInfo;
  Method := AMethod;
end;

{ TDynFuncAuto }

function TDynFuncAuto.DatumType: TDatumType;
begin
  Result := atAutoFunc
end;

procedure TDynFuncAuto.Call(out Result: TDatumRef; Par: TDynDatum);
begin
  CallAutoFunc(Result, Self, Par);
end;

constructor TDynFuncAuto.Create(ATypInfo: Pointer; AMethod: TMethod);
begin
  inherited Create(ATypInfo, AMethod);
end;

end.
