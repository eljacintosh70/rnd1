unit DTDatum;

interface

uses
  //Messages, ActiveX, Classes, ComObj,
  Windows, SysUtils,
  DynTypes, DUtils;

{$define INLINE_VMT}

{$ifdef INLINE_VMT} // Esta constante activa un hack para reducir el tamaño de
const               // los objetos que implementan interfaces, reusando la misma
  InlineVMT = true; // VMT de la clase para las diferentes interfaces.
{$endif}            // Las clases debe declarar como métodos virtuales todos los
                    // métodos de las interfaces que implementa, en el mismo orden,
                    // y sin otros métodos virtuales antes o en medio

type
  TCustomInterface = class(TObject, IInterface)
  // InlineVMT requiere los siguientes métodos idénticos a los de IInterface
  public
    {$ifdef FPC}
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; stdcall;
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    {$endif}
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  private
    function GetAsIInterface: IInterface;
  public
    property AsIInterface: IInterface read GetAsIInterface {$if Declared(InlineVMT)} implements IInterface {$ifend};
  protected
    FRefCount: Integer;
    procedure InvertMem(n: Integer = 0);
  public
    destructor Destroy; override;
    property RefCount: Integer read FRefCount;
    {$ifdef DEBUG_DESTROY}
    procedure FreeInstance; override;
    {$endif}
  end;

  TCustomDyn = class(TCustomInterface, IDyn)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDyn
  public
    // debería soportar el operador Obj(arg), pero no funciona,
    // hay que llamar Obj.Invoke(arg)
    function Invoke(const Arg: array of dyn): dyn; virtual; stdcall;
    // soportar for in
    function GetEnumerator: IDynEnumerator; virtual; stdcall;
    // esto permite usar el operador Obj[i] tanto para listas como para diccionarios
    function GetItem(const Key: dyn): dyn; virtual; stdcall;
    procedure SetItem(const Key: dyn; const Value: dyn); virtual; stdcall;
    property Item[const Key: dyn]: dyn read GetItem write SetItem;
        default;
    // llamar a método por Id: Obj.Id(Arg)
    function Method(Id: dyn; Arg: array of dyn): dyn; virtual; stdcall;
    // usado por el evaluador
    procedure DispatchMsg(var Message); virtual; stdcall;
  private
    function GetAsIDyn: IDyn;
  public
    property AsIDyn: IDyn read GetAsIDyn {$if Declared(InlineVMT)} implements IDyn {$ifend};
  protected
    procedure DoMsgWrite(var Msg: TWriteMsg); message MsgWrite;
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TDyn = class(TCustomDyn, IDynDatum)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynDatum
  public
    function CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer; virtual;
    function DatumType: TDatumType; virtual; abstract;
    function AsVariant: Variant; virtual;
    function DisplayStr(NeededChars: Integer): String; virtual;
    function WriteStr(NeededChars: Integer): String; virtual;
  private
    function GetAsIDatum: IDynDatum;
  public
    property AsIDatum: IDynDatum read GetAsIDatum {$if Declared(InlineVMT)} implements IDynDatum {$ifend};
  end;

  TInterfaceHelper = TCustomInterface deprecated 'use: TCustomInterface';
  TDynHelper = TCustomDyn deprecated 'use: TCustomDyn';
  TDatumHelper = TDyn deprecated 'use: TDyn';
  TDatumHelper2 = class(TDyn)
  public
  end;

  TCustomEnumerator = class(TCustomInterface, IDynEnumerator)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynEnumerator
  public
    function MoveNext: Boolean; virtual; stdcall; abstract;
    function GetCurrent: dyn; virtual; stdcall; abstract;
    property Current: dyn read GetCurrent;
  private
    function GetAsIDynEnumerator: IDynEnumerator;
  public
    property AsIDynEnumerator: IDynEnumerator read GetAsIDynEnumerator {$if Declared(InlineVMT)} implements IDynEnumerator {$ifend};
  end;

implementation

var
  DebugStr: String;

{ TCustomInterface }

function TCustomInterface.QueryInterface({$ifdef FPC} constref {$else} const {$endif} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCustomInterface._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TCustomInterface._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    DebugStr := Format('%s(%p)', [ClassName, Pointer(Self)]);
    Destroy;
  end;
end;

function TCustomInterface.GetAsIInterface: IInterface;
begin
  {$if Declared(InlineVMT)} Result := IInterface(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

procedure TCustomInterface.InvertMem(n: Integer = 0);
var
  i: Integer;
  p: PRaw;
begin
  if n = 0 then
    n := InstanceSize;
  p := Pointer(Self);
  for i := 0 to n - 1 do
    Byte(p[i]) := Byte(p[i]) xor $80;
end;

destructor TCustomInterface.Destroy;
begin
  {$ifdef DEBUG_MEM}
    InvertMem; // para generar excepciones al accesar objeto ya destruido
  {$endif}
  inherited Destroy;
end;

{$ifdef DEBUG_DESTROY}
procedure TCustomInterface.FreeInstance;
begin
  //inherited FreeInstance;
  CleanupInstance;
  TDestroyed.Patch(Self);
  //_FreeMem(Pointer(Self));
end;
{$endif}

{ TCustomDyn }

function TCustomDyn.Invoke(const Arg: array of dyn): dyn;
begin
  // TypeError: 'str' object is not callable
  DynError('TypeError: ''%t'' object is not callable', [IDyn(Self)])
end;

function TCustomDyn.GetEnumerator: IDynEnumerator;
begin
  // TypeError: 'int' object is not iterable
  DynError('TypeError: ''%t'' object is not iterable', [IDyn(Self)])
end;

function TCustomDyn.GetItem(const Key: dyn): dyn;
begin
  // a = {"x":10, "y":20}
  // a["x"]
  // -> 10
  // a["z"]
  // -> KeyError: 'z'

  // b=[1,2,3]
  // b[10]
  // -> IndexError: list index out of range
  // b["x"]
  // TypeError: list indices must be integers or slices, not str

  // 1[3]
  // -> TypeError: 'int' object is not subscriptable
  // 1["a"]
  // -> TypeError: 'int' object is not subscriptable
  DynError('TypeError: ''%t'' object is not subscriptable', [IDyn(Self)])
end;

procedure TCustomDyn.SetItem(const Key, Value: dyn);
begin
  // 1[1]=10
  // TypeError: 'int' object does not support item assignment
  DynError('TypeError: ''%t'' object does not support item assignment', [IDyn(Self)])
end;

function TCustomDyn.Method(Id: dyn; Arg: array of dyn): dyn;
begin
  // a = {"x":10, "y":20}
  // a["x"](5)
  // -> TypeError: 'int' object is not callable
  // a["z"](5)
  // -> KeyError: 'z'
  DynError('MethodError: ''%t'' has no method ''%s''', [IDyn(Self), Id])
end;

procedure TCustomDyn.DispatchMsg(var Message);
begin
  Dispatch(Message);
end;

function TCustomDyn.GetAsIDyn: IDyn;
begin
  {$if Declared(InlineVMT)} Result := IDyn(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

procedure TCustomDyn.DoMsgWrite(var Msg: TWriteMsg);
begin
  Msg.Msg := MsgDisplay;  // si no se define Write, es igual a Display
  Dispatch(Msg);
  Msg.Msg := MsgWrite;
end;

procedure TCustomDyn.DoMsgDisplay(var Msg: TWriteMsg);
var
  s: string;
begin
  s := Format('(:%s %p)', [ClassName(), Pointer(Self)]);
  Msg.Res := Ord(WriteToPort(Msg.Port, s));
end;

{ TDyn }

function TDyn.CommandExec(Command: Integer; Res,
  Data: Pointer): Integer;
begin
  Result := E_NOTIMPL
end;

function TDyn.AsVariant: Variant;
var
  DisplayStrFn: function (NeededChars: Integer): String of object;
begin
  DisplayStrFn := DisplayStr;
  if TMethod(DisplayStrFn).Code <> @TDyn.DisplayStr then
    Result := DisplayStrFn(MaxInt)
  else
    Result := Format('%s(%p)', [ClassName, Pointer(Self)]);
end;

function TDyn.DisplayStr(NeededChars: Integer): String;
begin
  Result := AsVariant;
end;

function TDyn.WriteStr(NeededChars: Integer): String;
begin
  Result := DisplayStr(NeededChars);
end;

function TDyn.GetAsIDatum: IDynDatum;
begin
  {$if Declared(InlineVMT)} Result := IDynDatum(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

{ TCustomEnumerator }

function TCustomEnumerator.GetAsIDynEnumerator: IDynEnumerator;
begin
  {$if Declared(InlineVMT)} Result := IDynEnumerator(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

end.
