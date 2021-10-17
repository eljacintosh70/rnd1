unit DTScope;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  Generics.Collections,
  DynTypes, DTDatum, DTPair;

type
  IDebug = interface(IInterface)
    ['{FA8A83D6-5EF6-4E9C-AF36-D6023A783E49}']
    function Debug: String;
  end;

  TAbstractDynScope = class(TDyn, IDynScope)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynDatum
  public
    function GetParent: IDynScope; virtual; abstract;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; virtual; abstract;
    property Value[const Key: dyn]: dyn read GetItem write SetItem;
    property Parent: IDynScope read GetParent;
  private
    function GetAsIDynScope: IDynScope;
  public
    property AsIDynScope: IDynScope read GetAsIDynScope {$if Declared(InlineVMT)} implements IDynScope {$ifend};
  public
    function DatumType: TDatumType; override;
    function Method(Id: dyn; Arg: array of dyn): dyn; override; stdcall;
  end;

  TListScope = class(TAbstractDynScope, IDebug)
  public
    function GetParent: IDynScope; override;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; override;
    function GetItem(const Key: dyn): dyn; override; stdcall;
    procedure SetItem(const Key: dyn; const Value: dyn); override; stdcall;
  public
    function Debug: String;
  protected
    FParent: IDynScope;
    List: IDynPair;
  public
    constructor Create(AParent: IDynScope = nil);
    destructor Destroy; override;
    property Value[const Key: dyn]: dyn read GetItem write SetItem;
    property Parent: IDynScope read GetParent;
  end;

  TKVPair = TPair<dyn, dyn>;
  TKVArray = TArray<TKVPair>;
  TSSymbolList = class(TDictionary<dyn, dyn>)
  public
    destructor Destroy; override;
    //function AddObject(const S: string; AObject: TObject): Integer; override;
  end;

  TBigScope = class(TAbstractDynScope, IDebug)
  public
    function GetParent: IDynScope; override;
    function GetItem(const Key: dyn): dyn; override; stdcall;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; override;
    procedure SetItem(const Key: dyn; const Value: dyn); override; stdcall;
  protected
    FParent: IDynScope;
  public
    function Debug: String;
  protected
    List: TSSymbolList;
  public
    {$ifdef FPC}
    function QueryInterface(constref IID: TGUID; out Obj): HResult; override; Cdecl;
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    {$endif}
  public
    constructor Create(AParent: IDynScope = nil);
    destructor Destroy; override;
    property Value[const Key: dyn]: dyn read GetItem write SetItem;
    property Parent: IDynScope read GetParent;
  end;

implementation /////////////////////////////////////////////////////////////////

{ TAbstractDynScope }

function TAbstractDynScope.GetAsIDynScope: IDynScope;
begin
  {$if Declared(InlineVMT)} Result := IDynScope(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TAbstractDynScope.DatumType: TDatumType;
begin
  Result := atScope
end;

function TAbstractDynScope.Method(Id: dyn; Arg: array of dyn): dyn;
var
  Fn: dyn;
begin
  Fn := GetItem(Id);
  Fn.Invoke(Arg);
end;

{ TListScope }

function TListScope.GetParent: IDynScope;
begin
  Result := FParent;
end;

function TListScope.GetItem(const Key: dyn): dyn;
var
  Res: TDynDatum;
begin
  Res := Assq(Key, List);
  if Assigned(Res) then
    Result := car(cdr(Res))
  else if Assigned(Parent) then
    Result := Parent.Value[Key]
  else
    Result := Unbound
end;

function TListScope.GetLocalValue(Symbol: TDynDatum): TDynDatum;
begin
  Result := Assq(Symbol, List);
  if Assigned(Result) then
    Result := car(cdr(Result))
  else
    Result := Unbound
end;

procedure TListScope.SetItem(const Key: dyn; const Value: dyn);
var
  Pair: IDynPair;
begin
  Pair := make_list([Key.Value, Value.Value]);
  List := DynTypes.cons(Pair, List)
end;

function TListScope.Debug: String;
begin
  Result := Deb(List)
end;

constructor TListScope.Create(AParent: IDynScope = nil);
begin
  FParent := AParent;
end;

destructor TListScope.Destroy;
begin
  inherited Destroy;
end;

{ TSSymbolList }

{
function TSSymbolList.AddObject(const S: string;
  AObject: TObject): Integer;
var
  PrevObject: TDynDatum;
begin
  AObject := TDynDatum(AObject).NewRef;
  if Find(S, Result) then
  begin
    PrevObject := TDynDatum(Objects[Result]);
    PrevObject.Free;
    Objects[Result] := AObject;
    //Strings[Result] := S;
  end
  else
    inherited AddObject(S, AObject);
end;
}
destructor TSSymbolList.Destroy;
//var
//  i: Integer;
begin
  //for i := 0 to Count - 1 do
  //  TDynDatum(Objects[i]).Free;
  inherited Destroy;
end;


{ TBigScope }

function TBigScope.GetLocalValue(Symbol: TDynDatum): TDynDatum;
var
  Res: dyn;
begin
  if List.TryGetValue(Symbol, Res) then
    Result := Res
  else
    Result := Unbound
end;

function TBigScope.GetParent: IDynScope;
begin
  Result := FParent;
end;

function TBigScope.GetItem(const Key: dyn): dyn;
var           
  Res: dyn;
begin
  if List.TryGetValue(Key, Res) then
    Result := Res
  else if Assigned(Parent) then
    Result := Parent.Value[Key]
  else
    Result := Unbound
end;

function TBigScope.QueryInterface({$ifdef FPC} constref {$else} const {$endif} IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj)
end;

procedure TBigScope.SetItem(const Key: dyn; const Value: dyn);
begin
  if List.ContainsKey(Key) then
    List[Key] := Value
  else
    List.Add(Key, Value)
end;

function TBigScope.Debug: String;
var
  i: Integer;
  e: TKVPair;
begin
  Result := '(';
  for e in List do
  begin
    Result := Result + Deb(e.Key) + '= ' + Deb(e.Value) + ', ';
  end;
  Result := Result + ')';
end;

constructor TBigScope.Create(AParent: IDynScope = nil);
begin
  FParent := AParent;
  List := TSSymbolList.Create;
end;

destructor TBigScope.Destroy;
begin
  List.Destroy;
  inherited Destroy;
end;

end. ///////////////////////////////////////////////////////////////////////////

