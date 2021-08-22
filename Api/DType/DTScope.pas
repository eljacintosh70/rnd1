unit DTScope;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils, Classes,
  DynTypes, DTDatum, DTPair, DTSymbol;

type
  IDebug = interface(IInterface)
    ['{FA8A83D6-5EF6-4E9C-AF36-D6023A783E49}']
    function Debug: String;
  end;

  TAbstractDynScope = class(TDyn, IDynScope)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynDatum
  public
    function GetParent: IDynScope; virtual; abstract;
    function GetValue(Symbol: TDynDatum): TDynDatum; virtual; abstract;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; virtual; abstract;
    procedure SetValue(Symbol: TDynDatum; const Value: TDynDatum); virtual; abstract;
    property Value[Symbol: TDynDatum]: TDynDatum read GetValue write SetValue;
    property Parent: IDynScope read GetParent;
  private
    function GetAsIDynScope: IDynScope;
  public
    property AsIDynScope: IDynScope read GetAsIDynScope {$if Declared(InlineVMT)} implements IDynScope {$ifend};
  public
    function DatumType: TDatumType; override;
    function AsVariant: Variant; override;
    function GetItem(const Key: dyn): dyn; override; stdcall;
    procedure SetItem(const Key: dyn; const Value: dyn); override; stdcall;
    function Method(Id: dyn; Arg: array of dyn): dyn; override; stdcall;
  end;

  TListScope = class(TAbstractDynScope, IDebug)
  public
    function GetParent: IDynScope; override;
    function GetValue(Symbol: TDynDatum): TDynDatum; override;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; override;
    procedure SetValue(Symbol: TDynDatum; const Value: TDynDatum); override;
  public
    function Debug: String;
    function CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer; override;
    function WriteStr(NeededChars: Integer): String; override;
    function DisplayStr(NeededChars: Integer): String; override;
  protected
    FParent: IDynScope;
    List: IDynPair;
  public
    constructor Create(AParent: IDynScope = nil);
    destructor Destroy; override;
    property Value[Symbol: TDynDatum]: TDynDatum read GetValue write SetValue;
    property Parent: IDynScope read GetParent;
  end;

  TSSymbolList = class(TStringList)
  public
    destructor Destroy; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
  end;

  TBigScope = class(TAbstractDynScope, IDebug)
  public
    function GetParent: IDynScope; override;
    function GetValue(Symbol: TDynDatum): TDynDatum; override;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum; override;
    procedure SetValue(Symbol: TDynDatum; const Value: TDynDatum); override;
  protected
    FParent: IDynScope;
  public
    function Debug: String;
    function CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer; override;
    function WriteStr(NeededChars: Integer): String; override;
  protected
    List: TSSymbolList;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function DisplayStr(NeededChars: Integer): String; override;
  public
    constructor Create(AParent: IDynScope = nil);
    destructor Destroy; override;
    property Value[Symbol: TDynDatum]: TDynDatum read GetValue write SetValue;
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

function TAbstractDynScope.AsVariant: Variant;
begin
  Result := DisplayStr(200);
end;

function TAbstractDynScope.GetItem(const Key: dyn): dyn;
begin
  Result := GetValue(Key)
end;

procedure TAbstractDynScope.SetItem(const Key, Value: dyn);
begin
  SetValue(Key, Value);
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

function TListScope.GetValue(Symbol: TDynDatum): TDynDatum;
begin
  Result := Assq(Symbol, List);
  if Assigned(Result) then
    Result := car(cdr(Result))
  else if Assigned(Parent) then
    Result := Parent.Value[Symbol]
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

procedure TListScope.SetValue(Symbol: TDynDatum; const Value: TDynDatum);
var
  Pair: IDynPair;
begin
  Pair := make_list([Symbol, Value]);
  List := DynTypes.cons(Pair, List)
end;

function TListScope.Debug: String;
begin
  Result := Deb(List)
end;

function TListScope.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('%s:%p', [ClassName, Pointer(Self)]);
end;

function TListScope.WriteStr(NeededChars: Integer): String;
begin
  Result := DisplayStr(NeededChars);
end;

function TListScope.CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer;
begin
  Result := E_NOTIMPL;
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

destructor TSSymbolList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TDynDatum(Objects[i]).Free;
  inherited Destroy;
end;


{ TBigScope }

function TBigScope.GetLocalValue(Symbol: TDynDatum): TDynDatum;
var
  Name: string;
  i: Integer;
begin
  Name := string(SymbolName(Symbol));
  if List.Find(Name, i) then
    Result := TDynDatum(List.Objects[i])
  else
    Result := Unbound
end;

function TBigScope.GetParent: IDynScope;
begin
  Result := FParent;
end;

function TBigScope.GetValue(Symbol: TDynDatum): TDynDatum;
var
  Name: string;
  i: Integer;
begin
  Name := string(SymbolName(Symbol));
  if List.Find(Name, i) then
    Result := TDynDatum(List.Objects[i])
  {else if Assigned(Parent) then
    Result := Parent.Value[Symbol] }
  else
    Result := Unbound
end;

function TBigScope.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj)
end;

procedure TBigScope.SetValue(Symbol: TDynDatum; const Value: TDynDatum);
var
  Name: string;
begin
  Name := string(SymbolName(Symbol));
  List.AddObject(Name, Value)
end;

function TBigScope.Debug: String;
var
  i: Integer;
begin
  Result := '(';
  for i := 0 to List.Count - 1 do
  begin
    Result := Result + List[i] + '= ' +
      String(TDynDatum(List.Objects[i]).AsVariant) + ', ';
  end;
  Result := Result + ')';
end;

function TBigScope.DisplayStr(NeededChars: Integer): String;
begin
  Result := Format('%s:%p', [ClassName, Pointer(Self)]);
end;

function TBigScope.WriteStr(NeededChars: Integer): String;
begin
  Result := DisplayStr(NeededChars);
end;

function TBigScope.CommandExec(Command: Integer; Res: Pointer; Data: Pointer = nil): Integer;
begin
  Result := E_NOTIMPL;
end;

constructor TBigScope.Create(AParent: IDynScope = nil);
begin
  FParent := AParent;
  List := TSSymbolList.Create;
  List.Sorted := True;
  List.CaseSensitive := False; // True segun r6rs
end;

destructor TBigScope.Destroy;
begin
  List.Destroy;
  inherited Destroy;
end;

end. ///////////////////////////////////////////////////////////////////////////

