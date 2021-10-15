unit DTScript;

interface

uses
  SysUtils,
  DynTypes, DTPair, DTProc, DTScope;

type
  IScriptObjectEx = interface(IScriptObject)
    ['{5AB1A80D-AEF2-482B-A732-B2EF15FC9E28}']
    function LocalList(Id: TDynDatum): TDynDatum;
    procedure AddNestedList(Id, Value: TDynDatum);
  end;

  IAddToList = interface(IInterface)
    ['{E5CD98EA-0054-4AD4-9213-51D5121CA62A}']
    procedure Add(Item: TDynDatum);
  end;

  TNestedList = class(TDynMethod, IAddToList)
  public
    Parent: IDynMethod;
    List: IDynPair;
    procedure Call(out Result: TDatumRef; const this: IDynScope; Params: TDynDatum); override;
    procedure Add(Item: TDynDatum);
  end;

  TScriptObject = class(TListScope, IScriptObject, IScriptObjectEx)
  protected
    FPublic: IDynPair;
    //FSymbols: IDynPair;
  public
    procedure SetItem(const Key: dyn; const Value: dyn); override;
    function DisplayStr(NeededChars: Integer): String; override;
    //procedure CalcSymbols;
  public
    constructor Create(const AParent: IDynScope; const PropNames: array of TDynDatum;
      const PropValues: array of const);
    function Symbols: IDynPair;
    procedure AddNestedList(Id, Value: TDynDatum);
    function LocalList(Id: TDynDatum): TDynDatum;
    function CreateLocalList(Id: TDynDatum): TDynDatum;
  end;

var
  SyType: dyn;

implementation

{ TScriptObject }

procedure TScriptObject.AddNestedList(Id, Value: TDynDatum);
var
  List: TDynDatum;
  Add: IAddToList;
begin
  List := LocalList(Id);
  if Supports(List, IAddToList, Add) then
    Add.Add(Value);
end;

function TScriptObject.LocalList(Id: TDynDatum): TDynDatum;
begin
  //DebStr := Format('%p.%s %s', [Pointer(Self), Deb(Id), Deb(List, 200)]);
  Result := Assq(Id, List);
  if Assigned(Result) then
    Result := car(cdr(Result))
  else
    Result := CreateLocalList(Id);
end;

function TScriptObject.CreateLocalList(Id: TDynDatum): TDynDatum;
var
  NL: TNestedList;
  //ParentList: TDynDatum;
  Ex: IScriptObjectEx;
begin
  //DebStr := Format('%p.%s %s', [Pointer(Self), Deb(Id), Deb(List, 200)]);
  NL := TNestedList.Create;
  if Assigned(FParent) then
    if Supports(FParent, IScriptObjectEx, Ex) then
      Supports(Ex.LocalList(Id), IDynMethod, NL.Parent);
  Result := Pointer(NL.AsIDynMethod);
  Value[Id] := Result;
end;

constructor TScriptObject.Create(const AParent: IDynScope;
  const PropNames: array of TDynDatum; const PropValues: array of const);
// const PropNames: array of Utf8String; const PropValues: array of const);
var
  i: Integer;
  //Name: Utf8String;
  Key, Val: TDynDatum;
begin
  inherited Create(AParent);
  // si no está ordenada, no encontrará los símbolos
  //List.Sorted := False;
  FPublic := nil;
  for i := 0 to High(PropNames) do
  begin
    //Name := PropNames[i];
    //Key := InitSymbol(PAnsiChar(Name), Length(Name));
    Key := PropNames[i];
    FPublic := DynTypes.cons(Key, FPublic);
    Val := ConstToDatum(PropValues[i]);
    Value[Key] := Val;
  end;
end;

function TScriptObject.DisplayStr(NeededChars: Integer): String;
var
  v: TDynDatum;
  SymbolList, Id, Pair: TDynDatum;
  SymbolsR: IDynPair;
begin
  v := Value[syType];
  Result := '{';
  if v <> Unbound then
    Result := Result + v.WriteStr(NeededChars) + ' ';

  SymbolsR := Reverse(FPublic);
  SymbolList := Pointer(SymbolsR);

  while (SymbolList <> nil) do
  begin
    Id := car(SymbolList);
    SymbolList := cdr(SymbolList);

    if Id <> SyType then
    begin
      Pair := Assq(Id, List);
      Result := Result + Pair.WriteStr(NeededChars - Length(Result));
    end;
  end;

  if Length(Result) < NeededChars then
    Result := Result + '}';
end;

procedure TScriptObject.SetItem(const Key: dyn; const Value: dyn);
var
  Pair: IDynPair;
begin
  Pair := make_list([Key.Value, Value.Value]);
  List := DynTypes.cons(Pair, List);
  FPublic := DynTypes.cons(Key, FPublic);
end;

function TScriptObject.Symbols: IDynPair;
begin
  Result := List;//FSymbols;
end;

{ TNestedList }

procedure TNestedList.Add(Item: TDynDatum);
begin
  List := DynTypes.cons(Item, List);
end;

procedure TNestedList.Call(out Result: TDatumRef; const this: IDynScope;
  Params: TDynDatum);
begin
  //DebStr := Format('%p(%p)(%p) %s ', [Pointer(Self), Pointer(Parent), Pointer(this), Deb(List, 200)]) + Deb(this, 200);
  if Assigned(Parent) then
    Parent.Call(Result, this, Params)
  else
    Result := (nil);
  if Assigned(List) then
    Result := (list_append([List], IDynDatum(Pointer(Result.Value))))
end;

initialization
  SyType := InitSymbol('type', 4);
end.
