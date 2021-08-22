unit AutoDestroy;
interface //////////////////////////////////////////////////////////////////////

const
  MaxLocalItems = 3;

type
  TAutoDestroyHelper = class
  protected
    // el orden debe ser el mismo que en IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall; abstract;
    function _AddRef: Integer; virtual; stdcall; abstract;
    function _Release: Integer; virtual; stdcall; abstract;
    class procedure DestroyItem(Item: Pointer); virtual; abstract;
    class function DefValue: Pointer; virtual; abstract;
  end;
  TAutoDestroyHelperClass = class of TAutoDestroyHelper;

  TAutoDestroyNoRefHelper = class(TAutoDestroyHelper)
  private
    ItemCount: Integer;
    LocalItems: array[0..MaxLocalItems - 1] of Pointer;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;
  end;

  TAutoDestroyRecord = object
    Ref: IInterface;
    VMT: TAutoDestroyHelperClass;
    // los campos de TAutoDestroyNoRefHelper en orden...
    ItemCount: Integer;
    LocalItems: array[0..MaxLocalItems - 1] of Pointer;
    procedure Init(AVMT: TAutoDestroyHelperClass; const Items: array of Pointer);
  end;

type
  TDatumRefHelper = class(TAutoDestroyNoRefHelper)
  protected
    class procedure DestroyItem(Item: Pointer); override;
    class function DefValue: Pointer; override;
  end;

implementation /////////////////////////////////////////////////////////////////

uses
  DynTypes;

{ TAutoDestroyNoRefHelper }

function TAutoDestroyNoRefHelper._AddRef: Integer;
begin
  Result := 1;
end;

function TAutoDestroyNoRefHelper._Release: Integer;
var
  i, n: Integer;
  DynItems, pItem: PPointer;
begin
  n := ItemCount;
  if n <= MaxLocalItems then
  begin
    for i := 0 to n - 1 do
      DestroyItem(LocalItems[i]);
  end
  else
  begin
    // LocalItems[MaxLocalItems - 1] = DynItems
    for i := 0 to MaxLocalItems - 2 do
      DestroyItem(LocalItems[i]);
    DynItems := LocalItems[MaxLocalItems - 1];
    pItem := DynItems;
    for i := MaxLocalItems - 1 to n - 1 do
    begin
      DestroyItem(pItem^);
      Inc(pItem);
    end;
    FreeMem(DynItems);
  end;
  Result := 0;
end;

function TAutoDestroyNoRefHelper.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

{ TAutoDestroyRecord }

procedure TAutoDestroyRecord.Init(AVMT: TAutoDestroyHelperClass;
  const Items: array of Pointer);
var
  i, n: Integer;
  DynItems: PPointer;
  Item: PPointer;
  DefValue: Pointer;
begin
  Ref := nil; // en caso de reasignar la variable
  Pointer(Ref) := @VMT;
  VMT := AVMT;
  DefValue := VMT.DefValue;
  n := Length(Items);
  ItemCount := n;
  if n <= MaxLocalItems then
  begin
    for i := 0 to n - 1 do
    begin
      Item := Items[i];
      Item^ := DefValue;
      LocalItems[i] := Item;
    end;
  end
  else
  begin
    for i := 0 to MaxLocalItems - 2 do
    begin
      Item := Items[i];
      Item^ := DefValue;
      LocalItems[i] := Item;
    end;
    GetMem(DynItems, (n - (MaxLocalItems - 1)) * SizeOf(Pointer));
    LocalItems[MaxLocalItems - 1] := DynItems;
    for i := MaxLocalItems - 1 to n - 1 do
    begin
      Item := Items[i];
      Item^ := DefValue;
      DynItems^ := Item;
      Inc(DynItems);
    end;
  end;
end;

{ TDatumRefHelper }

class function TDatumRefHelper.DefValue: Pointer;
begin
  Result := Unbound;
end;

class procedure TDatumRefHelper.DestroyItem(Item: Pointer);
begin
  PDatumRef(Item).Free;
end;

end. ///////////////////////////////////////////////////////////////////////////


