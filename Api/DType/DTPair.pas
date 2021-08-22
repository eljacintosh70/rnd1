unit DTPair;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes, DTDatum, DUtils;

type
  TCustomDynSeq = class(TDyn, IDynSeq)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynSeq
  public
    function Rest: IDynSeq; virtual;
    function First: TDynDatum; virtual;
    function HasData: Boolean; virtual;
  private
    function GetAsIDynSeq: IDynSeq;
  public
    property AsIDynSeq: IDynSeq read GetAsIDynSeq {$if Declared(InlineVMT)} implements IDynSeq {$ifend};
  end;

  TDynPair = class(TCustomDynSeq, IDynPair)
  // InlineVMT requiere los siguientes métodos idénticos a los de IDynPair
  public
    function Getcar: TDynDatum; virtual;
    procedure Setcar(const First: TDynDatum); virtual;
    property car: TDynDatum read Getcar write Setcar;
    function Getcdr: TDynDatum; virtual;
    procedure Setcdr(const First: TDynDatum); virtual;
    property cdr: TDynDatum read Getcdr write Setcdr;
  private
    function GetAsISchPair: IDynPair;
  public
    property AsISchPair: IDynPair read GetAsISchPair {$if Declared(InlineVMT)} implements IDynPair {$ifend};
  public
    function _Release: Integer; override; stdcall;
    function GetEnumerator: IDynEnumerator; override; stdcall;
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function Rest: IDynSeq; override;
    function First: TDynDatum; override;
    function Length: TArraySize;
  public
    FCar, FCdr: TDatumRef;
  public
    constructor Create(ACar, ACdr: dyn);
    destructor Destroy; override;
  protected
    procedure DoMsgDisplay(var Msg: TWriteMsg); message MsgDisplay;
  end;

  TPairEnumerator = class(TCustomEnumerator)
  private
    Obj: dyn;
    FCurrent: dyn;
  public
    function MoveNext: Boolean; override; stdcall;
    function GetCurrent: dyn; override; stdcall;
  public
    constructor Create(AObj: dyn);
  end;

  TDynSeqHelper = TCustomDynSeq deprecated 'use: TCustomDynSeq';
  TPairHelper = TDynPair deprecated 'use: TDynPair';

function Assq(const Key: dyn; List: IDynPair): TDynDatum;
// (append list ... obj)
function list_append(Lists: array of IDynPair; Obj: IDynDatum): IDynPair;
// usadas para facilitar recursividad...
function CdrRef(Pair: TDynDatum): PDatumRef;
function ListStr2(Datum: TDynDatum; Indent: string): string;

implementation /////////////////////////////////////////////////////////////////

uses
  Windows;

function CdrRef(Pair: TDynDatum): PDatumRef;
var
  P: TDynPair;
begin
  P := Pointer(Integer(Pair) and PointerMask);
  Result := @P.FCdr;
end;

function DatumAsTree(Datum: TDynDatum; const Indent: string): string;
begin
  case Datum.Kind of
    atPair:
      Result := ListStr2(Datum, Indent);
    else
      Result := Datum.AsVariant;
      Result := Indent + Result;
  end
end;

function ListStr2(Datum: TDynDatum; Indent: string): string;
var
  p: IDynPair;
  s: string;
  First: Boolean;
begin
  Result := Indent + '( ';
  Indent := Indent + '  ';
  First := True;
  while Datum.Kind = atPair do
  begin
    p := IDynPair(Pointer(Datum));
    if First then
    begin
      First := False;
      s := p.car.AsVariant;
      Result := Result + s;
    end
    else
    begin
      s := DatumAsTree(p.car, Indent);
      Result := Result + #13#10 + s;
    end;
    Datum := p.cdr;
  end;
  if Datum.Kind <> atNil then
  begin
    s := Datum.AsVariant;
    Result := Result  + ' . ' + s;
  end;
  Result := Result + ')';
end;

function Assq(const Key: dyn; List: IDynPair): TDynDatum;
var
  Item: TDynDatum;
  cdr: TDynDatum;
begin
  while Assigned(List) do
  begin
    Item := List.car;
    if Item.Kind = atPair then
      if (Key = car(Item)) then
      begin
        Result := Item;
        Exit;
      end;
    cdr := List.cdr;
    NeedPair(cdr, List);
  end;
  Result := nil;
end;

procedure Prepend(var Dest: IDynPair; const List: IDynPair);
begin
  if Assigned(List) then
  begin
    Prepend(Dest, IDynPair(Pointer(cdr(List))));
    Dest := cons(car(List), Dest);
  end;
end;

// (append list ... obj)
function list_append(Lists: array of IDynPair; Obj: IDynDatum): IDynPair;
var
  i: Integer;
begin
  Result := IDynPair(Pointer(Obj));
  for i := High(Lists) downto Low(Lists) do
    Prepend(Result, Lists[i]);
end;

{ TCustomDynSeq }

function TCustomDynSeq.Rest: IDynSeq;
begin
  Result := nil;
end;

function TCustomDynSeq.First: TDynDatum;
begin
  Result := nil;
end;

function TCustomDynSeq.HasData: Boolean;
begin
  Result := True
end;

function TCustomDynSeq.GetAsIDynSeq: IDynSeq;
begin
  {$if Declared(InlineVMT)} Result := IDynSeq(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

{ TDynPair }

function TDynPair.Getcar: TDynDatum;
begin
  Result := FCar.Value
end;

procedure TDynPair.Setcar(const First: TDynDatum);
begin
  FCar.Assign(First)
end;

function TDynPair.Getcdr: TDynDatum;
begin
  Result := FCdr.Value
end;

procedure TDynPair.Setcdr(const First: TDynDatum);
begin
  FCdr.Assign(First)
end;

function TDynPair.GetAsISchPair: IDynPair;
begin
  {$if Declared(InlineVMT)} Result := IDynPair(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynPair._Release: Integer;
asm
  mov  eax,Self
  add  eax,4
  push eax
  call InterlockedDecrement // Result := InterlockedDecrement(FRefCount);
  or   eax,eax              // if Result = 0 then
  jz   @@Destroy2           //   Destroy2;
  pop  ebp
  ret  4
@@Destroy2:
  mov  edx,Self
  mov  eax,TDynPair(edx).FCar
  test eax,1
  jnz  @@NoCarFree           // smInteger = 1; smInline = 3;
  call TDynDatum.Free
  mov  edx,Self
@@NoCarFree:
  mov  eax,edx //Self
  mov  edx,TDynPair(edx).FCdr
  mov  Self,edx
  call System.@FreeMem
  pop  ebp
  pop  eax       // continuation
  xchg eax,[esp] // FCdr
  jmp  TDynDatum.Free
end;

function TDynPair.GetEnumerator: IDynEnumerator;
begin
  Result := TPairEnumerator.Create(Self)
end;

function TDynPair.DatumType: TDatumType;
begin
  Result := atPair;
end;

function TDynPair.DisplayStr(NeededChars: Integer): String;
var
  s: string;
  Datum: TDynDatum;
begin
  Result := '(';
  Dec(NeededChars, System.Length(Result));

  Datum := Pointer(Self);
  while IsPair(Datum) do
  begin
    s := DynTypes.car(Datum).DisplayStr(NeededChars);
    Result := Result + s + ' ';
    Dec(NeededChars, System.Length(s) + 1);
    if NeededChars <= 0 then
      Exit;
    Datum := DynTypes.cdr(Datum);
  end;
  if not IsNull(Datum) then
  begin
    s := Datum.DisplayStr(NeededChars);
    Result := Result  + '. ' + s;
  end;
  Result := Result + ')';
end;

function TDynPair.Rest: IDynSeq;
begin
  Result := IDynSeq(Pointer(FCdr.Value))
end;

function TDynPair.First: TDynDatum;
begin
  Result := FCar.Value;
end;

function TDynPair.Length: TArraySize;
begin
  Result := 0; // no funciona si no existe el objeto
  repeat
    Inc(Result);
    Self := Pointer(FCdr);
  until (Self = nil);
end;

constructor TDynPair.Create(ACar, ACdr: dyn);
begin
  FCar.Init(TDynDatum(Pointer(ACar)));
  FCdr.Init(TDynDatum(Pointer(ACdr)));
end;

destructor TDynPair.Destroy;
begin
  FCar.Free;
  FCdr.Free;
  inherited Destroy;
end;

procedure TDynPair.DoMsgDisplay(var Msg: TWriteMsg);
var
  r: Boolean;
  List: IDynOutPort;
  L: dyn;
begin
  List := Msg.Port.BeginList(lkList);
  r := List.Write(Car);
  if r then
  begin
    L := Cdr;
    while L <> nil do
    begin
      r := List.WriteSepar;
      r := r and List.Write(DynTypes.car(L));
      if not r then
        Break;
      L := DynTypes.cdr(L);
    end;
  end;
  Msg.Res := Ord(r);
end;

{ TPairEnumerator }

function TPairEnumerator.MoveNext: Boolean;
begin
  if Obj <> nil then
  begin
    FCurrent := car(Obj);
    Obj := cdr(Obj);
    Result := True;
  end
  else
    Result := False;
end;

function TPairEnumerator.GetCurrent: dyn;
begin
  Result := FCurrent
end;

constructor TPairEnumerator.Create(AObj: dyn);
begin
  Obj := Aobj;
end;

end. ///////////////////////////////////////////////////////////////////////////




