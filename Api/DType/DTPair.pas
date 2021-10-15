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
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
    function GetEnumerator: IDynEnumerator; override; stdcall;
    function DatumType: TDatumType; override;
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
    procedure DoMsgEval(var Msg: TEvalMessage); message MsgEval;
    procedure DoMsgEvalItems(var Msg: TEvalMessage); message MsgEvalItems;

    procedure DoMsgIsPair(var Msg: TVarMessage); message MsgIsPair;
    procedure DoMsgIsPairR(var Msg: TVarMessage); message MsgIsPairR;
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

function Assq(const Key: dyn; List: IDynPair): TDynDatum;
// (append list ... obj)
function list_append(Lists: array of IDynPair; Obj: IDynDatum): IDynPair;
// usadas para facilitar recursividad...
function CdrRef(Pair: TDynDatum): PDatumRef;

implementation /////////////////////////////////////////////////////////////////

{$IFNDEF LINUX}
uses
  Windows;
{$ENDIF}

var
  DebStr: string;

function CdrRef(Pair: TDynDatum): PDatumRef;
var
  P: TDynPair;
begin
  P := Pointer(Integer(Pair) and PointerMask);
  Result := @P.FCdr;
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
  Result := FCar
end;

procedure TDynPair.Setcar(const First: TDynDatum);
begin
  FCar := (First)
end;

function TDynPair.Getcdr: TDynDatum;
begin
  Result := FCdr
end;

procedure TDynPair.Setcdr(const First: TDynDatum);
begin
  FCdr := (First)
end;

function TDynPair.GetAsISchPair: IDynPair;
begin
  {$if Declared(InlineVMT)} Result := IDynPair(Pointer(Self));
  {$else}                   Result := Self;
  {$endif}
end;

function TDynPair._Release: Integer;
{$IFDEF LINUX}
begin
  Result := inherited _Release;
{$ELSE}
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
{$ENDIF}
end;

function TDynPair.GetEnumerator: IDynEnumerator;
var
  Enum: TPairEnumerator;
begin
  Enum := TPairEnumerator.Create(Self.AsISchPair);
  Result := Enum.AsIDynEnumerator;
end;

function TDynPair.DatumType: TDatumType;
begin
  Result := atPair;
end;

function TDynPair.Rest: IDynSeq;
begin
  Result := IDynSeq(FCdr.Ptr)
end;

function TDynPair.First: TDynDatum;
begin
  Result := FCar;
end;

function TDynPair.Length: TArraySize;
begin
  Result := 0; // no funciona si no existe el objeto
  repeat
    Inc(Result);
    Self := FCdr.Ptr;
  until (Self = nil);
end;

constructor TDynPair.Create(ACar, ACdr: dyn);
begin
  FCar := ACar;
  FCdr := ACdr;
end;

destructor TDynPair.Destroy;
begin
  FCar := nil;
  FCdr := nil;
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
  List := nil;
end;

procedure TDynPair.DoMsgEval(var Msg: TEvalMessage);
var
  f: dyn;
  Msg2: TEvalCallMessage;
begin
  DebStr := Deb(car);
  f := Eval(car, Msg.Scope);

  Msg2.Msg := MsgEvalCall;
  Msg2.Scope := Msg.Scope;
  Msg2.Params := cdr;
  f.DispatchMsg(Msg2);
  Msg.Res := Msg2.Res;
end;

procedure TDynPair.DoMsgEvalItems(var Msg: TEvalMessage);
var
  e, v: dyn;
  L1: IDynPair;
begin
  for e in Self do
  begin
    v := Eval(e, Msg.Scope);
    L1 := cons(v, L1);
  end;
  Msg.Res := Reverse(L1);
end;

procedure TDynPair.DoMsgIsPair(var Msg: TVarMessage);
begin
  Msg.Res := 1; //True;
end;

procedure TDynPair.DoMsgIsPairR(var Msg: TVarMessage);
type
  PIDynPair = ^IDynPair;
begin
  Msg.Res := 1; //True;
  PIDynPair(Msg.VarPtr)^ := IDynPair(Self);
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




